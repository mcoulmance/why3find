(**************************************************************************)
(*                                                                        *)
(*  This file is part of the why3find.                                    *)
(*                                                                        *)
(*  Copyright (C) 2022-2024                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the enclosed LICENSE.md for details.                              *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Session Management                                                 --- *)
(* -------------------------------------------------------------------------- *)

module Th = Why3.Theory
module T = Why3.Task
module S = Why3.Session_itp
module Mid = Why3.Ident.Mid

(* -------------------------------------------------------------------------- *)
(* ---   Utility Functions                                                --- *)
(* -------------------------------------------------------------------------- *)

(* exported in the API *)
let proof_name : Why3.Ident.ident -> string =
  let names = ref Mid.empty in
  fun id ->
    try Mid.find id !names with Not_found ->
      let _,_,qid = Id.path id in
      let path = Id.cat qid in
      let name =
        if String.ends_with ~suffix:"'vc" path
        then String.sub path 0 (String.length path - 3) else path
      in names := Mid.add id name !names ; name

let thy_name th = th.Th.th_name.id_string
let task_name t =  proof_name (T.task_goal t).pr_name
let task_expl t =
  let (_, expl, _) = Why3.Termcode.goal_expl_task ~root:false t in expl

(* -------------------------------------------------------------------------- *)
(* ---  Sessions (File)                                                   --- *)
(* -------------------------------------------------------------------------- *)

type session = Session of { file : S.file ; session : S.session }

let create ~dir ~file ~format ths =
  let session = S.empty_session dir in
  let file = S.add_file_section session
      (Why3.Sysutil.relativize_filename dir file)
      ~file_is_detached:false ths format
  in Session { file ; session }

let save = function Session { session } ->
  Utils.mkdirs @@ S.get_dir session ;
  S.save_session session

(* -------------------------------------------------------------------------- *)
(* ---  Theories                                                          --- *)
(* -------------------------------------------------------------------------- *)

type theory = Thy of { session : S.session ; theory : S.theory }

let name (Thy t) = (S.theory_name t.theory).id_string
let theory (Thy t) = Th.restore_theory (S.theory_name t.theory)

let theories (Session { file ; session }) : theory list =
  List.map (fun theory -> Thy { theory ; session }) (S.file_theories file)

(* -------------------------------------------------------------------------- *)
(* ---  Goals                                                             --- *)
(* -------------------------------------------------------------------------- *)

type goal = Goal of {
    session : S.session ;
    node : S.proofNodeID ;
    task : T.task ;
    idename :  string ;
  }

let split (Thy { session ; theory }) : goal list =
  List.map
    (fun node ->
       let task = S.get_task session node in
       let idename = task_name task in
       Goal { session ; node ; task ; idename }
    ) @@ S.theory_goals theory

let split = Timer.timed ~name:"Split theories" split

let goal_task (Goal g) = g.task
let goal_idename (Goal g) = g.idename
let goal_loc g = (T.task_goal (goal_task g)).pr_name.id_loc
let goal_name g = task_name @@ goal_task g
let goal_expl g = task_expl @@ goal_task g

let pp_goal fmt g =
  Format.pp_print_string fmt (goal_name g) ;
  let expl = goal_expl g in
  if expl <> "" then Format.fprintf fmt " [%s]" expl

let silent : S.notifier = fun _ -> ()

let is_valid (pr : Why3.Call_provers.prover_result) = pr.pr_answer = Valid
let is_valid_attempt (pa : S.proof_attempt_node) =
  match pa.proof_state with Some pr -> is_valid pr | None -> false

let result (Goal { session ; node }) prv limits result =
  let pa,na =
    List.partition is_valid_attempt @@ S.get_proof_attempts session node in
  begin
    (* cleanup previously failed attempts on success *)
    if is_valid result then
      List.iter (fun p -> S.remove_proof_attempt session node p.S.prover) na ;
  end ;
  begin
    (* filter out failed result if node is already proved *)
    if is_valid result || pa = [] then
      let _id = S.graft_proof_attempt session node prv ~limits in
      S.update_proof_attempt silent session node prv result ;
  end ;
  S.update_goal_node silent session node

let apply env tactic (Goal { session ; node ; idename }) =
  begin
    try
      let allow_no_effect = false in
      let tr = Tactic.name tactic in
      let ts = S.apply_trans_to_goal session env ~allow_no_effect tr [] node in
      let tid = S.graft_transf session node tr [] ts in
      let nodes = S.get_sub_tasks session tid in
      if ts = [] then S.update_trans_node silent session tid ;
      let child i node =
        let task = S.get_task session node in
        let idename = Printf.sprintf "%s.%d" idename i in
        Goal { session ; node ; task ; idename }
      in Some (List.mapi child nodes)
    with _ -> None
  end

let apply = Timer.timed3 ~name:"Transformation" apply

(* -------------------------------------------------------------------------- *)
