(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Session Management                                                 --- *)
(* -------------------------------------------------------------------------- *)

module Th = Why3.Theory
module T = Why3.Task
module S = Why3.Session_itp

(* -------------------------------------------------------------------------- *)
(* ---  Sessions (File)                                                   --- *)
(* -------------------------------------------------------------------------- *)

type session = Session of { file : S.file ; session : S.session }

let create ~dir ~file ~format ts =
  let session = S.empty_session dir in
  let file = S.add_file_section session
      (Why3.Sysutil.relativize_filename dir file)
      ~file_is_detached:false ts format
  in Session { file ; session }

let save = function Session { session } ->
  Utils.mkdirs @@ S.get_dir session ;
  S.save_session session

(* -------------------------------------------------------------------------- *)
(* ---  Theories                                                          --- *)
(* -------------------------------------------------------------------------- *)

type theory = Thy of { file : S.file ; session : S.session ; theory : S.theory }

let thy (Thy t) = Th.restore_theory (S.theory_name t.theory)
let thy_name (Thy t) = (S.theory_name t.theory).id_string

let theories (Session { file ; session }) : theory list =
  List.map (fun theory -> Thy { file ; theory ; session }) (S.file_theories file)

(* -------------------------------------------------------------------------- *)
(* ---  Goals                                                             --- *)
(* -------------------------------------------------------------------------- *)

type goal = Goal of {
    file : S.file ;
    theory : S.theory ;
    session : S.session ;
    node : S.proofNodeID ;
    task : T.task ;
    parent : goal option ;
    mutable tactic : ( Tactic.tactic * goal list ) option ;
    mutable hashname : string ;
    mutable fullname : string ;
    mutable context : Vc.context option ;
    mutable proof : Crc.crc option ;
  }

let split (Thy { file ; session ; theory }) : goal list =
  List.map
    (fun node ->
       let task = S.get_task session node in
       Goal {
         file ; session ; theory ; node ; task ;
         parent = None ; tactic = None ;
         hashname = "" ; fullname = "" ; context = None ; proof = None ;
       }
    ) @@ S.theory_goals theory

let split = Timer.timed ~name:"Split theories" split

let task (Goal g) = g.task
let theory (Goal g) = Th.restore_theory (S.theory_name g.theory)
let name g = Vc.task_name @@ task g

let file (Goal g) =
  match (T.task_goal g.task).pr_name.id_loc with
  | Some loc -> let file,_,_,_,_ = Why3.Loc.get loc in file
  | None -> String.concat Filename.dir_sep @@ Why3.Sysutil.decompose_path @@ S.file_path g.file

let target (Goal g) = Vc.target g.task

let compute_names (Goal g) =
  if g.hashname = "" then
    begin
      let d = Vc.task_descr g.task in
      let buffer = Buffer.create 80 in
      Buffer.add_string buffer d.name ;
      if d.expl <> "" then Printf.bprintf buffer " [%s]" d.expl ;
      g.fullname <- Buffer.contents buffer ;
      if d.head <> "" then Printf.bprintf buffer " <%s>" d.head ;
      g.hashname <- Buffer.contents buffer ;
    end

let hashname (Goal g as goal) = compute_names goal ; g.hashname
let fullname (Goal g as goal) = compute_names goal ; g.fullname
let proof (Goal g) = g.proof

let pp_status fmt (Goal g) =
  match g.proof with
  | Some crc -> Crc.pp_proof fmt crc
  | None ->
    match g.tactic with
    | None -> Format.fprintf fmt "No proof"
    | Some(tactic, goals) ->
      let child (Goal g) = Option.value ~default:Crc.stuck g.proof in
      Format.fprintf fmt "Tactic %a, %a" Tactic.pretty tactic
        Crc.pp_proofs (List.map child goals)

let pp_goal fmt g = Format.pp_print_string fmt (fullname g)

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
      begin
        List.iter (fun p -> S.remove_proof_attempt session node p.S.prover) na ;
        List.iter
          (S.remove_transformation session)
          (S.get_transformations session node) ;
      end
  end ;
  begin
    (* update proof attempt on success *)
    if is_valid result || pa = [] then
      let _id = S.graft_proof_attempt session node prv ~limits in
      S.update_proof_attempt silent session node prv result ;
  end ;
  S.update_goal_node silent session node

let apply env tactic ?(allow_no_effect=false) goal =
  try
    let Goal { file ; theory ; session ; node } = goal in
    let tr = Tactic.name tactic in
    let ts = S.apply_trans_to_goal session env ~allow_no_effect tr [] node in
    List.iter
      (S.remove_transformation session)
      (S.get_transformations session node) ;
    let tid = S.graft_transf session node tr [] ts in
    if ts = [] then S.update_trans_node silent session tid ;
    let nodes = S.get_sub_tasks session tid in
    let parent = Some goal in
    let children =
      List.map
        (fun node ->
           let task = S.get_task session node in
           Goal {
             file ; session ; theory ; node ; task ; parent ;
             tactic = None ; fullname = "" ; hashname = "" ;
             context = None ; proof = None ;
           }
        ) nodes in
    let Goal g = goal in
    g.tactic <- Some (tactic, children) ;
    Some children
  with S.NoProgress | Why3.Trans.TransFailure _ -> None

let apply =
  let timer = Timer.create_global "Transformation" in
  fun env tactic ?allow_no_effect goal ->
    Timer.start timer;
    Fun.protect ~finally:(fun () -> Timer.stop timer)
      (fun () -> apply env tactic ?allow_no_effect goal)

let commit (Goal g) crc = g.proof <- Some crc

(* -------------------------------------------------------------------------- *)
(* ---  Context                                                           --- *)
(* -------------------------------------------------------------------------- *)

let context (Goal g as goal) =
  match g.context with Some w -> w | None ->
    let file = file goal in
    let thy = theory goal in
    let task = task goal in
    let context = Vc.context ~file thy task in
    g.context <- Some context ; context

(* -------------------------------------------------------------------------- *)
(* ---  Navigation                                                        --- *)
(* -------------------------------------------------------------------------- *)

let rec pp_path fmt (Goal g as goal) =
  match g.parent with
  | Some (Goal { tactic = Some(_,gs) } as parent) ->
    pp_path fmt parent ;
    List.iteri
      (fun i g0 ->
         if goal == g0 then Format.fprintf fmt ".%d" i
      ) gs
  | _ ->
    pp_goal fmt goal

let parent (Goal g) = g.parent
let tactic (Goal g) = Option.map fst g.tactic
let children (Goal g) = Option.fold ~none:[] ~some:snd g.tactic

let rec prev (Goal g as goal) =
  match g.parent with
  | None -> None
  | Some p ->
    let rec before = function
      | [] -> prev p
      | g::g'::_ when g' == goal -> Some g
      | _::gs -> before gs
    in before @@ children p

let rec next (Goal g as goal) =
  match g.parent with
  | None -> None
  | Some p ->
    let rec after = function
      | [] -> next p
      | g::g'::_ when g == goal -> Some g'
      | _::gs -> after gs
    in after @@ children p

let rec is_parent parent (Goal g) =
  match g.parent with
  | None -> false
  | Some p -> p == parent || is_parent parent p

(* -------------------------------------------------------------------------- *)
(* ---  Hashtbl                                                           --- *)
(* -------------------------------------------------------------------------- *)

type 'a hmap = 'a S.Hpn.t

let hmap = S.Hpn.create
let clear = S.Hpn.clear
let add m (Goal g) = S.Hpn.add m g.node
let remove m (Goal g) = S.Hpn.remove m g.node
let replace m (Goal g) = S.Hpn.replace m g.node
let find m (Goal g) = S.Hpn.find m g.node
let mem m (Goal g) = S.Hpn.mem m g.node
let find_opt m (Goal g) = S.Hpn.find_opt m g.node
let find_all m (Goal g) = S.Hpn.find_all m g.node

(* -------------------------------------------------------------------------- *)
