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
module Wid = Why3.Ident
module Mid = Why3.Ident.Mid

(* -------------------------------------------------------------------------- *)
(* ---   Utility Functions                                                --- *)
(* -------------------------------------------------------------------------- *)

(* exported in the API *)
let pr_name : Why3.Ident.ident -> string =
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
let task_name t = pr_name (T.task_goal t).pr_name
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

type theory = Thy of { file : S.file ; session : S.session ; theory : S.theory }

let name (Thy t) = (S.theory_name t.theory).id_string
let theory (Thy t) = Th.restore_theory (S.theory_name t.theory)

let theories (Session { file ; session }) : theory list =
  List.map (fun theory -> Thy { file ; theory ; session }) (S.file_theories file)

(* -------------------------------------------------------------------------- *)
(* ---  Goal Naming                                                       --- *)
(* -------------------------------------------------------------------------- *)

(* 0: hyp-name, 1: expl *)
let set_attr_label lbl (a : Wid.attribute) ~prefix k =
  if lbl.(k) = "" && String.starts_with ~prefix a.attr_string then
    let p = String.length prefix in
    let n = String.length a.attr_string in
    lbl.(k) <- String.sub a.attr_string p (n-p)

let set_attr_labels lbl a =
  begin
    set_attr_label lbl a ~prefix:"hyp_name:" 0 ;
    set_attr_label lbl a ~prefix:"expl:" 1 ;
  end

let set_attributes lbl attrs =
  if lbl.(0) = "" || lbl.(1) = "" then
    Wid.Sattr.iter (set_attr_labels lbl) attrs

let (^^) u v =
  if u = "" then v else if v = "" then u else Printf.sprintf "%s.%s" u v

let rec term_head lbl flag (t : Why3.Term.term) =
  let flag = flag && not (Wid.Sattr.mem Why3.Term.stop_split t.t_attrs) in
  (if flag then set_attributes lbl t.t_attrs) ;
  match t.t_node with
  | Tvar _ | Tconst _ | Ttrue -> ""
  | Tfalse -> "absurd"
  | Tapp (f, ts) ->
    if Id.is_infix f.ls_name.id_string then
      List.fold_right (fun t w -> term_head lbl false t ^ w) ts ""
    else f.ls_name.id_string
  | Tif(e,_,_) -> "if:" ^ term_head lbl flag e
  | Tcase(e,_) -> "case:" ^ term_head lbl flag e
  | Tnot p | Tbinop(Timplies,_,p) -> term_head lbl flag p
  | Tbinop((Tand|Tor|Tiff), a, b) -> term_head lbl flag a ^^ term_head lbl flag b
  | Tlet (_, b) | Teps b ->
    let _,e = Why3.Term.t_open_bound b in term_head lbl flag e
  | Tquant (_, b) ->
    let _,_,p = Why3.Term.t_open_quant b in term_head lbl flag p

type fullname = { name : string ; expl : string ; head : string }

let task_fullname (tsk : Why3.Task.task) =
  match tsk with
  | Some { task_decl = { td_node = (Decl { d_node = Dprop(Pgoal, g, p)}) } } ->
    let lbl = [| "" ; "" |] in
    let head = term_head lbl true p in
    set_attributes lbl g.pr_name.id_attrs ;
    let expl = lbl.(1) in
    let name = if lbl.(0) = "" then pr_name g.pr_name else lbl.(0) in
    { name ; expl ; head }
  | _ ->
    { name = "<goal>" ; expl = "" ; head = "" }

(* -------------------------------------------------------------------------- *)
(* ---  Goals                                                             --- *)
(* -------------------------------------------------------------------------- *)

type goal = Goal of {
    file : S.file ;
    theory : S.theory ;
    session : S.session ;
    node : S.proofNodeID ;
    task : T.task ;
    mutable idename : string ;
    mutable fullname : string ;
  }

let split (Thy { file ; session ; theory }) : goal list =
  List.map
    (fun node ->
       let task = S.get_task session node in
       Goal { file ; session ; theory ; node ; task ; idename = "" ; fullname = "" }
    ) @@ S.theory_goals theory

let split = Timer.timed ~name:"Split theories" split

let goal_task (Goal g) = g.task
let goal_theory (Goal g) = Th.restore_theory (S.theory_name g.theory)
let goal_loc g = (T.task_goal (goal_task g)).pr_name.id_loc
let goal_file (Goal g) =
  match (T.task_goal g.task).pr_name.id_loc with
  | Some loc -> let file,_,_,_,_ = Why3.Loc.get loc in file
  | None -> String.concat Filename.dir_sep @@ Why3.Sysutil.decompose_path @@ S.file_path g.file
let goal_name g = task_name @@ goal_task g
let compute_names (Goal g) =
  if g.idename = "" then
    begin
      let d = task_fullname g.task in
      let buffer = Buffer.create 80 in
      Buffer.add_string buffer d.name ;
      if d.expl <> "" then Printf.bprintf buffer " [%s]" d.expl ;
      g.idename <- Buffer.contents buffer ;
      if d.head <> "" then Printf.bprintf buffer " <%s>" d.head ;
      g.fullname <- Buffer.contents buffer ;
    end

let goal_idename (Goal g as goal) = compute_names goal ; g.idename
let goal_fullname (Goal g as goal) = compute_names goal ; g.fullname

let pp_goal fmt g = Format.pp_print_string fmt (goal_idename g)

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
    (* update proof attempt on success or when there is no successfull attempt yet *)
    if is_valid result || pa = [] then
      let _id = S.graft_proof_attempt session node prv ~limits in
      S.update_proof_attempt silent session node prv result ;
  end ;
  S.update_goal_node silent session node

let apply env tactic ?(allow_no_effect=false)
    (Goal { file ; session ; theory ; node }) =
  try
    let tr = Tactic.name tactic in
    let ts = S.apply_trans_to_goal session env ~allow_no_effect tr [] node in
    List.iter
      (S.remove_transformation session)
      (S.get_transformations session node) ;
    let tid = S.graft_transf session node tr [] ts in
    let nodes = S.get_sub_tasks session tid in
    if ts = [] then S.update_trans_node silent session tid ;
    let child node =
      let task = S.get_task session node in
      Goal { file ; session ; theory ; node ; task ; fullname = "" ; idename = "" }
    in Some (List.map child nodes)
  with S.NoProgress | Why3.Trans.TransFailure _ -> None

let apply =
  let timer = Timer.create_global "Transformation" in
  fun env tactic ?allow_no_effect goal ->
    Timer.start timer;
    Fun.protect ~finally:(fun () -> Timer.stop timer)
      (fun () -> apply env tactic ?allow_no_effect goal)

(* -------------------------------------------------------------------------- *)
