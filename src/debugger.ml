(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* ---  Observing Nodes                                                   --- *)
(* -------------------------------------------------------------------------- *)

let pp_goal_location fmt goal =
  match Session.goal_loc goal with
  | None -> ()
  | Some loc ->
    let file,line,_,_,_ = Why3.Loc.get loc in
    Format.fprintf fmt "%s:%d: " file line

let dloc_of_position (p : Why3.Loc.position) : Dap.location =
  let source,l1,c1,l2,c2 = Why3.Loc.get p in
  Dap.loc ~source ~line:(l1-1) ~column:c1 ~end_line:(l2-1) ~end_column:c2 ()

let dloc_of_range ~file ((p,q) : Range.range) : Dap.location =
  Dap.loc ~source:file
    ~line:p.line ~column:p.col ~end_line:q.line ~end_column:q.col ()

let dloc_of_goal g = Option.map dloc_of_position @@ Session.goal_loc g

let dloc_of_node n =
  let goal = Hammer.goal n in
  let file = Session.goal_file goal in
  let window = Hammer.window n in
  let loc =
    List.find_map
      (function Vc.{ kind = Goal ; range } -> Some range | _ -> None)
      window.chunks in
  match loc with
  | Some range -> Some (dloc_of_range ~file range)
  | None ->
    if 0 <= window.g1 && window.g1 <= window.g2 && window.g2 < max_int then
      Some (Dap.loc ~source:file ~line:window.g1 ~end_line:window.g2 ())
    else
      dloc_of_goal goal

(* -------------------------------------------------------------------------- *)
(* ---  Proof Tree Scope                                                  --- *)
(* -------------------------------------------------------------------------- *)

let rec is_stuck n =
  match Hammer.state n with
  | Paused | Scheduled -> false
  | Failed -> true
  | Closed crc -> Crc.is_stuck crc
  | Stacked(_,children) -> List.for_all is_stuck children

let add buffer ~name ~value ?vdecl ?index () =
  Darray.add buffer @@
  Dap.variable ~name ~value ?vdecl ?index ()

let add_vc buffer ~name ~file (chunk : Vc.chunk) =
  let value = Format.asprintf "@[<hov 2>%a@]"
      Why3.Pretty.print_term chunk.term in
  let vdecl = dloc_of_range ~file chunk.Vc.range in
  add buffer ~name ~value ~vdecl ()

let node_status n =
  match Hammer.state n with
  | Failed -> "Failed"
  | Paused | Scheduled -> "Paused"
  | Closed crc -> Format.asprintf "%a" Crc.pp_crc crc
  | Stacked(tactic,children) ->
    Format.asprintf "[%a] %a"
      Tactic.pretty tactic
      Crc.pp_crc_all (List.map Hammer.proof children)

let node_index n =
  match Hammer.state n with
  | Paused | Scheduled | Failed | Closed _ -> None
  | Stacked(_,children) ->
    if List.for_all is_stuck children then None else Some children

let rec vc_vars ~goal ?(status=goal) n =
  let buffer = Darray.create () in
  let vc = Hammer.window n in
  let file = vc.file in
  List.iter
    (fun chunk ->
       match chunk.Vc.kind with
       | Goal when goal -> add_vc buffer ~file ~name:"Goal" chunk
       | When when not goal -> add_vc buffer ~file ~name:"Hyp" chunk
       | WhenNot when not goal -> add_vc buffer ~file ~name:"Not" chunk
       | _ -> ()
    ) vc.chunks ;
  begin
    if status then
      let value = node_status n in
      let index = Option.map (Darray.delayed index_vars) (node_index n) in
      add buffer ~name:"Status" ~value ?index () ;
  end ;
  Darray.contents buffer

and index_vars children =
  Darray.list @@
  List.mapi
    (fun i n ->
       Dap.variable
         ~name:(string_of_int i)
         ~value:(node_status n)
         ~named:(Darray.delayed (vc_vars ~status:false ~goal:true) n)
         ()
    ) children

let vc_scope ~goal n =
  Dap.scope
    ~name:(if goal then "Proof" else "Context")
    ~expensive:true
    ~named:(Darray.delayed (vc_vars ~goal) n) ()

(* -------------------------------------------------------------------------- *)
(* ---  Callstack                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec reach stack n =
  match Hammer.state n with
  | Closed _ -> None
  | Paused | Scheduled | Failed -> Some (n::stack)
  | Stacked(_, children) ->
    if List.for_all is_stuck children then Some (n::stack) else
      List.find_map (reach (n::stack)) children

let frame n =
  let range = dloc_of_node n in
  let name =
    match Hammer.state n with
    | Stacked(tactic,_) -> Tactic.name tactic
    | _ -> Session.goal_expl @@ Hammer.goal n in
  Dap.frame ~name ~scopes:[
    vc_scope ~goal:false n ;
    vc_scope ~goal:true n ;
  ] ?range ()

let callstack n =
  match reach [] n with
  | None -> [frame n]
  | Some stk -> List.map frame stk

(* -------------------------------------------------------------------------- *)
(* ---  Observing Thread                                                  --- *)
(* -------------------------------------------------------------------------- *)

let root = Dap.attribute ()

let frames thread =
  Darray.list @@
  match Dap.get_opt root thread with
  | None -> []
  | Some n -> callstack n

(* -------------------------------------------------------------------------- *)
(* ---  Running State                                                     --- *)
(* -------------------------------------------------------------------------- *)

let exit = ref 0

type state =
  | Paused
  | Running of { progress : Dap.progress }

let state = ref Paused
let stopping = ref None

let update = Timer.throttle
    begin fun h ->
      match !state with
      | Paused -> ()
      | Running { progress } ->
        let message =
          Format.asprintf "%a%t" Hammer.pp_progress h Runner.pp_goals in
        Dap.update progress ~message ()
    end

let started adapter =
  let progress = Dap.progress adapter ~title:"Provers" () in
  Log.message "Debugger Started" ;
  state := Running { progress }

let exited () =
  match !state with
  | Paused -> ()
  | Running { progress } ->
    Log.message "Debugger Exited" ;
    state := Paused ;
    Dap.finished progress ~message:"exit" ()

let stopped s =
  stopping := None ;
  match s with
  | Some(thread,reason) when Dap.living thread ->
    Dap.stopped ~reason thread
  | _ ->
    match Dap.pending () with
    | main::_ ->
      Dap.stopped ~reason:`pause main
    | _ -> ()

let paused () =
  match !state with
  | Paused -> ()
  | Running { progress } ->
    Log.message "Debugger Paused" ;
    state := Paused ;
    Dap.finished progress ~message:"paused" () ;
    stopped !stopping

let observe () =
  match Hammer.status () with
  | Proving h -> update h
  | Exited -> exited ()
  | Paused -> paused ()

let on_pause thread =
  stopping := Some(thread,`pause) ;
  Hammer.pause ()

let on_continue thread =
  stopping := Some(thread,`pause) ;
  started (Dap.on thread) ;
  Hammer.resume ()

let rec step_in n =
  match Hammer.state n with
  | Closed _ -> None
  | Paused | Scheduled | Failed -> Some n
  | Stacked(_,children) -> List.find_map step_in children

let rec step_out n =
  match Hammer.state n with
  | Closed _ -> None
  | Paused | Scheduled | Failed -> None
  | Stacked(_,children) ->
    match List.find_map step_out children with
    | None -> Some children
    | Some _ as result -> result

let on_step_in thread =
  match Dap.get_opt root thread with
  | None -> on_continue thread
  | Some n ->
    match step_in n with
    | None -> on_continue thread
    | Some n ->
      stopping := Some(thread,`step) ;
      started (Dap.on thread) ;
      Hammer.step_in [n]

let on_step_out thread =
  match Dap.get_opt root thread with
  | None -> on_continue thread
  | Some n ->
    match step_out n with
    | None -> on_step_in thread
    | Some children ->
      stopping := Some(thread,`step) ;
      started (Dap.on thread) ;
      Hammer.step_out children

let on_failed node =
  begin
    exit := 1 ;
    let goal = Hammer.goal node in
    try
      let id = Hammer.thread_id node in
      let thread = Dap.find id in
      let name = Dap.name thread in
      stopping := Some (thread,`error) ;
      Hammer.pause () ;
      Log.error "%a%s @{<red>failed@}" pp_goal_location goal name
    with Not_found ->
      Log.error "%aGoal @{<red>failed@}" pp_goal_location goal
  end

(* -------------------------------------------------------------------------- *)
(* ---  Compilation                                                       --- *)
(* -------------------------------------------------------------------------- *)

let cc_proof adapter hammer theory goal hint =
  let thy = Session.theory theory in
  let tname = Session.thy_name thy in
  let gname = Session.goal_name goal in
  let name = Printf.sprintf "Goal %s.%s" tname gname in
  let frames = Darray.delayed frames in
  let thread = Dap.thread adapter
      ~on_continue ~on_pause ~on_step_in ~on_step_out ~name ~frames () in
  let result = Fibers.var () in
  let callback crc =
    begin
      Dap.terminate thread ;
      Log.message "%s %a" name Crc.pretty crc ;
      Prove.merge_stats hint crc ;
      Fibers.set result crc ;
    end in
  let node = Hammer.thread hammer ~id:(Dap.id thread) ~callback goal hint in
  Dap.set root thread node ; result

let cc_theory adapter hammer buffer strategy gnames theory =
  let module M = Why3.Wstdlib.Mstr in
  let thy = Session.name theory in
  let goals = Session.split theory in
  let hints = M.find_def M.empty thy strategy in
  Proofs.init theory goals hints buffer ;
  let accept g =
    gnames = [] || List.mem thy gnames ||
    List.mem (thy ^ "." ^ (Session.goal_name g)) gnames in
  Fibers.map (fun _ -> Log.message "Finished.") @@
  Fibers.all @@
  List.map
    (fun goal ->
       if not @@ accept goal then Fibers.return () else
         let gname = Session.goal_name goal in
         let hint = try M.find gname hints with Not_found -> Crc.stuck () in
         Fibers.map
           begin fun crc ->
             let complete = not @@ Hammer.interrupting () in
             let hint = Crc.update ~complete ~hint crc in
             Proofs.add theory goal hint buffer
           end @@
         cc_proof adapter hammer theory goal hint
    ) goals

let hline = "--------------------------------------------------------"

let cc_file adapter env gnames file =
  try
    Dap.output adapter "Compiling %s@." file ;
    let dir, lib = Wutil.filepath file in
    let theories, format = Wutil.load_theories env.Project.why3.env file in
    let session = Session.create ~dir ~file ~format theories in
    let local = Calibration.default env in
    let profile, strategy = Proofs.load ~local file in
    Prove.clear_stats () ;
    Runner.clear_stats () ;
    let buffer = Proofs.create ~file profile strategy in
    let hammer = Hammer.create ~profile env in
    let callback _ =
      begin
        Proofs.save buffer ;
        Log.printf "%s@\nLibrary %s:@\n%t%t%s@."
          hline (String.concat "." lib)
          Runner.print_stats
          (Prove.print_stats (Hammer.provers hammer) (Hammer.tactics hammer))
          hline ;
      end in
    Fibers.map callback @@ Fibers.all @@
    List.map
      (cc_theory adapter hammer buffer strategy gnames)
      (Session.theories session)
  with Why3.Loc.Located(loc,exn) ->
    let file,line,_,_,_ = Why3.Loc.get loc in
    Dap.output adapter ~category:`important
      "@{<red>Error:@} %s:%d:@\n%a@."
      file line Why3.Exn_printer.exn_printer exn ;
    Fibers.return ()

(* -------------------------------------------------------------------------- *)
(* ---  Debugger Initialization                                           --- *)
(* -------------------------------------------------------------------------- *)

let pp_exit fmt =
  if !exit = 0 then
    Format.pp_print_string fmt "Terminated"
  else
    Format.fprintf fmt "Failed (Status %d)" !exit

let init gnames files =
  begin
    Dap.on_init
      begin fun adapter ->
        let echo category pp = Dap.output adapter ~category "%t" pp in
        Log.pp_stdout := echo `stdout ;
        Log.pp_stderr := echo `stderr ;
        Hammer.pause () ;
        if files = [] then Log.warning "No files to debug." else
          match List.find_map Config.root_for files with
          | None ->
            Log.warning "No config file found." ;
            Dap.exited adapter 1
          | Some root ->
            let config = Config.load_config ~root in
            let env = Project.create ~root ~config () in
            let main =
              Dap.thread adapter ~name:"Monitor" ~on_pause ~on_continue () in
            let callback _ =
              begin
                Dap.terminate main ;
                Dap.output adapter "Why3 Debugger %t@." pp_exit ;
                Dap.exited adapter !exit ;
              end
            in
            Fibers.background ~callback @@ Fibers.all @@
            List.map (cc_file adapter env gnames) files ;
            Dap.initialized adapter
      end ;
    Dap.on_launch
      begin fun adapter ->
        Dap.output adapter "Why3 Debugger Started.@." ;
        started adapter ;
        Hammer.resume () ;
      end ;
    Dap.on_terminate
      begin fun adapter ~restart ->
        ignore adapter ;
        ignore restart ;
        Hammer.interrupt () ;
      end ;
    Fibers.on Hammer.failed on_failed ;
  end

let running adapter =
  Dap.running adapter || Hammer.running ()

let process adapter =
  let h = Hammer.process () in
  let d = Dap.process adapter in
  h || d

let run adapter =
  while running adapter do
    if not @@ process adapter then
      begin
        observe () ;
        Unix.sleepf 0.01 ;
      end
  done ;
  Client.terminate ()

(* -------------------------------------------------------------------------- *)
