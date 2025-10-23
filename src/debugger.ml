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

let dloc (p : Why3.Loc.position) : Dap.location =
  let source,l1,c1,l2,c2 = Why3.Loc.get p in
  Dap.loc ~source ~line:(l1-1) ~column:c1 ~end_line:(l2-1) ~end_column:c2 ()

let dloc_of_goal g = Option.map dloc @@ Session.goal_loc g
let rec dloc_of_node n =
  match dloc_of_goal @@ Hammer.goal n with
  | Some _ as r -> r
  | None ->
    match Hammer.parent n with
    | Thread _ -> None
    | Node n -> dloc_of_node n

(* -------------------------------------------------------------------------- *)
(* ---  Observing Frames                                                  --- *)
(* -------------------------------------------------------------------------- *)

let root = Dap.attribute ()

type frame = {
  frame : Dap.frame ;
  failed : Dap.variable Darray.buffer ;
  paused : Dap.variable Darray.buffer ;
}

let new_frame ~name ?range ?subtle () =
  let failed = Darray.create () in
  let paused = Darray.create () in
  let s_failed = Dap.scope ~name:"Failed" ~named:(Darray.contents failed) () in
  let s_paused = Dap.scope ~name:"Paused" ~named:(Darray.contents paused) () in
  let frame = Dap.frame ~name ?range ?subtle ~scopes:[s_failed;s_paused] () in
  { frame ; failed ; paused }

let rec variables stack frame n =
  match Hammer.state n with
  | Paused ->
    let g = Hammer.goal n in
    Darray.add frame.paused @@ Dap.variable ~name:(Session.goal_name g) ~value:"Paused" ()
  | Failed _ ->
    let g = Hammer.goal n in
    Darray.add frame.failed @@ Dap.variable ~name:(Session.goal_name g) ~value:"Failed" ()
  | Stacked(tactic,children) ->
    let range = dloc_of_node n in
    let frame = new_frame ?range ~name:(Tactic.name tactic) () in
    List.iter (variables stack frame) children ;
    Darray.add stack frame.frame
  | _ -> ()

let frames thread =
  let stk = Darray.create () in
  let root = Dap.get_opt root thread in
  let range = Option.bind root dloc_of_node in
  let frame = new_frame ?range ~subtle:true ~name:"root" () in
  Option.iter (variables stk frame) root ;
  Darray.add stk frame.frame ;
  Darray.contents stk

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
  state := Running { progress }

let exited () =
  match !state with
  | Paused -> ()
  | Running { progress } ->
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
  let thread = Dap.thread adapter ~on_continue ~on_pause ~name ~frames () in
  let node = Hammer.thread hammer ~id:(Dap.id thread) goal hint in
  Dap.set root thread node ;
  let callback crc =
    Dap.terminate thread ;
    Log.message "%s %a" name Crc.pretty crc ;
    Prove.merge_stats hint crc
  in Fibers.finally ~callback (Hammer.result node)

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
