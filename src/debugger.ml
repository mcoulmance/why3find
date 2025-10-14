(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* ---  Compilation                                                       --- *)
(* -------------------------------------------------------------------------- *)

let exit = ref 0
let progress = ref Dap.idle

let cc_proof adapter hammer theory goal hint =
  let tname = Session.thy_name @@ Session.theory theory in
  let gname = Session.goal_name goal in
  let name = Printf.sprintf "Goal %s.%s" tname gname in
  let thread = Dap.thread adapter ~name () in
  let node = Hammer.schedule hammer goal hint in
  let result crc =
    Log.message "%s %a" name Crc.pretty crc ;
    if Crc.is_complete crc then Dap.terminated_thread thread ;
    Prove.merge_stats hint crc ; Crc.merge hint crc
  in Fibers.map result (Hammer.result node)

let pp_goal_location fmt goal =
  match Session.goal_loc goal with
  | None -> ()
  | Some loc ->
    let file,line,_,_,_ = Why3.Loc.get loc in
    Format.fprintf fmt "%s:%d: " file line

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
             Proofs.add theory goal crc buffer ;
             if not @@ Crc.is_complete crc then
               begin
                 exit := 1 ;
                 Log.error "%aGoal %s.%s @{<red>failed@} %a"
                   pp_goal_location goal
                   thy gname Crc.pretty crc ;
               end
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
    Format.pp_print_string fmt "Success"
  else
    Format.fprintf fmt "Exit %d" !exit

let init gnames files =
  begin
    Dap.on_init
      begin fun adapter ->
        progress := Dap.progress adapter ~title:"Provers" () ;
        let echo category pp = Dap.output adapter ~category "%t" pp in
        Log.pp_stdout := echo `stdout ;
        Log.pp_stderr := echo `stderr ;
        Hammer.interrupt () ;
        if files = [] then Log.warning "No files to debug." else
          match List.find_map Config.root_for files with
          | None ->
            Log.warning "No config file found." ;
            Dap.exited adapter 1
          | Some root ->
            let config = Config.load_config ~root in
            let env = Project.create ~root ~config () in
            let main = Dap.thread adapter ~name:"Provers" () in
            let callback _ =
              begin
                Dap.terminated_thread main ;
                Dap.output adapter "Why3 Debugger Exited (%t)" pp_exit ;
                Dap.exited adapter !exit ;
                Dap.finished !progress ~message:"Finished" () ;
                progress := Dap.idle ;
              end
            in
            Fibers.background ~callback @@ Fibers.all @@
            List.map (cc_file adapter env gnames) files ;
            Dap.initialized adapter
      end ;
    Dap.on_launch
      begin fun adapter ->
        Dap.output adapter "Why3 Debugger Started.@." ;
        progress := Dap.progress adapter ~title:"Provers" () ;
        Hammer.resume () ;
      end ;
    Dap.on_terminate
      begin fun adapter ~restart ->
        ignore restart ;
        Dap.output adapter "Why3 Debugger Terminated.@" ;
      end ;
  end

let running adapter =
  Dap.running adapter || Hammer.running ()

let process adapter =
  let h = Hammer.process () in
  let d = Dap.process adapter in
  h || d

let status =
  Timer.throttle ~delay:0.25
    begin fun p ->
      let message =
        Format.asprintf "%a%t" Hammer.pp_progress p Runner.pp_goals in
      Dap.update !progress ~message ()
    end

let paused () =
  begin
    Dap.finished !progress ~message:"paused" () ;
    progress := Dap.idle ;
  end

let run adapter =
  while running adapter do
    if not @@ process adapter then
      begin
        Unix.sleepf 0.01 ;
        match Hammer.status () with
        | Proving p -> status p
        | Paused -> paused ()
        | Exited -> ()
      end
  done ;
  Client.terminate ()

(* -------------------------------------------------------------------------- *)
