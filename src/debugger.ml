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

let debug_proof adapter hammer goal hint =
  ignore adapter ;
  let node = Hammer.schedule hammer goal hint in
  let result crc = Prove.merge_stats hint crc ; Crc.merge hint crc in
  Fibers.map result (Hammer.result node)

let debug_theory adapter hammer buffer strategy gnames theory =
  let module M = Why3.Wstdlib.Mstr in
  let thy = Session.name theory in
  let goals = Session.split theory in
  let hints = M.find_def M.empty thy strategy in
  Proofs.init theory goals hints buffer ;
  let accept g =
    gnames = [] || List.mem thy gnames ||
    List.mem (thy ^ "." ^ (Session.goal_name g)) gnames
  in
  Fibers.all @@
  List.map
    (fun goal ->
       if not @@ accept goal then Fibers.return () else
         let gname = Session.goal_name goal in
         Log.eprintf "Prove %s.%s@." thy gname ;
         let hint = try M.find gname hints with Not_found -> Crc.stuck () in
         Fibers.map
           begin fun crc ->
             Proofs.add theory goal crc buffer ;
             if not @@ Crc.is_complete crc then
               begin
                 exit := 1 ;
                 Log.error "Goal %s.%s @{<red>failed@} %a" thy gname Crc.pretty crc ;
               end
           end @@
         debug_proof adapter hammer goal hint
    ) goals

let debug_file adapter env gnames file =
  try
    let dir, _ = Wutil.filepath file in
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
        Log.printf "File %S:@\n%t%t" file Runner.print_stats
        @@ Prove.print_stats (Hammer.provers hammer) (Hammer.tactics hammer) ;
      end in
    Fibers.finally ~callback @@ Fibers.all @@
    List.map
      (debug_theory adapter hammer buffer strategy gnames)
      (Session.theories session)
  with Why3.Loc.Located(loc,exn) ->
    let file,line,_,_,_ = Why3.Loc.get loc in
    Dap.output adapter ~category:`important
      "@{<red>Error:@} %s:%d:@\n%s@."
      file line @@ Printexc.to_string exn ;
    Fibers.return []

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
    Dap.on_launch
      (fun adapter ->
         Dap.output adapter "Why3 Debugger Started.@." ;
         let echo category pp = Dap.output adapter ~category "%t" pp in
         Log.pp_stdout := echo `stdout ;
         Log.pp_stderr := echo `stderr ;
         if files = [] then Log.warning "No files to debug." else
           match List.find_map Config.root_for files with
           | None -> Log.warning "No config file found."
           | Some root ->
             let config = Config.load_config ~root in
             let env = Project.create ~root ~config () in
             let callback _ =
               Dap.output adapter "Why3 Debugger Exited (%t)." pp_exit ;
               Dap.exited adapter !exit in
             Fibers.background ~callback @@ Fibers.all @@
             List.map (debug_file adapter env gnames) files
      ) ;
    Dap.on_terminate
      (fun adapter ~restart ->
         ignore restart ;
         Dap.output adapter "Why3 Debugger Terminated.@." ;
      ) ;
  end

let running adapter =
  Dap.running adapter || Hammer.running ()

let process adapter =
  let h = Hammer.process () in
  let d = Dap.process adapter in
  h || d

let run adapter =
  while running adapter do
    if not @@ process adapter then Unix.sleepf 0.01 ;
  done ;
  Client.terminate ()

(* -------------------------------------------------------------------------- *)
