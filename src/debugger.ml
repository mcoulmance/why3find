(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let init files =
  begin
    Dap.on_launch
      (fun adapter ->
         Dap.output adapter "Why3 Debugger Started.@." ;
         let echo category pp = Dap.output adapter ~category "%t" pp in
         Log.pp_stdout := echo `stdout ;
         Log.pp_stderr := echo `stderr ;
         List.iter (Log.message "Compile %S@.") files ;
      ) ;
    Dap.on_terminate
      (fun adapter ~restart ->
         ignore restart ;
         Dap.output adapter "Why3 Debugger Terminated.@." ;
      ) ;
  end
