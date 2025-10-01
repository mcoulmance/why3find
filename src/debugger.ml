(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let init () =
  begin
    let restarted = ref 0 in
    Dap.on_launch
      (fun adapter ~nodebug ~restart ->
         Dap.output adapter
           "Launched nodebug:@{<red>%b@} restart:@{<green>%a@}@."
           nodebug Json.pretty restart ;
         restarted := Json.jint restart ;
      ) ;
    Dap.on_terminate
      (fun adapter ~restart ->
         incr restarted ;
         Dap.output adapter
           "Terminated restart:@{<red>%b@} @{<green>#%d@}@."
           restart !restarted ;
         if not restart then Dap.terminated adapter ()
      ) ;
    Dap.on_disconnect
      (fun adapter _ -> Dap.output adapter "Disconnected@.") ;
  end
