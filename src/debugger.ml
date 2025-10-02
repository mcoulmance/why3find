(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let init () =
  begin
    Dap.on_launch
      (fun adapter ->
         Dap.output adapter
           "Launched nodebug:@{<red>%b@} restart:@{<green>%a@}@."
           (Dap.no_debug ()) Json.pretty (Dap.restarted ()) ;
         let a = Dap.thread adapter ~name:"Thread A" () in
         let b = Dap.thread adapter ~name:"Thread B" () in
         let c = Dap.thread adapter ~name:"Thread C" () in
         let connect th = Dap.connect th
             ~on_pause:(fun _ -> Dap.stopped ~reason:`pause th)
             ~on_next:(fun _ -> Dap.stopped ~reason:`step th)
             () in
         connect a ; connect b ; connect c ;
      ) ;
    Dap.on_terminate
      (fun adapter ~restart ->
         Dap.output adapter
           "Terminated restart:@{<red>%b@}@." restart ;
         if not restart then Dap.terminated adapter ()
      ) ;
    Dap.on_disconnect
      (fun adapter _ -> Dap.output adapter "Disconnected@.") ;
  end
