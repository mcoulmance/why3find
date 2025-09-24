(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Logging Facilities                                                 --- *)
(* -------------------------------------------------------------------------- *)

let is_log_on = ref false
let warnings = ref 0
let errors = ref 0
let summary = Queue.create ()

type level = [ `Message | `Warning | `Error ]

let pp_error pp = pp Format.err_formatter
let summarize pp = Queue.push pp summary ; pp_error pp

let output level pp =
  Utils.flush ();
  match level with
  | `Message -> Format.printf "@[<hov>%t@]@." pp
  | `Warning ->
    incr warnings ;
    Format.kdprintf summarize
      "@[<hov>@{<bold>@{<bright magenta>Warning:@}@} %t@]@." pp
  | `Error ->
    incr errors ;
    Format.kdprintf summarize
      "@[<hov>@{<bold>@{<bright red>Error:@}@} %t@]@." pp

let set_log b =
  is_log_on := b

let emit ?(level=`Message) msg =
  if !is_log_on
  then Format.kdprintf (output level) msg
  else Format.ifprintf Format.std_formatter msg

let message format =
  emit ~level:`Message format

let warning format =
  emit ~level:`Warning format

let error format =
  emit ~level:`Error format

let summary () =
  if !is_log_on then
    let wrn = !warnings in
    if wrn > 0 then
      begin
        Utils.flush ();
        if not Utils.tty || Utils.overflows () then
          begin
            Format.printf "Summary:@.";
            Queue.iter pp_error summary ;
          end ;
        Format.printf "Emitted %d warning%a" wrn Utils.pp_s wrn ;
        let err = !errors in
        if err > 0 then
          Format.printf ", %d error%a" err Utils.pp_s err ;
        Format.printf "@."
      end

let exit_summary () =
  try summary () with _ -> ()

let () =
  at_exit exit_summary
