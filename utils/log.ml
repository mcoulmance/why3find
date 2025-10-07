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

let pp_stdout = ref (fun pp -> pp Format.std_formatter)
let pp_stderr = ref (fun pp -> pp Format.err_formatter)

let summarize_err pp = Queue.push pp summary ; !pp_stderr pp

let output level pp =
  Utils.flush ();
  match level with
  | `Message ->
    Format.kdprintf !pp_stdout "@[<hov>%t@]@." pp
  | `Warning ->
    incr warnings ;
    Format.kdprintf summarize_err
      "@[<hov>@{<bold>@{<bright magenta>Warning:@}@} %t@]@." pp
  | `Error ->
    incr errors ;
    Format.kdprintf summarize_err
      "@[<hov>@{<bold>@{<bright red>Error:@}@} %t@]@." pp

let set_log b =
  is_log_on := b

let emit ?(level=`Message) msg =
  if !is_log_on
  then Format.kdprintf (output level) msg
  else Format.ifprintf Format.std_formatter msg

let printf msg = Format.kdprintf !pp_stdout msg
let eprintf msg = Format.kdprintf !pp_stderr msg
let message msg = emit ~level:`Message msg
let warning msg = emit ~level:`Warning msg
let error   msg = emit ~level:`Error   msg

let summary () =
  if !is_log_on then
    let wrn = !warnings in
    if wrn > 0 then
      begin
        Utils.flush ();
        if not Utils.tty || Utils.overflows () then
          begin
            eprintf "Summary:@.";
            Queue.iter !pp_stderr summary ;
          end ;
        printf "Emitted %d warning%a" wrn Utils.pp_s wrn ;
        let err = !errors in
        if err > 0 then
          printf ", %d error%a" err Utils.pp_s err ;
        printf "@."
      end

let exit_summary () =
  try summary () with _ -> ()

let () =
  at_exit exit_summary
