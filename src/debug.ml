(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type flag = {
  name : string; [@ocaml.warning "-69"]
  callback : unit -> unit;
  mutable active : bool;
}

module M = Why3.Wstdlib.Mstr

let all = ref M.empty

let create_flag ?(callback=ignore) name =
  let flag = { name ; callback; active = false } in
  let exn = Invalid_argument "Debug.create_flag: flag already exists" in
  all := M.add_new exn name flag !all;
  flag

let default =
  create_flag "default" ~callback:(fun () -> Printexc.record_backtrace true)

let parse_one s =
  match M.find_opt s !all with
  | Some f -> f.callback (); f.active <- true
  | None ->
    let msg = Format.sprintf "unknown debug flag %S" s in
    raise (Stdlib.Arg.Bad msg)

let parse_and_set_flags s =
  parse_one "default";
  List.iter parse_one (String.split_on_char ',' s)

let why3_retry_flags = ref []

let parse_why3 ~retry s =
  try
    let f = Why3.Debug.lookup_flag s in
    Why3.Debug.set_flag f
  with
  | _ -> (* Why3 does not exposes Debug.UnknownFlag *)
    if retry then
      why3_retry_flags := s :: !why3_retry_flags
    else
      let msg = Format.sprintf "Unknown why3 debug flag %S" s in
      raise (Stdlib.Arg.Bad msg)

let init_why3_debug () =
  List.iter (parse_why3 ~retry:false) !why3_retry_flags

let why3_debug_flags s =
  List.iter (parse_why3 ~retry:true) (String.split_on_char ',' s)

let options = [
  ("--debug", Stdlib.Arg.String parse_and_set_flags,
   "FLAG Activate the given debug flag (eg default)");
  ("--why3-debug", Stdlib.Arg.String why3_debug_flags,
   "FLAG Activate the given Why3 debug flag");
]

let debug ?(flag=default) format =
  if flag.active then
    begin
      Utils.flush ();
      let out = Format.std_formatter in
      Format.pp_open_hovbox out 0 ;
      Format.kfprintf
        begin fun fmt ->
          Format.pp_close_box fmt () ;
          Format.pp_print_newline fmt () ;
        end out format ;
    end
  else Format.ifprintf Format.std_formatter format
