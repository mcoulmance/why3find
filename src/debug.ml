(**************************************************************************)
(*                                                                        *)
(*  This file is part of the why3find.                                    *)
(*                                                                        *)
(*  Copyright (C) 2022-2024                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the enclosed LICENSE.md for details.                              *)
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
      Format.printf ("@[<hov>" ^^ format ^^ "@]@.")
    end
  else Format.ifprintf Format.std_formatter format
