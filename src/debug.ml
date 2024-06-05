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

type label = {
  name : string; [@ocaml.warning "-69"]
  mutable active : bool;
}

module M = Why3.Wstdlib.Mstr

let all = ref M.empty

let create_label name =
  if name = "why3" || String.starts_with ~prefix:"why3:" name then
    raise (Invalid_argument "Debug.create_label");
  let label = { name ; active = false } in
  let exn = Invalid_argument "Debug.create_label: label already exists" in
  all := M.add_new exn name label !all;
  label

let default = create_label "default"

let parse_one s =
  try
    if String.starts_with ~prefix:"why3:" s then
      let f = Why3.Debug.lookup_flag (String.sub s 5 (String.length s - 5)) in
      Why3.Debug.set_flag f
    else if s = "why3" then (* why3's --debug-all *)
      let l = Why3.Debug.list_flags () in
      List.iter (fun (_, f, b, _) -> if b then Why3.Debug.set_flag f) l
    else
      let lbl = M.find s !all in
      lbl.active <- true
  with
  | _ (* Why3.Debug.UnknownFlag is not exported !! *)
    (*| Not_found *) ->
    let msg = Format.sprintf "unknown debug label %S" s in
    raise (Stdlib.Arg.Bad msg)

let parse_and_set_labels s =
  Printexc.record_backtrace true;
  default.active <- true;
  List.iter parse_one (String.split_on_char ',' s)

let option =
  ("--debug", Stdlib.Arg.String parse_and_set_labels,
   "LABEL Activate the given debug label (eg default or why3:whyconf)")

let debug ?(label=default) format =
  if label.active then
    begin
      Utils.flush ();
      Format.printf ("@[<hov>" ^^ format ^^ "@]@.")
    end
  else Format.ifprintf Format.std_formatter format
