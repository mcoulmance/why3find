(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Soundness Registry                                                 --- *)
(* -------------------------------------------------------------------------- *)

module Thy = Why3.Theory
module Sinst = Set.Make
    (struct
      open Docref
      type t = instance
      let compare (Instance a) (Instance b) =
        let p = String.concat "." @@ List.rev a.scope in
        let q = String.concat "." @@ List.rev b.scope in
        let cmp = String.compare p q in
        if cmp <> 0 then cmp else a.order - b.order
    end)

type soundness =
  | Unsound
  | Sound of Docref.instance list
  | Unknown of Docref.instance list

let pretty fmt = function
  | Unsound -> Format.pp_print_string fmt "Unsound"
  | Sound ds -> Format.fprintf fmt "Sound (%d)" (List.length ds)
  | Unknown ds -> Format.fprintf fmt "Unknown (%d)" (List.length ds)

(* Everything indexed by fullname for inter-file consistency. *)
type env = {
  theories : (string,Docref.theory) Hashtbl.t ;
  instances : (string,Sinst.t) Hashtbl.t ;
  soundness : (string,soundness) Hashtbl.t ;
}

let init () = {
  theories  = Hashtbl.create 0 ;
  instances = Hashtbl.create 0 ;
  soundness = Hashtbl.create 0 ;
}

let add hinst ~lib (Docref.Instance s as inst) =
  let key = Id.fullname ~lib s.cloned.th_name in
  try
    let clones = Hashtbl.find hinst key in
    Hashtbl.replace hinst key (Sinst.add inst clones)
  with Not_found ->
    Hashtbl.add hinst key (Sinst.singleton inst)

let register henv (src : Docref.source) =
  let lib = src.lib in
  Docref.Mstr.iter
    (fun id (thy : Docref.theory) ->
       let path = Printf.sprintf "%s.%s" (Id.cat lib) id in
       Hashtbl.add henv.theories path thy ;
       Docref.iter_instances (add henv.instances ~lib) thy.instances
    ) src.theories

let free = Sound []
let unknown = Unknown []

let merge a b =
  match a,b with
  | Unsound, _ | _, Unsound -> Unsound
  | Sound xs , Sound ys -> Sound (xs @ ys)
  | Unknown xs , Unknown ys -> Unknown (xs @ ys)
  | (Unknown _ as w), Sound _ | Sound _, (Unknown _ as w) -> w

let is_free = function Sound [] -> true | _ -> false
let is_sound = function Sound _ -> true | Unsound | Unknown _ -> false
let is_unsound = function Unsound -> true | Sound _ | Unknown _ -> false

let rec compute (env : env) (th : Docref.theory) : soundness =
  let path = th.path in
  try Hashtbl.find env.soundness path with Not_found ->
    Hashtbl.add env.soundness path (Unknown []) ;
    let s =
      let ps = Axioms.parameters th.signature in
      if List.exists (fun p -> Axioms.is_unsafe p) ps then Unsound else
        let ok = List.for_all (fun p -> Axioms.is_free p) ps in
        if ok then Sound [] else
          try
            let instances = Sinst.elements (Hashtbl.find env.instances path) in
            let grounds = List.filter (has_ground_instance env) instances in
            if grounds <> [] then Sound grounds else Unknown instances
          with Not_found -> Unknown []
    in Hashtbl.replace env.soundness path s ; s

and has_ground_instance env (Docref.Instance s) =
  try Hashtbl.find env.theories s.container |> compute env |> is_sound
  with Not_found -> false

let instance env (Docref.Instance s) =
  try Hashtbl.find env.soundness @@ Id.fullname s.cloned.th_name
  with Not_found -> free

(* -------------------------------------------------------------------------- *)
