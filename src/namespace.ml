(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* ---  Reversed Name Spaces                                              --- *)
(* -------------------------------------------------------------------------- *)

module Ident = Why3.Ident
module Mstr = Why3.Wstdlib.Mstr

type node = {
  single : Ident.ident option ; (* always None for the root *)
  index : node Mstr.t ;
}

type t = { known : Ident.Sid.t ; node : node }

let rec dump fmt node =
  Format.fprintf fmt "@[<hv 2>{" ;
  Option.iter (Format.fprintf fmt "@ %a" Id.pp) node.single ;
  Mstr.iter (fun a n -> Format.fprintf fmt "@ %s%a" a dump n) node.index ;
  Format.fprintf fmt "@ }@]"

let pp fmt m = dump fmt m.node

let root = { single = None ; index = Mstr.empty }
let empty = { known = Ident.Sid.empty ; node = root }

let from ~lib id =
  let l,m,q = Id.path ~lib id in
  List.rev_append q (m :: List.rev l)

let rec populate self from d =
  let single, index =
    match d with
    | None -> self, Mstr.empty
    | Some d -> None, d.index in
  match from with
  | [] -> { single ; index }
  | a::from ->
    let d = populate self from @@ Mstr.find_opt a index in
    { single ; index = Mstr.add a d index }

let add ~lib id d =
  if not @@ Ident.Sid.mem id d.known then
    {
      known = Ident.Sid.add id d.known ;
      node = populate (Some id) (from ~lib id) (Some d.node) ;
    }
  else d

let rec lookup id path from d =
  match d.single with
  | Some a -> if Ident.id_equal a id then path else raise Not_found
  | None ->
    match from with
    | [] -> raise Not_found
    | a::from -> lookup id (a::path) from @@ Mstr.find a d.index

let find ~lib id d =
  let path = from ~lib id in
  try lookup id [] path d.node
  with Not_found -> List.rev path

let add_type ~lib (ts : Why3.Ty.tysymbol) m = add ~lib ts.ts_name m
let add_data ~lib (d : Why3.Decl.data_decl) m = add_type ~lib (fst d) m

let add_types ~lib (th : Why3.Theory.theory) m =
  let pool = ref m in
  Why3.Ident.Mid.iter
    (fun _ (d : Why3.Decl.decl) ->
       match d.d_node with
       | Dtype t -> pool := add_type ~lib t !pool
       | Ddata ds -> pool := List.fold_right (add_data ~lib) ds !pool
       | Dparam _ | Dlogic _ | Dind _ | Dprop _ -> ()
    ) th.th_known ; !pool
