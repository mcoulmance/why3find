(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Word detection *)

module Cmap = Map.Make(Char)

type 'a t = {
  data : 'a option ;
  next : 'a t Cmap.t ;
}

let rec dump fmt n =
  begin
    Format.fprintf fmt "@[<hov 2>{" ;
    if n.data <> None then Format.fprintf fmt "* " ;
    Cmap.iter
      (fun c n ->
         Format.fprintf fmt "@ %c: %a" c dump n
      ) n.next ;
    Format.fprintf fmt " }@]" ;
  end

let empty = { data = None ; next = Cmap.empty }

let rec insert k a v node =
  try
    let c = a.[k] in
    let n0 = try Cmap.find c node.next with Not_found -> empty in
    let n1 = insert (succ k) a v n0 in
    { node with next = Cmap.add c n1 node.next }
  with Invalid_argument _ ->
    { node with data = Some v }

let add a v n = insert 0 a v n

let rec lookup best text k node =
  try
    let best = match node.data with None -> best | data -> data in
    lookup best text (succ k) (Cmap.find text.[k] node.next)
  with Not_found | Invalid_argument _ ->
  match node.data with
  | Some _ as data -> data
  | None -> best

let find text k n = lookup None text k n

let starts_with ~prefix text k =
  try
    let n = String.length prefix in
    for i = 0 to n - 1 do
      if text.[k + i] <> prefix.[i] then raise Not_found
    done ; true
  with Not_found | Invalid_argument _ -> false

let ends_with ~suffix text k =
  try
    let n = String.length suffix in
    for i = 0 to n - 1 do
      if text.[k - n + i] <> suffix.[i] then raise Not_found
    done ; true
  with Not_found | Invalid_argument _ -> false
