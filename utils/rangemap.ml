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

open Range

type 'a t =
  | Empty
  | Range of { range : range ; value : 'a ; inner : 'a t }
  | Node of { range : range ; left : 'a t ; right : 'a t }

let rec dump pp fmt = function
  | Empty -> Format.fprintf fmt "-"
  | Range { range ; value ; inner } ->
    Format.fprintf fmt "@[<hv 2>{ %a@ value: %a@ inner: %a }@]"
      Range.pp_range range pp value (dump pp) inner
  | Node { range ; left ; right } ->
    Format.fprintf fmt "@[<hv 2>{ %a@ left: %a@ right: %a }@]"
      Range.pp_range range (dump pp) left (dump pp) right
[@@ warning "-32"]

let empty = Empty

let compare (a,b) (c,d) =
  let cmp = compare c a in
  if cmp <> 0 then cmp else
    compare b d

let inside p (a,b) = a <<= p && p << b

let subset (a,b) (c,d) = c <<= a && b <<= d

let compare_entry x y =
  match x,y with
  | None, None -> 0
  | Some _, None -> (-1)
  | None, Some _ -> (+1)
  | Some(a,_) , Some (b,_) -> compare a b

let rec find_opt p = function
  | Empty -> None
  | Range { range ; value ; inner } ->
    begin
      match find_opt p inner with
      | Some _ as r -> r
      | None -> if inside p range then Some(range,value) else None
    end
  | Node { range ; left ; right } ->
    if inside p range then
      let l = find_opt p left in
      let r = find_opt p right in
      if compare_entry l r < 0 then l else r
    else None

let find p t = match find_opt p t with None -> raise Not_found | Some e -> e

let singleton rg v = Range { range = rg ; value = v ; inner = Empty }

let range = function
  | Empty -> assert false
  | Range { range } | Node { range } -> range

let (++) u v =
  let a,b = range u in
  let c,d = range v in
  if b <<= c then
    Node { range = a,d ; left = u ; right = v }
  else
  if d <<= a then
    Node { range = c,b ; left = v ; right = u }
  else
    let p = if a <<= c then a else c in
    let q = if b <<= d then d else b in
    Node { range = p,q ; left = u ; right = v }

let union a b =
  match a,b with
  | Empty,c | c,Empty -> c
  | _ -> a ++ b

let rec add rg v = function
  | Empty -> singleton rg v
  | Range r as a ->
    if subset rg r.range then
      if subset r.range rg then
        Range { r with value = v }
      else
        Range { r with inner = add rg v r.inner }
    else
      singleton rg v ++ a
  | Node { range = ra ; left ; right } as a ->
    if subset ra rg then
      Range { range = rg ; value = v ; inner =  a }
    else
    if rg <<< range right then add rg v left ++ right else
    if range left <<< rg then left ++ add rg v right else
      singleton rg v ++ a
