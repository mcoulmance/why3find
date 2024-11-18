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

(* -------------------------------------------------------------------------- *)
(* --- Position & Ranges                                                  --- *)
(* -------------------------------------------------------------------------- *)

type pos = int * int
type range = pos * pos

let pp_pos fmt (l,c) = Format.fprintf fmt "line %d, character %d" l (succ c)

let (<<) ((l1,c1):pos) ((l2,c2):pos) = l1 < l2 || (l1 = l2 && c1 < c2)
let (<<=) ((l1,c1):pos) ((l2,c2):pos) = l1 < l2 || (l1 = l2 && c1 <= c2)
let (<<<) ((_,b):range) ((c,_):range) = b <<= c

let start : pos = (1,0)
let next ((l,c):pos) = (l,succ c)
let prev ((l,c):pos) = (l,pred c)
let newline ((l,_):pos) = (succ l,0)
let after p = function '\n' -> newline p | _ -> next p
let pp_range fmt ((l,c),(l',d)) =
  if l = l' then
    Format.fprintf fmt "line %d, characters %d-%d" l (succ c) (succ d)
  else
    Format.fprintf fmt "lines %d-%d" l l'

let pp_position fmt ~file r =
  Format.fprintf fmt "file %S, %a" file pp_range r

let empty : range = (0,0),(0,0)
let is_empty (a,b) = b <<= a

let inside p (a,b) = a <<= p && p << b
let subset (a,b) (c,d) = c <<= a && b <<= d
let disjoint (a,b) (c,d) = b <<= c || d <<= a
let union ((a,b) as u) ((c,d) as v) =
  if is_empty u then v else
  if is_empty v then u else
    min a c, max b d
let diff (a,b) (c,d) =
  let r = max a d, min b c in
  if is_empty r then empty else r

let first_line (r : range) = fst @@ fst r
let last_line (r : range) = fst @@ snd r
