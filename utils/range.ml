(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Position & Ranges                                                  --- *)
(* -------------------------------------------------------------------------- *)

type pos = { line : int ; col : int } (* starts at 0 *)
type range = pos * pos

let pp_pos fmt p =
  Format.fprintf fmt "line %d, character %d" (p.line+1) (p.col+1)

let (<<) p1 p2  = p1.line < p2.line || (p1.line = p2.line && p1.col <  p2.col)
let (<<=) p1 p2 = p1.line < p2.line || (p1.line = p2.line && p1.col <= p2.col)
let (<<<) ((_,b):range) ((c,_):range) = b <<= c

let at ?(line=0) ?(col=0) () = { line ; col }
let start : pos = { line=0 ; col=0 }
let next p = { line = p.line ; col = p.col+1 }
let prev p = { line = p.line ; col = p.col-1 }
let newline p = { line = p.line+1 ; col = 0 }
let after p = function '\n' -> newline p | _ -> next p
let pp_range fmt (p,q) =
  if p.line = q.line then
    Format.fprintf fmt "line %d, characters %d-%d" (p.line+1) (p.col+1) (q.col+1)
  else
    Format.fprintf fmt "lines %d-%d" p.line q.line

let pp_position fmt ~file r =
  Format.fprintf fmt "file %S, %a" file pp_range r

let empty : range = start,start
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

let first_line (r : range) = (fst r).line
let last_line (r : range) = (snd r).line
