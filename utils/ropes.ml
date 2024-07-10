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

type pos = int * int
type range = pos * pos

type t =
  | Empty
  | Line of string
  | Cat of { lines : int ; left : t ; right : t }

(* -------------------------------------------------------------------------- *)
(* --- Accessors                                                          --- *)
(* -------------------------------------------------------------------------- *)

let lines = function Empty -> 0 | Line _ -> 1 | Cat { lines } -> lines

let rec line_at l = function
  | Empty -> invalid_arg "Ropes.line_at"
  | Line s -> if l <> 0 then invalid_arg "Ropes.line_at" ; s
  | Cat { left ; right } ->
    let n = lines left in
    if l < n then line_at l left else line_at (l-n) right

let char_at (l,c) a = String.get (line_at l a) c

let start = 0,0

let eof a =
  let rec last l = function
    | Empty -> l, 0
    | Line s -> l, String.length s
    | Cat { left ; right } -> last (l + lines left) right
  in last 0 a

(* -------------------------------------------------------------------------- *)
(* --- Balancing                                                          --- *)
(* -------------------------------------------------------------------------- *)

let make a b = Cat { lines = lines a + lines b ; left = a ; right = b }

let rec concat a b =
  match a,b with
  | Empty,c | c,Empty -> c
  | Cat { left ; right } , _ when lines a > lines b ->
    make left (concat right b)
  | _ , Cat { left ; right } when lines a < lines b ->
    make (concat a left) right
  | _ -> make a b

let (++) = concat

(* -------------------------------------------------------------------------- *)
(* --- Iterators                                                          --- *)
(* -------------------------------------------------------------------------- *)

let rec fold_left f w = function
  | Empty -> w
  | Line s -> f w s
  | Cat { left ; right } -> fold_left f (fold_left f w left) right

let rec fold_right f a w =
  match a with
  | Empty -> w
  | Line s -> f s w
  | Cat { left ; right } -> fold_right f left (fold_right f right w)

let iter f a = fold_left (fun () s -> f s) () a
let iteri f a = ignore @@ fold_left (fun k s -> f k s ; succ k) 0 a

let length = fold_left (fun n s -> n + 1 + String.length s) 0

(* -------------------------------------------------------------------------- *)
(* --- Printers                                                           --- *)
(* -------------------------------------------------------------------------- *)

let bline b s = Buffer.add_string b s ; Buffer.add_char b '\n' ; b
let blines = fold_left bline

let to_buffer b s = ignore @@ blines b s

let to_channel out s =
  ignore @@
  fold_left (fun o s -> output_string o s ; output_char o '\n' ; o) out s

let to_string = function
  | Empty -> ""
  | Line n -> n ^ "\n"
  | a -> Buffer.contents @@ blines (Buffer.create 0) a

let pretty fmt s =
  ignore @@
  fold_left
    (fun fmt s ->
       Format.pp_print_string fmt s ;
       Format.pp_print_newline fmt () ;
       fmt)
    fmt s

(* -------------------------------------------------------------------------- *)
(* --- Constructors                                                       --- *)
(* -------------------------------------------------------------------------- *)
let empty = Empty
let trim =
  let rec trimk l r = function
    | Empty -> Empty
    | Line "" -> Empty
    | Line _ as a -> a
    | Cat { left ; right } ->
      (if l then trimk true false left else left) ++
      (if r then trimk false true right else right)
  in trimk true true

let split s =
  List.fold_left (fun w s -> w ++ Line s) Empty (String.split_on_char '\n' s)

let rec endline = function
  | (Empty | Line _) as a -> a
  | Cat { left ; right = Line "" } -> left
  | Cat { left ; right } -> make left (endline right)

let of_string s =
  if String.length s = 0 then Empty else endline @@ split s

let prefix_string c s =
  if c <= 0 then "" else
    let n = String.length s in
    if c < n then String.sub s 0 c else s

let suffix_string c s =
  if c <= 0 then s else
    let n = String.length s in
    if c < n then String.sub s c (n-c) else ""

let prefix p a =
  let rec wprefix acc l = function
    | Empty -> acc, ""
    | Line s -> acc, if l = 0 then s else ""
    | Cat { left ; right } ->
      let n = lines left in
      if l < n then wprefix acc l left else wprefix (acc ++ left) (l-n) right
  in let w,s =  wprefix Empty (fst p) a in w , prefix_string (snd p) s

let suffix p a =
  let rec wsuffix l a acc =
    match a with
    | Empty -> "", acc
    | Line s -> (if l = 0 then s else ""), acc
    | Cat { left ; right } ->
      let n = lines left in
      if l < n then wsuffix l left (right ++ acc) else wsuffix (l-n) right acc
  in let s,w =  wsuffix (fst p) a Empty in suffix_string (snd p) s, w

let dump = iteri (Format.printf "#%d %S@.")

let update (p,q) s a =
  if Range.(<<=) p q then
    begin
      Format.printf "----------------------------------------@." ;
      dump a ;
      let l,u = prefix p a in
      let v,r = suffix q a in
      l ++ split (u ^ s ^ v) ++ r
    end
  else a

let substring (p,q) a =
  let l1 = fst p in
  let l2 = snd q in
  if l1 > l2 then "" else
  if l1 < l2 then
    let u,w = suffix p a in
    let w,v = prefix (l2-l1-1,snd q) w in
    let b = Buffer.create 0 in
    ignore (bline b u) ;
    ignore (blines b w) ;
    Buffer.add_string b v ;
    Buffer.contents b ;
  else
    let u = line_at l1 a in
    let p = max (snd p) 0 in
    let q = min (snd q) (String.length u) in
    if p < q then String.sub u p (q-p) else ""
