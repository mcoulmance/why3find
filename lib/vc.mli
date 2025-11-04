(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* ---  Task Colorization                                                 --- *)
(* -------------------------------------------------------------------------- *)

type kind = Goal | When | WhenNot
type chunk = { kind : kind ; range : Range.range ; term : Why3.Term.term }
type window = { g1 : int ; g2 : int ; h1 : int ; h2 : int ; chunks : chunk list }

val by_range : chunk -> chunk -> int
val by_window : window -> window -> int

val dump : file:string -> Why3.Task.task -> unit
val window : file:string -> Why3.Theory.theory -> Why3.Task.task -> window
val highlight : file:string -> context:int -> Why3.Theory.theory -> Why3.Task.task -> unit
