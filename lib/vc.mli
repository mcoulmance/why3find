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
type chunk = { kind : kind ; range : Range.range }

val chunks: file:string -> Why3.Task.task -> chunk list

val dump : file:string -> Why3.Task.task -> unit
val highlight : file:string -> context:int -> Why3.Theory.theory -> Why3.Task.task -> unit
