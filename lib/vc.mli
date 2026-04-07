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

type context = {
  file : string ;
  line : int ; end_line : int ;
  chunks : chunk list ;
}

val dump : file:string -> Why3.Task.task -> unit
val context : file:string -> Why3.Theory.theory -> Why3.Task.task -> context
val highlight : file:string -> context:int -> Why3.Theory.theory -> Why3.Task.task -> unit

type fullname = { name : string ; expl : string ; head : string }

val id_name : Why3.Ident.ident -> string
val task_name : Why3.Task.task -> string
val task_expl : Why3.Task.task -> string
val task_fullname : Why3.Task.task -> fullname
