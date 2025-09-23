(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Tactics are transformations without parameters *)

type tactic

val name : tactic -> string
val equal : tactic -> tactic -> bool
val pretty : Format.formatter -> tactic -> unit

val lookup : Why3.Env.env -> string -> tactic option
val select : Why3.Env.env -> string list -> tactic list

val iter : (string -> Why3.Pp.formatted -> unit) -> unit
