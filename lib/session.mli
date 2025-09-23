(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Session Management                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Why3

type session

val create :
  dir:string ->
  file:string ->
  format:string ->
  Why3.Theory.theory list ->
  session

val save : session -> unit

type theory

val name : theory -> string
val theory : theory -> Why3.Theory.theory
val theories : session -> theory list

type goal

val split : theory -> goal list
val goal_name : goal -> string
val goal_loc : goal -> Loc.position option
val goal_task : goal -> Task.task
val goal_expl : goal -> string
val goal_idename : goal -> string
val pp_goal : Format.formatter -> goal -> unit

val thy_name : Theory.theory -> string
val proof_name : Ident.ident -> string
val task_name : Task.task -> string
val task_expl : Task.task -> string

val result : goal ->
  Whyconf.prover ->
  Call_provers.resource_limits ->
  Call_provers.prover_result ->
  unit

val apply : Env.env -> Tactic.tactic -> goal -> goal list option

(* -------------------------------------------------------------------------- *)
