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
  Theory.theory list ->
  session

val save : session -> unit

type theory

val name : theory -> string
val theory : theory -> Theory.theory
val theories : session -> theory list

type goal

val split : theory -> goal list
val pr_name : Ident.ident -> string
val goal_name : goal -> string
val goal_idename : goal -> string
val goal_fullname : goal -> string
val goal_file : goal -> string
val goal_loc : goal -> Loc.position option
val goal_task : goal -> Task.task
val goal_theory : goal -> Theory.theory

val pp_goal : Format.formatter -> goal -> unit

val thy_name : Theory.theory -> string
val task_name : Task.task -> string
val task_expl : Task.task -> string

val result : goal ->
  Whyconf.prover ->
  Call_provers.resource_limits ->
  Call_provers.prover_result ->
  unit

val apply : Env.env -> Tactic.tactic -> ?allow_no_effect:bool -> goal
  -> goal list option
(** Apply a tactic to a goal. When allow_no_effect is [false] (the default),
    the function treat the tactic returning the goal unchanged as an "error"
    and return {!None}. When it is [true] this condition is treated as a normal
    result and the goal is returned. *)

val prev : goal -> goal option
val next : goal -> goal option
val step_in : goal -> goal option
val step_out : goal -> goal option

(* -------------------------------------------------------------------------- *)
