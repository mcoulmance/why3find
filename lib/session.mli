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
val goal_idename : goal -> string
val goal_fullname : goal -> string
val goal_file : goal -> string
val goal_loc : goal -> Why3.Loc.position option
val goal_task : goal -> Why3.Task.task
val goal_theory : goal -> Why3.Theory.theory
val goal_context : goal -> Vc.context
val goal_proof : goal -> Crc.crc option

val pp_goal : Format.formatter -> goal -> unit

val result : goal ->
  Why3.Whyconf.prover ->
  Why3.Call_provers.resource_limits ->
  Why3.Call_provers.prover_result ->
  unit

val apply : Why3.Env.env -> Tactic.tactic -> ?allow_no_effect:bool -> goal
  -> goal list option
(** Apply a tactic to a goal. When allow_no_effect is [false] (the default),
    the function treat the tactic returning the goal unchanged as an "error"
    and return {!None}. When it is [true] this condition is treated as a normal
    result and the goal is returned. *)

val commit : goal -> Crc.crc -> unit

val parent : goal -> goal option
val prev : goal -> goal option
val next : goal -> goal option
val step_in : goal -> goal option
val step_out : goal -> goal option
val iter : (goal -> unit) -> goal -> unit

type 'a hmap

val hmap : int -> 'a hmap
val add : 'a hmap -> goal -> 'a -> unit
val replace : 'a hmap -> goal -> 'a -> unit
val remove : 'a hmap -> goal -> unit
val mem : 'a hmap -> goal -> bool
val find : 'a hmap -> goal -> 'a
val find_opt : 'a hmap -> goal -> 'a option
val find_all : 'a hmap -> goal -> 'a list
val clear : 'a hmap -> unit

(* -------------------------------------------------------------------------- *)
