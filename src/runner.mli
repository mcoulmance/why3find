(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Why3 Runner                                                        --- *)
(* -------------------------------------------------------------------------- *)

type callback =
  Why3.Whyconf.prover ->
  Why3.Call_provers.resource_limits ->
  Why3.Call_provers.prover_result ->
  unit

val prove :
  Why3.Whyconf.main ->
  ?name:string ->
  ?idename:string ->
  ?cancel:unit Fibers.signal ->
  ?callback:callback ->
  Cache.t -> Prover.prover -> Why3.Task.task -> float -> Result.t Fibers.t
(** Use and update cache *)

type prooftask

val digest : prooftask -> string
val data : prooftask -> string
val prepare : Prover.prover -> Why3.Task.task -> prooftask

val prove_cached : Cache.t -> Prover.prover -> Why3.Task.task -> float -> Result.t option
(** Query cache only *)

val prove_prepared : Why3.Whyconf.main ->
  ?name:string ->
  ?idename:string ->
  ?cancel:unit Fibers.signal ->
  ?callback:callback ->
  Prover.prover -> prooftask -> float -> Result.t Fibers.t
(** Do not read nor update cache *)

val prove_buffer : Why3.Whyconf.main ->
  ?cancel:unit Fibers.signal ->
  Prover.prover -> Buffer.t -> float -> Result.t Fibers.t
(** Do not read nor update cache *)

val notify : Why3.Whyconf.main -> Prover.prover -> Result.t -> callback -> unit
val memlimit : Why3.Whyconf.main -> int

val set_jobs : int -> unit
val get_jobs : Why3.Whyconf.main -> int

val pending : unit -> int
val running : unit -> int
val pp_goals : Format.formatter -> unit

val set_logfile : string -> unit

val clear_stats : unit -> unit
val print_stats : Format.formatter -> unit

(* -------------------------------------------------------------------------- *)
