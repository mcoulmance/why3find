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

val options : (string * Arg.spec * string) list
val usecache : bool ref
val pending : unit -> int
val running : unit -> int
val maxjobs : Why3.Whyconf.main -> int
val pp_goals : Format.formatter -> unit

val is_modified : unit -> bool
val memlimit : Why3.Whyconf.main -> int
val save_config : Project.env -> unit
val print_stats : unit -> unit

(* -------------------------------------------------------------------------- *)
