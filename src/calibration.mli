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
(* --- Prover Calibration                                                 --- *)
(* -------------------------------------------------------------------------- *)

type profile

val create : unit -> profile
val default : Project.env -> profile
val of_json : ?default:profile -> Json.t -> profile
val to_json : profile -> Json.t

val mem : profile -> Prover.desc -> bool
val get : profile -> Prover.desc -> (int * float) option
val set : profile -> Prover.desc -> int -> float -> unit
val iter : (Prover.desc -> int -> float -> unit) -> profile -> unit
val lock : profile -> Prover.desc -> bool
(** Returns [true] if the profile was not locked before. *)

(** Returns 1.0 when not calibrated *)
val observed : profile -> Prover.prover -> float

(** Local Time / Profile Time *)
val velocity : Why3.Whyconf.main -> Cache.t -> profile -> Prover.prover -> float Fibers.t

val profile : Why3.Whyconf.main -> Cache.t -> profile -> Prover.prover
  -> (int * float) Fibers.t

val calibrate_provers : saved:bool -> Project.env -> Prover.prover list -> profile Fibers.t
val velocity_provers : Project.env -> Prover.prover list -> unit

val options : (string * Arg.spec * string) list

(* -------------------------------------------------------------------------- *)
