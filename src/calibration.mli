(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
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

(** @raise Not_found when not available *)
val observed : profile -> Prover.prover -> float

(** Local Time / Profile Time *)
val velocity : Why3.Whyconf.main -> Cache.t -> profile -> Prover.prover -> float Fibers.t

val profile : Why3.Whyconf.main -> Cache.t -> profile -> Prover.prover
  -> (int * float) Fibers.t

val calibrate_provers : saved:bool -> Project.env -> Prover.prover list -> profile Fibers.t
val velocity_provers : Project.env -> Prover.prover list -> unit

val options : (string * Arg.spec * string) list

(* -------------------------------------------------------------------------- *)
