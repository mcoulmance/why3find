(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Proof Client                                                       --- *)
(* -------------------------------------------------------------------------- *)

type client

val options : (string * Arg.spec * string) list
val connect : Project.env -> client option
val request : client ->
  Calibration.profile -> Prover.prover -> Runner.prooftask -> float ->
  Result.t Fibers.signal

val yield : unit -> unit
val terminate : unit -> unit

(* -------------------------------------------------------------------------- *)
