(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type t =
  | NoResult | Failed | Unknown of float | Timeout of float | Valid of float

val map : (float -> float) -> t -> t
val merge : t -> t -> t

(** Lift the (cached) result to the given timeout, if applicable.
    An overhead of 1.5 accepted for valid results. *)
val crop : timeout:float -> t -> t option

(** Full description *)
val pp_result : Format.formatter -> t -> unit

(** No time info *)
val pp_verdict : Format.formatter -> t -> unit

val of_json : Json.t -> t
val to_json : t -> Json.t
