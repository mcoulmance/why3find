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
