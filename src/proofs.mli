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
(* --- Proof Files                                                        --- *)
(* -------------------------------------------------------------------------- *)

open Calibration
open Why3.Wstdlib

(** Contains CRCs for theories and tasks of a single mlw file.
    The map is indexed first by theory name then by  task name. *)
type proofs = Crc.crc Mstr.t Mstr.t

val filename : string -> string
(** Returns the proofs file associated with the given mlw file *)

val load : ?local:profile -> string -> profile * proofs
(**
   Load the content of the proofs file associated with the given mlw file.
   If a [~local] profile is provided, the returned profile also inherits from
   it in addition to the one present in the file.
*)

(** Collection of proofs to be saved *)
type dumper

val create : string -> profile -> dumper
val add : Session.theory -> string -> Crc.crc -> dumper -> unit
val dump : dumper -> unit
