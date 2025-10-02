(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
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
(** Load the content of the proofs file associated with the given mlw file. If a
    [~local] profile is provided, the returned profile also inherits from it in
    addition to the one present in the file. *)

(** Collection of proofs to be saved *)
type buffer

val create : file:string -> profile -> proofs -> buffer
val add : Session.theory -> Session.goal -> Crc.crc -> buffer -> unit
val init : Session.theory -> Session.goal list -> Crc.crc Mstr.t -> buffer -> unit
val save : buffer -> unit
