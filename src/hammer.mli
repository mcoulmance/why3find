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
(* --- Hammer Proof Strategy                                              --- *)
(* -------------------------------------------------------------------------- *)

open Crc
open Session

(** Hammer Strategy *)
type hammer

(** Options:
    - [~level] max hammer strategy level used (0..4, default 4)
    - [~replay] replay mode (default false)
    - [~minimize] minimize mode (default false)
    - [~provers] from environment's config
    - [~tactics] from environment's config
    - [~profile] from environment's config
    - [~preprocess] from environment's config
*)
val create :
  ?level:int ->
  ?replay:bool ->
  ?minimize:bool ->
  ?preprocess:Tactic.tactic option ->
  ?provers:Prover.prover list ->
  ?tactics:Tactic.tactic list ->
  ?profile:Calibration.profile ->
  Project.env -> hammer

(** Enqueue some goal to be processed. *)
val schedule : hammer -> ?cancel:unit Fibers.signal -> goal -> crc -> crc Fibers.t

(** Current Task Server State *)
type status = {
  broken  : int ; (** Number of pending tasks with broken CRC *)
  proven  : int ; (** Number of pending tasks with proven CRC *)
  pending : int ; (** Number of pending prover tasks *)
  running : int ; (** Number of running prover tasks *)
}

val status : unit -> status
val running : unit -> bool

(** Empty the current task queue and kill all running provers. *)
val interrupt : unit -> unit

(** Process one pending node, if any.
    Returns true if some node has been processed. *)
val process : unit -> bool

(** Loop process and log process on stdout until there is no more activity. *)
val run : unit -> unit

(** Generate a why3 ide configuration file for the hammer strategy and
    write it in the specified file. *)
val why3_ide_config :
  tactics:Tactic.tactic list ->
  provers:Prover.prover list ->
  time:int ->
  mem:int ->
  string -> unit

(* -------------------------------------------------------------------------- *)
