(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Hammer Proof Strategy                                              --- *)
(* -------------------------------------------------------------------------- *)

open Crc
open Session

(** Hammer Strategy *)
type hammer

val provers : hammer -> Prover.prover list
val tactics : hammer -> Tactic.tactic list

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

type node
val result : node -> Crc.crc Fibers.t
val goal : node -> Session.goal
val parent : node -> node option
val children : node -> node list

(** Enqueue some goal to be processed. *)
val schedule : hammer -> ?cancel:unit Fibers.signal -> goal -> crc -> node

(** Current Task Server State *)
type status = {
  broken  : int ; (** Number of pending tasks with broken CRC *)
  proven  : int ; (** Number of pending tasks with proven CRC *)
  pending : int ; (** Number of pending prover tasks *)
  running : int ; (** Number of running prover tasks *)
}

val iter : (node -> unit) -> unit

val status : unit -> status
val running : unit -> bool

(** Empty the current task queue and kill all running provers. *)
val interrupt : unit -> unit

(** Process one node. Returns [true] is some progress has been made. *)
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
