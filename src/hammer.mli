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
type parent = Thread of int | Node of node
type state =
  | Paused            (* in q_paused *)
  | Scheduled         (* in q_proven or q_broken *)
  | Failed of Crc.crc (* in q_failed *)
  | Closed of Crc.crc (* not in any queue *)
  | Stacked of Tactic.tactic * node list (* children still queued *)

val result : node -> Crc.crc Fibers.t
val goal : node -> Session.goal
val parent : node -> parent
val state : node -> state
val thread_id : node -> int

(** Creates a root node for some goal to be proved. *)
val thread : hammer -> ?id:int -> ?cancel:unit Fibers.signal -> goal -> crc -> node

(** Current Task Server State *)
type progress = {
  broken  : int ; (** Number of pending tasks with broken CRC *)
  proven  : int ; (** Number of pending tasks with proven CRC *)
  pending : int ; (** Number of pending prover tasks *)
  running : int ; (** Number of running prover tasks *)
}

type status =
  | Proving of progress (** Nodes are pending *)
  | Paused (** Empty queues with some paused nodes *)
  | Exited (** All nodes and fibers have been processed *)

val pp_status : Format.formatter -> status -> unit
val pp_progress : Format.formatter -> progress -> unit

val iter : (node -> unit) -> unit
val status : unit -> status
val progress : unit -> progress
val running : unit -> bool
val interrupting : unit -> bool

(** Interrupt all running provers and discard all pending nodes. *)
val interrupt : unit -> unit

(** Interrupt all running provers and pauses all nodes. *)
val pause : unit -> unit

(** Commit this node and all its children to their current status. *)
val terminate : node -> unit

(** Re-schedule the given nodes, pausing children and failed nodes. *)
val step_in : node list -> unit

(** Re-schedule the given nodes, auto-scheduling children but pausing failed nodes. *)
val step_out : node list -> unit

(** Re-schedule all paused nodes and continue, pausing failed nodes. *)
val resume : unit -> unit

(** Re-schedule all paused nodes and continue, commiting all nodes. *)
val resume_to_end : unit -> unit

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
