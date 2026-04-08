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
type parent =
  | Root of (Crc.crc -> unit)
  | Parent of node

type state =
  | Paused            (* in q_paused *)
  | Scheduled         (* in q_proven or q_broken *)
  | Closed of Crc.crc (* not in any queue *)
  | Stacked of Tactic.tactic * node list (* children still queued *)

type result =
  | Local of Result.t
  | Profile of Result.t

val proof : node -> Crc.crc
val goal : node -> Session.goal
val parent : node -> parent
val state : node -> state
val result : node -> Prover.prover -> result
val results : node -> (Prover.prover * result) list

val pp_node : Format.formatter -> node -> unit
val pp_state : Format.formatter -> state -> unit
val pp_result : Format.formatter -> result -> unit


(** Creates a root node for some goal to be proved. *)
val prove : hammer ->
  ?callback:(crc -> unit) ->
  goal -> crc -> node

(** Current Task Server State *)
type progress = {
  broken  : int ; (** Number of pending tasks with broken CRC *)
  proven  : int ; (** Number of pending tasks with proven CRC *)
  pending : int ; (** Number of pending prover tasks *)
  running : int ; (** Number of running prover tasks *)
}

val pp_progress : Format.formatter -> progress -> unit

val iter : (node -> unit) -> unit
val progress : unit -> progress

type status =
  | Proving  (** Running provers normally *)
  | Pausing  (** Pausing pending nodes and interrupting provers *)
  | Paused   (** Finally paused *)
  | Stopping  (** Terminating pending nodes and running provers *)
  | Stopped   (** Finally stopped *)
  | Complete (** All proof tasks completely finished *)

val status : unit -> status

val pending : unit -> bool
val completing : unit -> bool
(** Emitted CRCs are complete: not stopping nor pausing *)

val enter : node Fibers.signal
val leave : node Fibers.signal
val failed : node Fibers.signal

(** Interrupt all running provers and discard all pending nodes. *)
val stop : unit -> unit

(** Interrupt all running provers and pauses all nodes. *)
val pause : unit -> unit

(** Re-schedule all paused nodes and continue. *)
val resume : unit -> unit

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
