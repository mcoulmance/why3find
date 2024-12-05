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
open Calibration
open Session

(** Enqueue some goal to be processed. *)
val schedule :
  profile ->
  replay:bool ->
  depth:int ->
  goal ->
  crc ->
  crc Fibers.t

(** Hammer Strategy Environment *)
type henv = {
  env : Config.env ;
  fast : float ;
  time : float ;
  client : Client.client option ;
  level : int ;
  maxdepth : int ;
  patterns : string list ;
  provers : Prover.prover list ;
  tactics : Tactic.tactic list ;
  minimize : bool ;
}

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
val process : henv -> bool

(** Loop process and log process on stdout until there is no more activity. *)
val run : henv -> unit

(** Generate a why3 ide configuration file for the hammer strategy and
    write it in the specified file. *)
val why3_ide_config :
  tactics:Tactic.tactic list ->
  provers:Prover.prover list ->
  time:int ->
  mem:int ->
  string -> unit

(* -------------------------------------------------------------------------- *)
