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

val schedule :
  profile ->
  replay:bool ->
  depth:int ->
  idename:string ->
  goal ->
  crc ->
  crc Fibers.t
(** idename should be the goal name as in Why3 IDE *)

type henv = {
  env : Config.env ;
  time : float ;
  client : Client.client option ;
  level : int ;
  maxdepth : int ;
  patterns : string list ;
  provers : Prover.prover list ;
  tactics : Tactic.tactic list ;
  minimize : bool ;
}

val run : henv -> unit

val config :
  tactics:Tactic.tactic list ->
  provers:Prover.prover list ->
  time:int ->
  mem:int ->
  string (* config file *)

(* -------------------------------------------------------------------------- *)
