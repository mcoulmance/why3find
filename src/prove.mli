(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Proof Manager                                                      --- *)
(* -------------------------------------------------------------------------- *)

type mode = [ `Force | `Update | `Minimize | `Replay ]
type results = [ `Default | `Modules | `Theories | `Goals | `Proofs | `Tasks | `Context of int ]

val stdlib : bool ref
val externals : bool ref
val builtins : bool ref

val clear_stats : unit -> unit
val merge_stats : Crc.crc -> Crc.crc -> unit
val print_stats : Prover.prover list -> Tactic.tactic list -> Format.formatter -> unit

type outcome = {
  provers : Prover.prover list ;
  tactics : Tactic.tactic list ;
  time : int ;
  mem : int ;
  unfixed : string list ;
}

val prove_files :
  env:Project.env ->
  mode:mode ->
  level:int ->
  session:bool ->
  results:results ->
  axioms:bool ->
  details:bool ->
  files:string list ->
  gnames:string list ->
  outcome
