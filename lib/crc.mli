(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Proof Certificates                                                 --- *)
(* -------------------------------------------------------------------------- *)

type crc = private
  | Stuck
  | Prover of string * string option * float
  | Tactic of {
      id : string ;
      children : crc list ;
      stuck : int ;
      proved : int ;
      depth : int ;
      time : float ;
    }

val stuck : crc
val prover : Prover.prover -> float -> crc
val tactic : Tactic.tactic -> crc list -> crc

type verdict = [ `Valid of int | `Failed of int | `Partial of int * int ]
val verdict : crc -> verdict
val verdict_all : crc list -> verdict

val nverdict : stuck:int -> proved:int -> verdict

val get_stuck :  crc -> int
val get_proved : crc -> int
val get_time :   crc -> float
val get_depth :  crc -> int
val size :       crc -> int

val get_stuck_all :  crc list -> int
val get_proved_all : crc list -> int
val get_time_all :   crc list -> float
val get_depth_all :  crc list -> int

val is_stuck : crc -> bool
val is_unknown : crc -> bool
val is_complete : crc -> bool

val pretty : Format.formatter -> crc -> unit
val pp_status : Format.formatter -> crc -> unit
val pp_checked : Format.formatter -> stuck:int -> proved:int -> unit

val pp_time : Format.formatter -> float -> unit
val pp_depth : Format.formatter -> int -> unit
val pp_provers : Format.formatter -> crc list -> unit
val pp_verdict : Format.formatter -> verdict -> unit

val pp_hint : Format.formatter -> crc -> unit
val pp_hints : Format.formatter -> crc list -> unit

val pp_proof : Format.formatter -> crc -> unit
val pp_proofs : Format.formatter -> crc list -> unit

val dump : Format.formatter -> crc -> unit
val update : ?complete:bool -> hint:crc -> crc -> crc

val of_json : Json.t -> crc
val to_json : crc -> Json.t

(* -------------------------------------------------------------------------- *)
