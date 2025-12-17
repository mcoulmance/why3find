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

type status = private
  | Stuck
  | Prover of Prover.desc * float
  | Tactic of {
      id : string ;
      children : crc list ;
      stuck : int ;
      proved : int ;
      depth : int ;
      time : float ;
    }

and crc = private {
  goal : Session.goal option ;
  status : status ;
}

val none : status
val stuck : ?goal:Session.goal -> unit -> crc
val prover : ?goal:Session.goal -> Prover.desc -> float -> crc
val tactic : ?goal:Session.goal -> Tactic.tactic -> crc list -> crc

(** Iterates over all goals in the CRC.
    Tactic nodes are visite first, followed by their children. *)
val iter : (Session.goal -> status -> unit) -> crc -> unit

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
val pp_status : Format.formatter -> status -> unit
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
