(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Dynamic Arrays *)

type 'a t
val get : 'a t -> int -> 'a
val length : 'a t -> int

val map : ('a -> 'b) -> 'a t -> 'b t
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit

val empty : 'a t
val const : length:int -> 'a -> 'a t
val option : 'a option -> 'a t
val array : 'a array -> 'a t
val list : 'a list -> 'a t
val concat : 'a t -> 'a t -> 'a t

type 'a buffer

val create : unit -> 'a buffer

val add : 'a buffer -> 'a -> unit

(** Returns the index of the added element. *)
val addi : 'a buffer -> 'a -> int

(** Returns all the elements added to the buffer {i so far}. *)
val contents : 'a buffer -> 'a t

(** Build the collection from a sequence. *)
val sequence : ?overhead:int -> 'a Seq.t -> 'a t

(** Low-level constructor *)
val make : length:(unit -> int) -> value:(int -> 'a) -> 'a t

(** Low-level constructor, with cache *)
val cache : length:(unit -> int) -> value:(int -> 'a) -> 'a t
