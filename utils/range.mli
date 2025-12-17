(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Location & Range utilities                                         --- *)
(* -------------------------------------------------------------------------- *)

(** Boundaries, line and column internally starts at 0, but pretty-printed to starts at 1 *)
type pos = private { line : int ; col : int }
type range = pos * pos

val ( << ) : pos -> pos -> bool
val ( <<= ) : pos -> pos -> bool
val ( <<< ) : range -> range -> bool

val start : pos
val next : pos -> pos
val prev : pos -> pos
val newline : pos -> pos
val after : pos -> char -> pos

(** Line and col starts at 0. *)
val at : ?line:int -> ?col:int -> unit -> pos

val pp_pos : Format.formatter -> pos -> unit
val pp_range : Format.formatter -> range -> unit
val pp_position : Format.formatter -> file:string -> range -> unit

val empty : range
val is_empty : range -> bool
val inside : pos -> range -> bool
val subset : range -> range -> bool
val disjoint : range -> range -> bool
val union : range -> range -> range
val diff : range -> range -> range

val first_line : range -> int
val last_line : range -> int
