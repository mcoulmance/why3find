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

(** Text document with line-column random access *)

type t

type pos = int * int
type range = pos * pos

val lines : t -> int
val length : t -> int
val line_at : int -> t -> string
val char_at : pos -> t -> char

val start : pos (* 0,0 *)
val eof : t -> pos (* last line , length of last line *)

val empty : t
val concat : t -> t -> t
val ( ++ ) : t -> t -> t

val fold_left : ('a -> string -> 'a) -> 'a -> t -> 'a
val fold_right : (string -> 'a -> 'a) -> t -> 'a -> 'a

val iter : (string -> unit) -> t -> unit
val iteri : (int -> string -> unit) -> t -> unit

val to_buffer : Buffer.t -> t -> unit
val to_channel : out_channel -> t -> unit
val to_string : t -> string
val of_string : string -> t

val pretty : Format.formatter -> t -> unit

val trim : t -> t
val prefix_string : int -> string -> string
val suffix_string : int -> string -> string
val prefix : pos -> t -> t * string
val suffix : pos -> t -> string * t
val update : range -> string -> t -> t
val substring : range -> t -> string
