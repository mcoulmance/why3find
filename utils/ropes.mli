(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Text document with line-column random access *)

type t

val lines : t -> int
val length : t -> int
val line_at : int -> t -> string
val char_at : Range.pos -> t -> char

val eof : t -> Range.pos (* last line , length of last line *)

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
val prefix : Range.pos -> t -> t * string
val suffix : Range.pos -> t -> string * t
val update : Range.range -> string -> t -> t
val substring : Range.range -> t -> string
