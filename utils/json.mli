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
(* --- JSON Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

type t = Yojson.Basic.t

val of_file : string -> t
val to_file : string -> t -> unit
val of_string : string -> t
val to_string : t -> string
val pretty : Format.formatter -> t -> unit

val is_empty : t -> bool

val jint : t -> int
val jbool : t -> bool
val jfloat : t -> float
val jstring : t -> string
val jstringlist : t -> string list
val jlist : t -> t list
val jmap : (t -> 'a) -> t -> 'a list
val jfield : string -> t -> t
val jfield_exn : string -> t -> t
val joption : (t -> 'a) -> t -> 'a option
val jdefault : 'a -> (t -> 'a) -> t -> 'a
val jmem : string -> t -> bool
val jiter : (string -> t -> unit) -> t -> unit

val int : int -> t
val string : string -> t
val assoc : (string * t) list -> t
val list : t list -> t
val list_map : ('a -> t) -> 'a list -> t
val option_map : ('a -> t) -> 'a option -> t
