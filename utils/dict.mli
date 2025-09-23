(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)


type 'a t
val empty : 'a t
val add : string -> 'a -> 'a t -> 'a t
val find : string -> int -> 'a t -> 'a option

val starts_with : prefix:string -> string -> int -> bool
val ends_with : suffix:string -> string -> int -> bool

val dump : Format.formatter -> 'a t -> unit
