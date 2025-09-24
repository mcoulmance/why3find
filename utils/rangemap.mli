(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Range

type 'a t

val compare : range -> range -> int

val empty : 'a t
val add : range -> 'a -> 'a t -> 'a t
val union : 'a t -> 'a t -> 'a t
val find : pos -> 'a t -> range * 'a
val find_opt : pos -> 'a t -> (range * 'a) option

val iter : (range -> 'a -> unit) -> 'a t -> unit
