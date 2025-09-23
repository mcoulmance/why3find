(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Reversed Why3 Name Spaces *)

type t

val empty : t
val add : lib:string list -> Why3.Ident.ident -> t -> t
val add_types : lib:string list -> Why3.Theory.theory -> t -> t
val find : lib:string list -> Why3.Ident.ident -> t -> string list

val pp : Format.formatter -> t -> unit
