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

(** Clone Instance Parsing *)

type export = Cloned | Exported | Aliased of string

val instance : cloned:Id.t -> scope:string list -> export -> string list
(** Returns a clone instance scope from current scope and export kind *)

type state

val init : state

val space : state -> state option
(** If none, the current state shall be kept and the space shall be printed *)

type token = [
  | `Id of Id.t
  | `Kwd of string
  | `Name of string
  | `Equal
  | `Coma
  | `Dot
  | `Comment
  | `Newline
]

type action = [
  | `Nop
  | `Space
  | `Spaces of int
  | `Target of Id.t * int
  | `Clone of int * export * Token.position * Id.t
]

val process : state -> Token.input -> token -> state * action
