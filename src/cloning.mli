(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
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
