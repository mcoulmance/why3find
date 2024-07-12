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

val self : unit

val to_file : uri:string -> string option
val to_uri : filename:string -> string

type position = { line: int ; char : int }
type range = { start: position ; stop: position }
type change = Text of string | Update of range * string

type document = { uri: string ; lang: string ; text: Ropes.t }
type update = { before: Ropes.t ; change: change ; after: Ropes.t }

val document : uri:string -> document

val on_save : (Lsp.channel -> document -> unit) -> unit
val on_open : (Lsp.channel -> document -> unit) -> unit
val on_close : (Lsp.channel -> document -> unit) -> unit
val on_update : (Lsp.channel -> uri:string -> update -> unit) -> unit

type severity = [ `Error | `Warning | `Information | `Hint ]
type diagnostic = Diag of {
    range : range ;
    severity : severity ;
    source : string option ;
    message : string ;
    data : Json.t ;
  }

val diagnostic :
  range:range ->
  ?severity:severity ->
  ?source:string ->
  message:string ->
  ?data:Json.t ->
  unit -> diagnostic

val publish_diagnostic : Lsp.channel -> uri:string -> diagnostic list -> unit
