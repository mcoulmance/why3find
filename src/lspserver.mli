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

type document = Doc of { uri: string ; lang: string ; text: Ropes.t }
type location = Loc of { uri: string ; range: range }
type update = { before: Ropes.t ; change: change ; after: Ropes.t }

type markup = [ `Plaintext | `Markdown ]
type hover = Hover of {
    range : range option ;
    kind : markup ;
    value : string list ;
  }

type severity = [ `Error | `Warning | `Information | `Hint ]
type diagnostic = Diag of {
    range : range ;
    severity : severity ;
    source : string option ;
    message : string ;
    data : Json.t ;
  }

val backward_position : update -> position -> position option
val backward_range : update -> range -> range option

val document : uri:string -> document

val on_save : (Lsp.channel -> document -> unit) -> unit
val on_open : (Lsp.channel -> document -> unit) -> unit
val on_close : (Lsp.channel -> document -> unit) -> unit
val on_update : (Lsp.channel -> uri:string -> update -> unit) -> unit

val on_definition :
  (Lsp.channel -> uri:string -> position -> location list) -> unit

(** Markdown code block *)
val code : ?lang:string -> string list -> string list

(** Markdown hover. Default is `Plaintext *)
val hover : ?range:range -> ?kind:markup -> string list -> hover

val on_hover : (Lsp.channel -> uri:string -> position -> hover) -> unit

val diagnostic :
  range:range ->
  ?severity:severity ->
  ?source:string ->
  message:string ->
  ?data:Json.t ->
  unit -> diagnostic

val publish_diagnostic : Lsp.channel -> uri:string -> diagnostic list -> unit
