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

(** {2 Basic Structures} *)

val to_file : uri:string -> string option
val to_uri : filename:string -> string

type position = { line: int ; char : int }
type range = { start: position ; stop: position }

(** {2 Document Synchronization} *)

type change = Text of string | Update of range * string
type document = Doc of { uri: string ; lang: string ; text: Ropes.t }
type update = { before: Ropes.t ; change: change ; after: Ropes.t }

val backward_position : update -> position -> position option
val backward_range : update -> range -> range option

val forward_position : position -> update -> position option
val forward_range : range -> update -> range option

val document : uri:string -> document

val on_save : (Lsp.channel -> document -> unit) -> unit
val on_open : (Lsp.channel -> document -> unit) -> unit
val on_close : (Lsp.channel -> document -> unit) -> unit
val on_update : (Lsp.channel -> uri:string -> update -> unit) -> unit

(** {2 Document Diagnostic} *)

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

(** {2 Goto Definitions} *)

type location = Loc of { uri: string ; range: range }
type defkind = [ `Def | `Typedef | `Implementation ]

val on_definition : ?kind:defkind ->
  (Lsp.channel -> uri:string -> position -> location list) -> unit

(** {2 Code Hovers} *)

type markup = [ `Plaintext | `Markdown ]

type hover = Hover of {
    range : range option ;
    kind : markup ;
    value : string list ;
  }

(** Markdown code block *)
val code : ?lang:string -> string list -> string list

(** Markdown hover. Default is `Plaintext *)
val hover : ?range:range -> ?kind:markup -> string list -> hover

val on_hover : (Lsp.channel -> uri:string -> position -> hover) -> unit

(** {2 Code Lenses} *)

type command = Cmd of {
    title : string ;
    command : string ;
    args : Json.t list ;
  }

type codelens

val codelens : position -> Json.t -> codelens
val resolved : position -> command -> codelens

val on_codelens :
  ?resolve:(position -> Json.t -> command) ->
  (Lsp.channel -> uri:string -> codelens list) -> unit

val refresh_codelens : Lsp.channel -> unit

(** {2 Folding Ranges} *)

type folding = Folding of {
    range : range ;
    kind : string option ;
    text : string option ;
  }

val folding : range:range -> ?kind:string -> ?text:string -> unit -> folding

val on_folding : (Lsp.channel -> uri:string -> folding list) -> unit
val refresh_folding : Lsp.channel -> unit

(** {2 Document Symbols} *)

type symbol
type kind = [
  | `File | `Module | `Namespace | `Package | `Class
  | `Method | `Property | `Field | `Constructor | `Enum | `Interface
  | `Function | `Variable | `Constant
  | `String | `Number | `Boolean | `Array | `Object | `Key
  | `Null | `EnumMember | `Struct
  | `Event | `Operator| `TypeParameter
]

val symbol :
  name:string -> kind:kind -> range:range ->
  ?selection:range -> ?children:symbol list ->
  unit -> symbol

val on_document_symbol : (Lsp.channel -> uri:string -> symbol list) -> unit

(** {2 Worspace File Watching} *)

type event = [ `Created | `Changed | `Deleted ]

val on_file_event : (Lsp.channel -> uri:string -> event -> unit) -> unit

(** Starts watching for file events on the given global file patterns. *)
val watch : Lsp.channel ->
  ?events:event list -> ?uri:string -> string list -> Lsp.capability

(** {2 Workspace Execute Command} *)

val on_execute_command : string -> (Json.t list -> Json.t) -> unit
