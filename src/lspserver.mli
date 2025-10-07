(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {2 Basic Structures} *)

val to_file : uri:string -> string option
val to_uri : filename:string -> string

type location = Loc of { uri: string ; range: Range.range}
val location : uri:string -> range:Range.range -> location

(** {2 Document Synchronization} *)

type change = Text of string | Update of Range.range * string
type document = Doc of { uri: string ; lang: string ; text: Ropes.t }
type update = { before: Ropes.t ; change: change ; after: Ropes.t }

val backward_position : update -> Range.pos -> Range.pos option
val backward_range : update -> Range.range -> Range.range option

val forward_position : Range.pos -> update -> Range.pos option
val forward_range : Range.range -> update -> Range.range option

val document : uri:string -> document

val on_save : (Lsp.server -> document -> unit) -> unit
val on_open : (Lsp.server -> document -> unit) -> unit
val on_close : (Lsp.server -> document -> unit) -> unit
val on_update : (Lsp.server -> uri:string -> update -> unit) -> unit

(** {2 Document Diagnostic} *)

type severity = [ `Error | `Warning | `Information | `Hint ]

type related = Info of {
    location : location ;
    message : string ;
  }

type diagnostic = Diag of {
    range : Range.range ;
    severity : severity ;
    source : string option ;
    message : string ;
    related : related list ;
    data : Json.t ;
  }

val related : location:location -> message:string -> related
val diagnostic :
  range:Range.range ->
  ?severity:severity ->
  ?source:string ->
  message:string ->
  ?related:related list ->
  ?data:Json.t ->
  unit -> diagnostic

val publish_diagnostic : Lsp.server -> uri:string -> diagnostic list -> unit

(** {2 Goto Definitions} *)

type defkind = [ `Def | `Typedef | `Implementation ]

val on_definition : ?kind:defkind ->
  (Lsp.server -> uri:string -> Range.pos -> location list) -> unit

(** {2 Code Hovers} *)

type markup = [ `Plaintext | `Markdown ]

type hover = Hover of {
    range : Range.range option ;
    kind : markup ;
    value : string list ;
  }

(** Markdown code block *)
val code : ?lang:string -> string list -> string list

(** Markdown hover. Default is `Plaintext *)
val hover : ?range:Range.range -> ?kind:markup -> string list -> hover

val on_hover : (Lsp.server -> uri:string -> Range.pos -> hover) -> unit

(** {2 Code Lenses} *)

type command = Cmd of {
    title : string ;
    command : string ;
    args : Json.t list ;
  }

type codelens

val codelens : Range.pos -> Json.t -> codelens
val resolved : Range.pos -> command -> codelens

val on_codelens :
  ?resolve:(Range.pos -> Json.t -> command) ->
  (Lsp.server -> uri:string -> codelens list) -> unit

val refresh_codelens : Lsp.server -> unit

(** {2 Folding Ranges} *)

type folding = Folding of {
    range : Range.range ;
    kind : string option ;
    text : string option ;
  }

val folding : range:Range.range -> ?kind:string -> ?text:string -> unit -> folding

val on_folding : (Lsp.server -> uri:string -> folding list) -> unit
val refresh_folding : Lsp.server -> unit

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
  name:string -> kind:kind -> range:Range.range ->
  ?selection:Range.range -> ?children:symbol list ->
  unit -> symbol

val on_document_symbol : (Lsp.server -> uri:string -> symbol list) -> unit

(** {2 Worspace File Watching} *)

type event = [ `Created | `Changed | `Deleted ]

val on_file_event : (Lsp.server -> uri:string -> event -> unit) -> unit

(** Starts watching for file events on the given global file patterns. *)
val watch : Lsp.server ->
  ?events:event list -> ?uri:string -> string list -> Lsp.capability

(** {2 Workspace Execute Command} *)

val on_execute_command : string -> (Json.t list -> Json.t) -> unit
