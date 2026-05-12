(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Documentation Generator Tokens                                     --- *)
(* -------------------------------------------------------------------------- *)

type position = Lexing.position * Lexing.position

type style =
  | Head
  | Emph
  | Bold
  | Dash
  | Ordered

type token =
  | Eof
  | Newline
  | Char of char
  | Text of string
  | Comment of string
  | Infix of string
  | Ident of string
  | OpenDoc
  | CloseDoc
  | OpenSection of bool * string
  | CloseSection of string
  | Space
  | Style of style * string
  | Ref of string
  | Verb of string

val src_lexer : (Lexing.lexbuf -> token) ref
val doc_lexer : (Lexing.lexbuf -> token) ref

type input

(** Opens the given file. *)
val input : ?doc:bool -> ?filename:string -> string -> input

(** Closes the opened channel. *)
val close : input -> unit

(** End of file has been reached. *)
val eof : input -> bool

(** In source code. *)
val src : input -> bool

(** In documentation. *)
val doc : input -> bool

(** Set error handler routine. By default, exits with error code 1. *)
val set_handler : (input -> string -> unit) -> unit

(** Error message.
    @raises Exit unless error handler exits or raises an exception. *)
val error : input -> ('a,Format.formatter,unit,'b) format4 -> 'a

(** Fetch the next token from input. *)
val token : input -> token (** next buffer *)

(** Fetch the next identifier from input, skipping spaces only.
    Raises an error ["missing [kind] name"] if no identifier is found. *)
val fetch_id : kind:string -> input -> string

(** Position of the last token. *)
val position : input -> position

(** Line number of the last token. *)
val line : input -> int

(** Indentation of the token. *)
val indent : input -> int

(** The last returned token is at then beginninf of the line. *)
val startline : input -> bool

(** The last returned token is at then beginning of the line
    {i and} after an empty line. *)
val emptyline : input -> bool

val pp : Format.formatter -> token -> unit

(* -------------------------------------------------------------------------- *)
