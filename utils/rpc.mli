(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Base protocol for LSP and DAP *)

type channel
type transport =
  | PIPE of string
  | PORT of int

(** Create connection *)
val establish : transport -> channel

(** Send and Receives buffered bytes *)
val yield : channel -> unit

(** Send all remaining bytes *)
val flush : channel -> unit

(** Decode one incoming message, if any *)
val pull : channel -> string option

(** Encode one message to outcome *)
val push : channel -> string -> unit

(** Has buffered bytes *)
val processing : channel -> bool
