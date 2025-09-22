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

(** Decode one incomming message, if any *)
val pull : channel -> string option

(** Encode one message to outcome *)
val push : channel -> string -> unit

(** Has buffered bytes *)
val processing : channel -> bool
