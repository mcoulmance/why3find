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

(** {1 Debug Adapater Processing }*)

type adapter
type handler = adapter -> Json.t -> Json.t

(** Establish the adapter or exit *)
val establish: Rpc.transport -> adapter

(** Shutdown the adapter *)
val shutdown: adapter -> unit

(** Testing if the adapter is still running.
    Returns true when there is no more in/out pending messages. *)
val running: adapter -> bool

(** Only process pending I/O (non-blocking). *)
val yield: adapter -> unit

(** Send and Event to the client *)
val emit: adapter -> string -> Json.t -> unit

(** Register a request handler *)
val register : ?force:bool -> string -> handler -> unit

(** Process (at most) one pending LSP message, if any (non-blocking).
    First, yield the adapter to process pending I/O.
    Then, process one LSP message, if any.
    Returns true is some request has been processed or not.
    This function immediately returns false when it called during
    a request handler.
*)
val process: adapter -> bool

(** Conversions from/to Json with respect to server capabilities *)

val of_line : start:int -> Json.t -> int
val to_line : start:int -> int -> Json.t
val of_column : start:int -> Json.t -> int
val to_column : start:int -> int -> Json.t
val of_path : Json.t -> string
val to_path : string -> Json.t

(** Register adapter capbility. Default value `true`. *)
val register_capability : string -> ?value:Json.t -> unit -> unit
