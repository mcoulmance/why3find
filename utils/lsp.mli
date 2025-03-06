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

(** {1 LSP Server Processing }*)

type transport =
  | PIPE of string
  | PORT of int

type channel

type response = (Json.t,Json.t) result

(** Establish the server or exit *)
val establish: transport -> channel

(** Shutdown the server *)
val shutdown: channel -> unit

(** Testing if the server is still running.
    Returns true when there is no more in/out pending messages. *)
val running: channel -> bool

(** Only process pending I/O (non-blocking). *)
val yield: channel -> unit

(** Process (at most) one pending LSP message, if any (non-blocking).
    First, yield the channel to process pending I/O.
    Then, process one LSP message, if any.
    Returns true is some request has been processed or not.
    This function immediately returns false when it called during
    a request handler.
*)
val process: channel -> bool

(** {1 LSP Server Capabilities }*)

type capability

(**
   Dynamically register a server capability.
   Can be unregistered mater with the returned capability.
*)
val register_capability: channel -> string -> Json.t -> capability

(** Unregister a server capability. *)
val unregister_capability: channel -> capability -> unit

(** {1 LSP Request Handlers }*)

(**
   Register a request or notification handler. Some builtin request and
   notifications are already registered and shall be forced updated, namely
   [initialize], [initialized], [shutdown], [exit] and [$/setTrace].

   The callbacks may raise exceptions, in which case an error is reported
   to the client. Meaningfull exceptions are:
   - [Not_found] request is not supported
   - [Invalid_argument _] request arguments are incorrect
   - [Failure _] and any other exception are treated as internal error
*)
val register: ?force:bool -> string -> (channel -> Json.t -> Json.t) -> unit

(** Ignore a request or notification. *)
val ignore: string -> unit

(** Notify the server *)
val notify: channel -> string -> Json.t -> unit

(** Send a request or notification to the server *)
val send: channel -> string -> ?callback:(response -> unit) -> Json.t -> unit
