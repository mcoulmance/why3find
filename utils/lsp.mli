(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {1 LSP Server Protocol }*)

type server

type response = (Json.t,Json.t) result

(** Establish the server or exit *)
val establish: Rpc.transport -> server

(** Shutdown the server *)
val shutdown: server -> unit

(** Testing if the server is still running.
    Returns true when there is no more in/out pending messages. *)
val running: server -> bool

(** Only process pending I/O (non-blocking). *)
val yield: server -> unit

(** Process (at most) one pending LSP message, if any (non-blocking).
    First, yield the server to process pending I/O.
    Then, process one LSP message, if any.
    Returns true is some request has been processed or not.
    This function immediately returns false when it is called during
    a request handler.
*)
val process: server -> bool

(** {1 LSP Server Capabilities }*)

type capability

(**
   Dynamically register a server capability.
   Can be unregistered mater with the returned capability.
*)
val register_capability: server -> string -> Json.t -> capability

(** Unregister a server capability. *)
val unregister_capability: server -> capability -> unit

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
val register: ?force:bool -> string -> (server -> Json.t -> Json.t) -> unit

(** Ignore a request or notification. *)
val ignore: string -> unit

(** Notify the server *)
val notify: server -> string -> Json.t -> unit

(** Send a request or notification to the server *)
val send: server -> string -> ?callback:(response -> unit) -> Json.t -> unit
