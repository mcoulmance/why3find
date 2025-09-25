(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
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

(** Send and Event to the client *)
val emit: adapter -> string -> Json.t -> unit

(** Register a request handler *)
val register : ?force:bool -> string -> handler -> unit

(** Process (at most) one pending LSP message of one scheduled callback, if any.

    Always process pending non-blocking I/O.

    Then, provided no request nor callback is being processed:
    flush one pending callback, if any, or
    process one LSP message, if any.

    Returns true is some request or callback has been processed.
*)
val process : adapter -> bool

(** Same as [process]. *)
val yield: adapter -> unit

(** Schedule a callback to be executed during subsequent calls to `process` or `yield`. *)
val schedule: adapter -> (unit -> unit) -> unit

(** Conversions from/to Json with respect to server capabilities *)

val of_line : start:int -> Json.t -> int
val to_line : start:int -> int -> Json.t
val of_column : start:int -> Json.t -> int
val to_column : start:int -> int -> Json.t
val of_path : Json.t -> string
val to_path : string -> Json.t

(** Register adapter capbility. Default value `true`. *)
val register_capability : string -> ?value:Json.t -> unit -> unit

(** Attached Request callback *)
val on_attached : (debug:bool -> unit) -> unit

(** Launched Request callback *)
val on_launched : (debug:bool -> restart:Json.t -> unit) -> unit
