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

(** Attached Request callback *)
val on_attached : (debug:bool -> unit) -> unit

(** Launched Request callback *)
val on_launched : (debug:bool -> restart:Json.t -> unit) -> unit
