(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {1 Debug Adapater Protocol }*)

type adapter


(** {2 Base Protocol} *)

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
val schedule: adapter -> (adapter -> unit) -> unit

(** Conversions from/to Json with respect to server capabilities *)

val of_line : start:int -> Json.t -> int
val to_line : start:int -> int -> Json.t
val of_column : start:int -> Json.t -> int
val to_column : start:int -> int -> Json.t
val of_path : Json.t -> string
val to_path : string -> Json.t

(** Register adapter capbility. Default value `true`. *)
val register_capability : string -> ?value:Json.t -> unit -> unit

(** {2 Output & Logging} *)

type category = [ `console | `important | `stdout | `stderr ]

val output : adapter -> ?category:category ->
  ('a, Format.formatter, unit, unit) format4 -> 'a

(** {2 Launch Sequence} *)

(** Attached Request callback *)
val on_attach : (adapter -> nodebug:bool -> unit) -> unit

(** Launched Request callback *)
val on_launch : (adapter -> nodebug:bool -> restart:Json.t -> unit) -> unit

(** {2 End Sequence} *)

(** Emits an exited event with the corresponding exit code. *)
val exited : adapter -> int -> unit

(** Emits a terminated event. *)
val terminated : adapter -> ?restart:Json.t -> unit -> unit

(** Terminate request callback. *)
val on_terminate : (adapter -> restart:bool -> unit) -> unit

(** Disconnect request arguments *)
type disconnect = Disconnect of { restart : bool ; terminate : bool ; suspend : bool }
(** Disconnect request callback. Use the options to register the associated capabilities. *)
val on_disconnect : ?terminate:bool -> ?suspend:bool -> (adapter -> disconnect -> unit) -> unit

(** {2 Debuggee State} *)

type thread = private Thread of { id : int ; name : string ; mutable paused : bool }

val updated : adapter -> ?reason:string -> thread -> unit
val thread : adapter -> name:string -> ?paused:bool -> unit -> thread
val kill : adapter -> thread -> unit

val pause : thread -> unit
val resume : thread -> unit

type reason = [ `step | `breakpoint | `error | `pause | `entry ]

(** Emit a stopped event.
    If ``~thread` is set, pause this thread.
    If `~all:true` is set, pause all threads. *)
val stopped : adapter -> reason:reason ->
  ?description:string -> ?text:string ->
  ?thread:thread -> ?all:bool ->
  ?preserveFocus:bool ->
  unit -> unit

val on_pause : (adapter -> thread -> unit) -> unit
val on_continue : ?single:bool -> (adapter -> single:bool -> thread -> unit) -> unit
