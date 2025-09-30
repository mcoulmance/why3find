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

(** {2 Locations} *)

val set_line_start : int -> unit
val set_column_start : int -> unit

val of_line : Json.t -> int
val to_line : int -> Json.t
val of_column : Json.t -> int
val to_column : int -> Json.t
val of_source : Json.t -> string
val to_source : string -> Json.t

type location
val loc :
  ?source:string ->
  ?line:int -> ?column:int ->
  ?end_line:int -> ?end_column:int ->
  unit -> location

(** {2 Variables} *)

type variable
val variable :
  name:string ->
  ?value:string ->
  ?vtype:string ->
  ?kind:string ->
  ?attributes:string list ->
  ?visiblity:string ->
  ?index:variable Darray.t ->
  ?named:variable Darray.t ->
  ?decl:location ->
  ?vdecl:location ->
  unit -> variable
(**
   @param kind ["property"], ["method"], ["class"], ["data"], ["event"],
   ["baseClass"], ["innerClass"], ["interface"], ["mostDerivedClass"], ["virtual"], etc.
   @param attributes ["static"], ["constant"], ["readOnly"], ["rawString"], etc.
   @param visibility ["public"], ["private"], ["protected"], ["internal"], ["final"], etc.
*)

(** {2 Scopes} *)

type scope
val scope :
  name:string ->
  ?kind:string ->
  ?index:variable Darray.t ->
  ?named:variable Darray.t ->
  ?expensive:bool ->
  ?range:location ->
  unit -> scope

(** {2 Stack Frames} *)

val setRestartableFrames : unit -> unit

type frame
val label : name:string -> frame
val frame :
  name:string ->
  ?range:location ->
  ?restart:(adapter -> unit) ->
  ?scopes:scope list ->
  unit -> frame

(** {2 Threads} *)

type reason = [ `step | `breakpoint | `error | `pause | `entry ]
type resume = [ `continue | `next | `step_in | `step_out ]

val set_single_thread_support : unit -> unit

class virtual thread :
  adapter ->
  name:string ->
  object
    method id : int
    method name : string
    method updated : unit
    method terminated : unit
    method stopped :
      reason:reason ->
      ?description:string -> ?text:string ->
      ?all:bool ->
      ?preserveFocus:bool ->
      unit -> unit

    method virtual on_paused : unit
    method virtual on_resumed : resume -> unit
    method virtual frames : frame Darray.t
  end
