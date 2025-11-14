(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {1 Debug Adapater Protocol }*)

type adapter
val trace : bool ref

(** {2 Base Protocol} *)

type handler = adapter -> Json.t -> Json.t

(** Establish the adapter or exit *)
val establish: Rpc.transport -> adapter

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

(** Flush output channel. *)
val flush: adapter -> unit

(** Schedule a callback to be executed during subsequent calls to `process` or `yield`. *)
val schedule: adapter -> (adapter -> unit) -> unit

(** Register adapter capbility. Default value `true`. *)
val register_capability : string -> ?value:Json.t -> unit -> unit

(** {2 Output & Logging} *)

type category = [ `console | `important | `stdout | `stderr ]

val output : adapter -> ?category:category ->
  ('a, Format.formatter, unit, unit) format4 -> 'a

(** {2 Initialization Sequence} *)

val initialized : adapter -> unit
val on_init : (adapter -> unit) -> unit
(** By default, [on_init] emits an [initialized] event.
    If overriden, don't forget to emit an [initialized] event at some point. *)

val on_configured : (adapter -> unit) -> unit
(** See also breakpoint callback, below *)

(** {2 Launch Sequence} *)

val no_debug : unit -> bool
val restarted : unit -> Json.t

(** Attached Request callback *)
val on_attach : (adapter -> unit) -> unit

(** Launched Request callback *)
val on_launch : (adapter -> unit) -> unit

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

(** {2 Progress} *)

type progress

val idle : progress

val progress :
  adapter ->
  ?id:string ->
  title:string ->
  ?cancel:(adapter -> unit) ->
  ?message:string ->
  ?percent:int ->
  unit -> progress

val update : progress ->
  ?message:string -> ?percent:int ->
  unit -> unit

val finished : progress -> ?message:string -> unit -> unit

(** {2 Locations} *)

val of_line : Json.t -> int
val to_line : int -> Json.t
val of_column : Json.t -> int
val to_column : int -> Json.t
val of_source : Json.t -> string
val to_source : string -> Json.t

type location =  Loc of {
    file : string ;
    line : int ;
    column : int ;
    end_line : int ;
    end_column : int ;
  }

val js_locfields : location -> (string * Json.t) list

val loc :
  ?file:string ->
  ?line:int -> ?column:int ->
  ?end_line:int -> ?end_column:int ->
  unit -> location

(** {2 Breakpoints} *)

type breakpoint

val failed : ?message:string -> unit -> breakpoint
val pending : id:int -> ?message:string -> unit -> breakpoint
val verified : id:int -> ?message:string ->
  ?file:string -> ?line:int -> ?column:int -> ?end_line:int -> ?end_column:int ->
  unit -> breakpoint

val breakpoint :
  adapter ->
  id:int ->
  ?reason:string ->
  ?verified:bool ->
  ?message:string ->
  ?file:string -> ?line:int -> ?column:int -> ?end_line:int -> ?end_column:int ->
  unit -> unit
(** Update a (pending) breakpoint. *)

type source_breakpoint =
    SourceBreakpoint of {
      file : string ;
      line : int ;
      column : int ;
      condition : string ;
      hit_condition : string ;
      log_message : string ;
    }

val on_breakpoint :
  ?condition:bool ->
  ?hit_condition:bool ->
  ?log_message:bool ->
  ?clear:(unit -> unit) ->
  (source_breakpoint -> breakpoint) -> unit
(** If specified, the flags will activate the associated capabilities. *)

(** {2 Identified Objects}

    For custom requests, it is possible to attach attributes to identified
    objects and to recover them by object identifier.

    You first create an attribute map with {!attribute} function. Then, you can
    create values for this attribue map with {!value} and attach them to
    variables, frames and/or threads. Values can be obtained back from
    identifiers with {!find_by_varId}, {!find_by_frameId} and
    {!find_by_threadId}.

    Lifetime of identifiers ends on stopped events.
*)

type 'a attribute and value

val attribute : unit -> 'a attribute
val value : 'a attribute -> 'a -> value
val get_by_varId : 'a attribute -> int -> 'a option
val get_by_frameId : 'a attribute -> int -> 'a option
val get_by_threadId : 'a attribute -> int -> 'a option

(** {2 Variables}

    Its is recommended to use {!Darray.delayed} for returning
    large array of variables. *)

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
  ?values:value list ->
  unit -> variable
(**
   @param kind ["property"], ["method"], ["class"], ["data"], ["event"],
   ["baseClass"], ["innerClass"], ["interface"], ["mostDerivedClass"], ["virtual"], etc.
   @param attributes ["static"], ["constant"], ["readOnly"], ["rawString"], etc.
   @param visibility ["public"], ["private"], ["protected"], ["internal"], ["final"], etc.
*)

(** {2 Scopes}

    Its is recommended to use {!Darray.delayed} for returning
    large array of variables. *)

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
  ?subtle:bool ->
  ?range:location ->
  ?restart:(adapter -> unit) ->
  ?scopes:scope list ->
  ?values:value list ->
  unit -> frame

(** {2 Threads} *)

type thread
val thread :
  adapter ->
  name:string ->
  ?frames:(thread -> frame Darray.t) ->
  ?on_pause:(thread -> unit) ->
  ?on_next:(thread -> unit) ->
  ?on_step_in:(thread -> unit) ->
  ?on_step_out:(thread -> unit) ->
  ?on_continue:(thread -> unit) ->
  unit -> thread

val connect :
  thread ->
  ?frames:(thread -> frame Darray.t) ->
  ?on_pause:(thread -> unit) ->
  ?on_next:(thread -> unit) ->
  ?on_step_in:(thread -> unit) ->
  ?on_step_out:(thread -> unit) ->
  ?on_continue:(thread -> unit) ->
  unit -> unit

val id : thread -> int
val on : thread -> adapter
val name : thread -> string

(** Sorted by Id. *)
val threads : unit -> thread list

(** Find by Id *)
val find : int -> thread

val get : 'a attribute -> thread -> 'a option
val set : 'a attribute -> thread -> 'a -> unit
val remove : 'a attribute -> thread -> unit

val living : thread -> bool
val updated : ?reason:string -> thread -> unit
val terminate : thread -> unit

type reason = [ `step | `breakpoint | `pause | `entry | `except | `error ]
val stopped :
  reason:reason ->
  ?description:string ->
  ?text:string ->
  ?all:bool ->
  ?preserveFocus:bool ->
  ?breakpoints:int list ->
  thread -> unit

val set_single_thread_support : unit -> unit
