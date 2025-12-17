(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- System Utilities                                                   --- *)
(* -------------------------------------------------------------------------- *)

val mkdirs : string -> unit
val rmpath : string -> unit
val copy : src:string -> tgt:string -> unit
val locate : string -> (string * string) option
val is_local : root:string -> file:string -> bool
val chdir : string -> unit
val absolute : string -> string
val path : ?root:string -> string -> string
(** Leave absolute path unchaned.
    For relative paths, concat to `~root` is provided, otherwize, leave
    unchanged. *)

val load : file:string -> Buffer.t -> unit
val readfile : file:string -> string
val writefile : file:string -> string -> unit
val output_and_close : out_channel -> (out_channel -> unit) -> unit
val outputfile : file:string -> (out_channel -> unit) -> unit
val formatfile : file:string -> (Format.formatter -> unit) -> unit

val input : file:string -> (in_channel -> 'a) -> 'a
val output : file:string -> (out_channel -> 'a) -> 'a

val readdir : (string -> unit) -> string -> unit
val iterpath :
  ?enter:(string -> unit) ->
  ?file:(string -> unit) ->
  ?leave:(string -> unit) ->
  ?ignored:(string -> bool) ->
  string -> unit

val pp_hex : Format.formatter -> string -> unit
val pp_arg : Format.formatter -> string -> unit
val pp_args : Format.formatter -> string list -> unit

val pp_ok : Format.formatter -> unit
val pp_ko : Format.formatter -> unit
val pp_mark : Format.formatter -> bool -> unit
val pp_time : Format.formatter -> float -> unit
val pp_atime : Format.formatter -> float -> unit (* left aligned, positive *)
val pp_rtime : Format.formatter -> float -> unit (* right aligned, positive *)
val pp_delta : Format.formatter -> float -> unit
val pa_time : string -> float (* seconds *)
val pp_s : Format.formatter -> int -> unit
val pp_yies : Format.formatter -> int -> unit

val to_string : (Format.formatter -> 'a -> unit) -> 'a -> string

val round : float -> float

val show_progress_opt : string * Arg.spec * string

val tty : bool
val set_ansi_tags : Format.formatter -> unit

val flush : unit -> unit
val overflows : unit -> bool
val progress : ('a,Format.formatter,unit) format -> 'a
val failwith : ('a,Format.formatter,unit,'b) format4 -> 'a

(* -------------------------------------------------------------------------- *)
