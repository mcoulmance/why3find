(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Debugging mechanism *)

type flag
(** The type of degugging flags *)

val create_flag : ?callback:(unit -> unit) -> string -> flag
(** Create a debugging flag from the given name. If given, callback will be
    called on flag activation. *)

val default : flag
(** Default debugging flag, activated whenever any other flag is activated *)

val parse_and_set_flags : string -> unit
(** Take a list of comma separated flag names and activate the corresponding
    flags. *)

val init_why3_debug : unit -> unit
(** Call this after plugins loading so that why3 debug flags get activated *)

val options : (string * Stdlib.Arg.spec * string) list
(** The Arg debug options specs *)

val debug : ?flag:flag -> ('a, Format.formatter, unit) format -> 'a
(** Print a debugging message when the given [flag] is activated. [flag]
    default to [default] when omitted *)
