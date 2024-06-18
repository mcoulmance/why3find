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

(** Debugging mechanism *)

type flag
(** The type of degugging flags *)

val create_flag : string -> flag
(** Create a debugging flag from the given name *)

val default : flag
(** Default debugging flag, activated whenever any other flag is activated *)

val parse_and_set_flags : string -> unit
(** Take a list of comma separated flag names and activate the corresponding
    flags. A flag name prefixed with "why3:" activates the corresponding
    debugging flag in Why3. The flag "why3" activates all "info flags" of
    why3. *)

val options : (string * Stdlib.Arg.spec * string) list
(** The Arg debug options specs *)

val debug : ?flag:flag -> ('a, Format.formatter, unit) format -> 'a
(** Print a debugging message when the given [flag] is activated. [flag]
    default to [default] when omitted *)
