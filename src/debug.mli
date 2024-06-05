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

type label
(** The type of degugging labels *)

val create_label : string -> label
(** Create a debugging label from the given name *)

val default : label
(** Default debugging label, activated whenever any other label is activated *)

val parse_and_set_labels : string -> unit
(** Take a list of comma separated label names and activate the corresponding
    labels. A label name prefixed with "why3:" activates the corresponding
    debugging flag in Why3. The label "why3" activates all "info flags" of
    why3. *)

val option : string * Stdlib.Arg.spec * string
(** The Stdlib.Arg --debug option spec *)

val debug : ?label:label -> ('a, Format.formatter, unit) format -> 'a
(** Print a debugging message when the given [label] is activated. [label]
    default to [default] when omitted *)
