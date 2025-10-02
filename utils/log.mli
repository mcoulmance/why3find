(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val pp_stdout : ((Format.formatter -> unit) -> unit) ref
val pp_stderr : ((Format.formatter -> unit) -> unit) ref

type level = [ `Message | `Warning | `Error ]

val set_log : bool -> unit
val emit : ?level:level -> ('a, Format.formatter, unit) format -> 'a
val message : ('a, Format.formatter, unit) format -> 'a
val warning : ('a, Format.formatter, unit) format -> 'a
val error : ('a, Format.formatter, unit) format -> 'a
