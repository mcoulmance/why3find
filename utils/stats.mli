(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type stats

val create : unit -> stats

val add : stats -> float -> unit

val min : stats -> float

val max : stats -> float

val sum : stats -> float

val count : stats -> int

val average : stats -> float
