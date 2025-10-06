(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val exit : int ref
val init : string list -> string list -> unit
val running : Dap.adapter -> bool
val process : Dap.adapter -> bool
val run : Dap.adapter -> unit
