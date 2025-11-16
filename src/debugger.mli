(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val files : string list ref
val goals : string list ref
val running : Dap.adapter -> bool
val process : Dap.adapter -> bool
val run : Dap.adapter -> unit
