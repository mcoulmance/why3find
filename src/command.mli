(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Why3 Find Builtin Commands                                         --- *)
(* -------------------------------------------------------------------------- *)

val iter : (string -> string -> unit) -> unit
val process : string -> string array -> unit
val register : name:string -> ?args:string -> (string array -> unit) -> unit

val parse : string array -> (string * Arg.spec * string) list -> Arg.anon_fun -> string -> unit
val noargv : string -> unit

(* -------------------------------------------------------------------------- *)
