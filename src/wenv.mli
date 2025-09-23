(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Why3 Environment                                                   --- *)
(* -------------------------------------------------------------------------- *)

val options :
  ?packages:bool ->
  ?provers:bool ->
  ?drivers:bool ->
  unit -> (string * Arg.spec * string) list

val arg1 : string -> string
val argv : string list -> string list
val argfiles : ?exts:string list -> string list -> string list
val allfiles : ?exts:string list -> (string -> unit) -> string -> unit

val default : Config.config

val load_raw_config : unit -> Json.t

val add_options : Config.config -> Config.config

val load_config : unit -> Config.config

val save : Config.config -> unit

val init : ?root:string -> ?loadplugins:bool -> Config.config -> Project.env

val update_why3_config : Why3.Whyconf.config -> unit
(** Update [~/.why3conf] with the specified number of jobs, if modified. *)
