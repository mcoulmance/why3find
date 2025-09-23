(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Why3find Environment                                               --- *)
(* -------------------------------------------------------------------------- *)

type env = private {
  root : string ;
  cache : Cache.t ;
  config : Config.config ;
  why3 : Config.why3env ;
  pkgs : Meta.pkg list ;
}

val create : ?root:string -> ?usecache:bool -> ?config:Config.config -> unit -> env
(** Create an environment with configuration [config] for the project
    located in [root] (which default to ".") *)
