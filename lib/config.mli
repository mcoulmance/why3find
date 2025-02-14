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

(** {1 Configurations and Environments} *)

type config = {
  fast : float ;
  time : float ;
  depth : int ;
  packages : string list ;
  provers : string list ;
  tactics : string list ;
  drivers : string list ;
  warnoff : string list ;
  preprocess : string option ;
  profile : Json.t option ; (* We'll keep it like that for now *)
}
(** The project configuration type *)

val default : config
(** Default configuration *)

val conf_file : string
(** The name of the configuration file *)

val load_raw_config : string -> Json.t
(** Load configuration as a Json object from the given configuration file *)

val config_of_json : Json.t -> config
(** Convert the raw configuration Json object to a config value *)

val config_to_json : config -> Json.t
(** Convert the given config to a raw configuration Json object *)

val load_config : string -> config
(** Read the configuration from the given configuration file *)

val save_config : string -> config -> unit
(** Save the given configuration as a configuration file in the given
    {e directory} *)

type wenv = private {
  config : Why3.Whyconf.config ;
  env : Why3.Env.env ;
}

module WenvWhtbl : Why3.Weakhtbl.S with type key = wenv

val create_wenv : ?loadpath:string list -> Why3.Whyconf.config -> wenv
(** Create a why3 environment from the given why3 config.
    If present, the parameter [loadpath] is added to the loadpath defined
    by the config. *)
