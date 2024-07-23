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

type config = {
  time : float ;
  depth : int ;
  configs : string list ;
  packages : string list ;
  provers : string list ;
  tactics : string list ;
  drivers : string list ;
  warnoff : string list ;
  profile : Json.t option ; (* We'll keep it like that for now *)
}

let conf_file = "why3find.json"

let load_raw_config p =
  let p = Filename.concat p conf_file in
  if Sys.file_exists p
  then Json.of_file p
  else `Null

let get ~of_json ~default name json =
  try of_json (Json.jfield_exn name json) with
  | Not_found -> default

let gets name ?(default=[]) json =
  get ~of_json:Json.jstringlist ~default name json

let config_of_json json = {
  time = get "time" ~of_json:Json.jfloat ~default:1.0 json;
  depth = get "depth" ~of_json:Json.jint ~default:4 json;
  configs = gets "configs" json;
  packages = gets "packages" json;
  provers = gets "provers" json;
  tactics = gets "tactics" ~default:["split_vc"] json;
  drivers = gets "drivers" json;
  warnoff = gets "warnoff" json;
  profile = get ~of_json:Option.some ~default:None "profile" json;
}

let default = config_of_json `Null

let lists fd =
  `List (List.map (fun s -> `String s) fd)

let config_to_json config =
  let fields = [
    "time", `Float config.time ;
    "depth", `Int config.depth ;
    "configs", lists config.configs ;
    "packages", lists config.packages ;
    "provers", lists config.provers ;
    "tactics", lists config.tactics ;
    "drivers", lists config.drivers ;
    "warnoff", lists config.warnoff ;
  ] @ Option.fold ~none:[] ~some:(fun p -> [ "profile", p ]) config.profile in
  `Assoc fields

let load_config p =
  config_of_json @@ load_raw_config p

let save_config p config =
  let p = Filename.concat p conf_file in
  Json.to_file p @@ config_to_json config

type env = {
  config : config ;
  wconfig : Why3.Whyconf.config ;
  wenv : Why3.Env.env ;
  pkgs : Meta.pkg list ;
}

let create_env ?(root=".") ~config () =
  let pkgs = Meta.find_all config.packages in
  let pkg_path = List.map (fun m -> m.Meta.path) pkgs in
  let extra_config = config.configs in
  let wconfig = Why3.Whyconf.init_config ~extra_config None in
  let wmain = Why3.Whyconf.get_main wconfig in
  let wpath = Why3.Whyconf.loadpath wmain in
  let wenv = Why3.Env.create_env (root :: pkg_path @ wpath) in
  { config ; wconfig ; wenv ; pkgs }
