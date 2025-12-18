(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Why3find Configuration                                             --- *)
(* -------------------------------------------------------------------------- *)

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

let conf_file = "why3find.json"
let root_for file =
  let rec lookup = function
    | "/" | "." -> None
    | dir ->
      let config = Filename.concat dir "why3find.json" in
      if Sys.file_exists config
      then Some dir
      else lookup (Filename.dirname dir)
  in lookup (Filename.dirname file)

let load_raw_config ~root =
  let path = Filename.concat root conf_file in
  if Sys.file_exists path then Json.of_file path else `Null

let get ~of_json ~default name json =
  try of_json (Json.jfield_exn name json) with
  | Not_found -> default

let gets name ?(default=[]) json =
  get ~of_json:Json.jstringlist ~default name json

let config_of_json json =
  if gets "configs" json <> [] then
    Log.warning "Support for extra config files was removed in why3find 1.1.0";
  {
    fast = get "fast" ~of_json:Json.jfloat ~default:0.2 json;
    time = get "time" ~of_json:Json.jfloat ~default:1.0 json;
    depth = get "depth" ~of_json:Json.jint ~default:4 json;
    packages = gets "packages" json;
    provers = gets "provers" json;
    tactics = gets "tactics" ~default:["split_vc"] json;
    drivers = gets "drivers" json;
    warnoff = gets "warnoff" json;
    preprocess = Json.(joption jstring @@ jfield "preprocess" json);
    profile = get ~of_json:Option.some ~default:None "profile" json;
  }

let default = config_of_json `Null

let lists fd =
  `List (List.map (fun s -> `String s) fd)

let config_to_json config =
  let fields = [
    "fast", `Float config.fast ;
    "time", `Float config.time ;
    "depth", `Int config.depth ;
    "packages", lists config.packages ;
    "provers", lists config.provers ;
    "tactics", lists config.tactics ;
    "drivers", lists config.drivers ;
    "warnoff", lists config.warnoff ;
    "preprocess", Json.option_map Json.string config.preprocess ;
    "profile", Json.option_map Fun.id config.profile
  ] in
  Json.assoc ~keepnull:false fields

let load_config ~root =
  config_of_json @@ load_raw_config ~root

let save_config ~root config =
  let path = Filename.concat root conf_file in
  Json.to_file ~pretty_floats:true path @@ config_to_json config

(* -------------------------------------------------------------------------- *)
(* --- Why3 Environments                                                  --- *)
(* -------------------------------------------------------------------------- *)

module WEnv = Why3.Env
module WConf = Why3.Whyconf

type why3env = {
  config : WConf.config ;
  env : WEnv.env ;
}

module Whtbl = Why3.Weakhtbl.Make (struct
    type t = why3env
    let tag w = WEnv.env_tag w.env
    (* [LC] a new tag is generated for every why3 environment.
       Hence it is sufficient to identify both the Whyconf and the Env. *)
  end)

let why3env ?(loadpath=[]) config =
  let loadpath = loadpath @ WConf.(loadpath @@ get_main config) in
  let env = WEnv.create_env loadpath in
  { config ; env }
