(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module WConf = Why3.Whyconf

type env = {
  root : string ;
  cache : Cache.t ;
  config : Config.config ;
  why3 : Config.why3env ;
  pkgs : Meta.pkg list ;
}

let to_rc provers =
  let to_section (path, name, version) =
    let s = Why3.Rc.empty_section in
    let s = Why3.Rc.set_string s "name" name in
    let s = Why3.Rc.set_string s "path" path in
    let s = Why3.Rc.set_string s "version" version in
    s in
  let sections = List.map to_section provers in
  Why3.Rc.set_simple_family Why3.Rc.empty "partial_prover" sections

(* brittle but the best we can do with the current API *)
let add_autodetected_provers =
  Timer.timed ~name:"Prover autodetection" (fun config ->
      let data = Why3.Autodetection.read_auto_detection_data config in
      let provers = Why3.Autodetection.find_provers data in
      !WConf.provers_from_detected_provers config (to_rc provers)
    )

let why3_config autodetect =
  let config = WConf.init_config ~extra_config:[] None in
  if autodetect then
    add_autodetected_provers config
  else
    config

let create ?(root=".") ?(usecache=true) ?(config=Config.default)
    ?(autodetect_provers=true) () =
  let pkgs = Meta.find_all config.Config.packages in
  let pkg_path = List.map (fun m -> m.Meta.path) pkgs in
  let wconfig = why3_config autodetect_provers in
  let why3 = Config.why3env ~loadpath:(root :: pkg_path) wconfig in
  let cdir = Filename.concat root ".why3find" in
  let cache = Cache.create ~cdir ~usecache () in
  { root; config ; cache ; why3 ; pkgs }
