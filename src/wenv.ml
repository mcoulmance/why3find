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

(* -------------------------------------------------------------------------- *)
(* --- Why3Find Environment                                               --- *)
(* -------------------------------------------------------------------------- *)

let prefix = ref ""
let chdir = ref ""

let move_to_root () =
  if !chdir <> "" then
    Utils.chdir !chdir
  else
    begin
      match Utils.locate Config.conf_file with
      | Some(dir,path) -> Utils.chdir dir ; prefix := path
      | None -> ()
    end

(* -------------------------------------------------------------------------- *)
(* --- Command Line Options                                               --- *)
(* -------------------------------------------------------------------------- *)

let time = ref None
let depth = ref None
let cfgs = ref []
let drvs = ref []
let pkgs = ref []
let prvs = ref []
let tacs = ref []
let warn = ref []

(* -------------------------------------------------------------------------- *)
(* --- Variable Argument Processing                                       --- *)
(* -------------------------------------------------------------------------- *)

type mode =
  | Set
  | Add
  | Sub
  | Move of int

let parse_arg ~mode s =
  let n = String.length s in
  if n > 1 then
    match s.[0] with
    | '+' -> Add, String.sub s 1 (n-1)
    | '-' -> Sub, String.sub s 1 (n-1)
    | _ ->
      let rec digits k =
        if k < n then
          match s.[k] with
          | ':' -> k
          | '0'..'9' -> digits (succ k)
          | _ -> 0
        else 0 in
      let d = digits 0 in
      if d > 0 then
        let at = int_of_string (String.sub s 0 d) in
        Move at, String.sub s (d+1) (n-d-1)
      else mode, s
  else mode, s

(* principal prover name (without '@version') *)
let name p =
  String.lowercase_ascii @@ List.hd @@ String.split_on_char '@' p

(* equal by principal name *)
let eq_name p q = name p = name q

(* has an '@version' qualifier *)
let precise p = String.contains p '@'

(* append or update an item *)
let rec add a = function
  | [] -> [a]
  | p::ps -> if eq_name a p then a :: ps else p :: add a ps

(* remove an item *)
let rec sub a = function
  | [] -> []
  | p::ps -> if eq_name a p then ps else p :: sub a ps

(* current version of the name, if any *)
let rec current a = function
  | [] -> a
  | p::ps -> if eq_name a p then p else current a ps

(* insert at position, starting at 1 *)
let rec insert ~at a = function
  | [] -> [a]
  | p::ps ->
    if eq_name a p then
      if at <= 1 then a :: sub a ps
      else insert ~at a ps
    else
    if at <= 1 then a :: p :: sub a ps
    else p :: insert ~at:(pred at) a ps

let move ~at a ps =
  insert ~at (if precise a then a else current a ps) ps

let rec process config mode = function
  | [] -> config
  | arg :: args ->
    let m, a = parse_arg ~mode arg in
    if a = "none" then
      match m with
      | Set -> process [] Add args
      | Add | Sub | Move _ -> process config mode args
    else
      match m with
      | Set -> process [ a ] Add args
      | Add -> process (add a config) Add args
      | Sub -> process (sub a config) Sub args
      | Move at -> process (move ~at a config) (Move (succ at)) args

let process_args config args =
  process config Set @@ String.split_on_char ',' args

(* -------------------------------------------------------------------------- *)
(* --- Command Line Arguments                                             --- *)
(* -------------------------------------------------------------------------- *)

let setv r v =
  r := Some v

let getv fd r =
  match !r with
  | Some v -> v
  | None -> fd

let add r a =
  r := a :: !r

let gets fd ropt =
  List.fold_left process_args fd !ropt

type opt = [
  | `All
  | `Package
  | `Prover
  | `Driver
]

let settime s =
  try setv time (Utils.pa_time s)
  with Invalid_argument _ -> Utils.failwith "Invalid time (-t %s)" s

let alloptions : (opt * string * Arg.spec * string) list = [
  `All, "--root", Arg.Set_string chdir, "DIR change to directory";
  `All, "--extra-config", Arg.String (add cfgs), "CFG extra why3 config";
  `All, "--why3-warn-off", Arg.String (add warn), "WRN,… disable why3 warnings";
  `Package, "--package", Arg.String (add pkgs), "±PKG,… add package dependency";
  `Prover,  "--time", Arg.String settime, "TIME median proof time";
  `Prover,  "--depth", Arg.Int (setv depth), "DEPTH proof search limit";
  `Prover,  "--prover", Arg.String (add prvs), "±PRV,… configure provers";
  `Prover,  "--tactic", Arg.String (add tacs), "±TAC,… configure tactics";
  `Driver,  "--driver", Arg.String (add drvs), "±DRV,… configure drivers";
  `Package, "-p", Arg.String (add pkgs), " same as --package";
  `Prover,  "-t", Arg.String settime, " same as --time";
  `Prover,  "-d", Arg.Int (setv depth), " same as --depth";
  `Prover,  "-P", Arg.String (add prvs), " same as --prover";
  `Prover,  "-T", Arg.String (add tacs), " same as --tactic";
  `Driver,  "-D", Arg.String (add drvs), " same as --driver";
]

let options ?(packages=false) ?(provers=false) ?(drivers=false) () =
  let default = not packages && not provers && not drivers in
  List.filter_map
    (fun (opt,name,spec,descr) ->
       if default ||
          match opt with
          | `All -> true
          | `Package -> packages
          | `Prover -> provers
          | `Driver -> drivers
       then Some(name,spec,descr)
       else None
    ) alloptions

let time config = getv config.Config.time time
let depth config = getv config.Config.depth depth
let configs config = gets config.Config.configs cfgs
let packages config = gets config.Config.packages pkgs
let provers config = gets config.Config.provers prvs
let tactics config = gets config.Config.tactics tacs
let drivers config = gets config.Config.drivers drvs
let warnoff config = gets config.Config.warnoff warn

let exts () =
  let formats = Why3.Env.list_formats Why3.Env.base_language in
  let exts = List.concat_map (fun (_, exts, _) -> exts) formats in
  List.map (fun ext -> "." ^ ext) exts

let arg0 file =
  if Filename.is_relative file then Filename.concat !prefix file else file

let arg1 file = move_to_root () ; arg0 file
let argv files = move_to_root () ; List.map arg1 files

let filter ~exts p = List.mem (Filename.extension p) exts

let ignored p =
  match String.get (Filename.basename p) 0 with
  | 'a'..'z' | 'A'..'Z' | '0'..'9' -> false
  | _ -> true

let allfiles ?(exts=exts ()) f path =
  if not (Sys.file_exists path) then
    Utils.failwith "Unknown file or directory %S" path ;
  if not (filter ~exts path || Sys.is_directory path) then
    Utils.failwith "File %S is neither a Why3 file nor a directory" path ;
  Utils.iterpath
    ~file:(fun p -> if filter ~exts p then f p)
    ~ignored:(fun p -> ignored p && p <> path)
    path

let argfiles ?exts files = move_to_root () ;
  let paths = ref [] in
  List.iter
    (fun f -> allfiles ?exts (fun p -> paths := p :: !paths) (arg1 f))
    files ;
  List.rev !paths

(* -------------------------------------------------------------------------- *)
(* --- Saving Project Config                                              --- *)
(* -------------------------------------------------------------------------- *)

let save conf =
  let path = Sys.getcwd () in
  Config.save_config path conf ;
  Format.printf "Why3find config saved to %s@."
    (Filename.concat path Config.conf_file)

(* -------------------------------------------------------------------------- *)
(* --- Why3 Environment                                                   --- *)
(* -------------------------------------------------------------------------- *)

let default = Config.default

let load_raw_config () =
  move_to_root ();
  Config.load_raw_config "."

let add_options config =
  Config.{
    time = time config ;
    depth = depth config ;
    configs = configs config ;
    packages = packages config ;
    provers = provers config  ;
    tactics = tactics config ;
    drivers = drivers config ;
    warnoff = warnoff config ;
    profile = config.profile ;
  }

let load_config () =
  add_options @@ Config.config_of_json @@ load_raw_config ()

type env = Config.env = private {
  config : Config.config ;
  wconfig : Why3.Whyconf.config ;
  wenv : Why3.Env.env ;
  pkgs : Meta.pkg list ;
}

(* TODO: fix this ugly workaround one way or another *)
let disable_warning s =
  Why3.Loc.disable_warning @@
  Why3.Loc.register_warning s Why3.Pp.empty_formatted

let init ?root config =
  begin
    let wenv = Config.create_env ?root ~config () in
    Why3.Whyconf.load_plugins @@ Why3.Whyconf.get_main wenv.wconfig;
    Debug.init_why3_debug ();
    List.iter disable_warning config.warnoff;
    wenv
  end

(* -------------------------------------------------------------------------- *)
