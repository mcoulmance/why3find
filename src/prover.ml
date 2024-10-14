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

open Why3

(* -------------------------------------------------------------------------- *)
(* --- Provers                                                            --- *)
(* -------------------------------------------------------------------------- *)

type prover_desc = {
  name : string ;
  version : string ;
}

exception InvalidPattern of string
exception InvalidProverDescription of string

let split_pattern s =
  match String.split_on_char '@' s with
  | [] -> assert false
  | [ name ] -> String.lowercase_ascii name, None
  | [ name ; version ] -> String.lowercase_ascii name, Some version
  | _  -> raise (InvalidPattern s)

let desc_to_string p =
  Format.sprintf "%s@%s" p.name p.version

let desc_of_string s =
  try
    let (name, version) = split_pattern s in
    { name ; version = Option.get version }
  with
  | Invalid_argument _ | InvalidPattern _->
    raise (InvalidProverDescription s)

let desc_name p = p.name
let desc_version p = p.version

let pp_desc fmt p = Format.fprintf fmt "%s@%s" p.name p.version

type sem = V of int | S of string
let sem s = try V (int_of_string s) with Failure _ -> S s
let cmp x y =
  match x,y with
  | V a,V b -> b - a
  | V _,S _ -> (-1)
  | S _,V _ -> (+1)
  | S a,S b -> String.compare a b
let cmp x y = cmp (sem x) (sem y)

let compare_version p q =
  List.compare cmp (String.split_on_char '.' p) (String.split_on_char '.' q)

let compare_desc p q =
  let c = String.compare p.name q.name in
  if c <> 0 then c else
    compare_version p.version q.version

module Mdesc = Extmap.Make (struct
    type t = prover_desc
    let compare = compare_desc
  end)

type prover = {
  desc : prover_desc ;
  config : Whyconf.config_prover ;
  driver : Driver.driver ;
  digest : string ;
}

let compare_prover (p : prover) (q : prover) =
  compare_desc p.desc q.desc
[@@warning "-32"]

let why3_desc prv = Whyconf.prover_parseable_format prv.config.prover
let name prv = prv.desc.name
let version prv = prv.desc.version
let fullname p = Format.sprintf "%s@%s" (name p) (version p)
let infoname p = Format.sprintf "%s(%s)" (name p) (version p)
let pp_prover fmt p = Format.fprintf fmt "%s@%s" (name p) (version p)

(* Digest the file that why3 will load *)
let digest_driver main (dir, name) =
  let file = Driver.resolve_driver_name main "drivers" ~extra_dir:dir name in
  Digest.file file

let digest_config main Whyconf.{
    prover ;
    command ;
    command_steps ;
    driver ;
    in_place = _ ;
    editor = _ ;
    interactive = _ ;
    extra_options ;
    extra_drivers ;
  } =
  let open Buffer in
  let buf = create 128 in
  add_string buf (Digest.string prover.prover_name);
  add_string buf (Digest.string prover.prover_version);
  add_string buf (Digest.string prover.prover_altern);
  add_string buf (Digest.string command);
  if command_steps = None then
    add_string buf "None                            "
  else
    add_string buf (Digest.string (Option.get command_steps));
  add_string buf (digest_driver main driver);
  List.iter (fun s -> add_string buf (Digest.string s))
    extra_options;
  List.iter (fun (d, l) ->
      List.iter (fun f -> add_string buf (digest_driver main (Some d, f))) l
    ) extra_drivers;
  Digest.(to_hex (string (Buffer.contents buf)))
[@@ocaml.warning "+9"]

let pmatch ~pattern d =
  let (name, version) = split_pattern pattern in
  name = d.name && (version = None || Option.get version = d.version)

let pattern_name s =
  let (name, _) = split_pattern s in
  name

let pattern_version s =
  let (_, version) = split_pattern s in
  version

let desc_of_config config = {
  name = String.lowercase_ascii config.Whyconf.prover.prover_name ;
  version = config.Whyconf.prover.prover_version ;
}

let digest_config = Timer.timed2 ~name:"Digest prover" digest_config

let load_driver_for_prover =
  Timer.timed3 ~name:"Load driver" Driver.load_driver_for_prover

let load wconfig wenv (pconfig : Whyconf.config_prover) =
  try
    let main = Whyconf.get_main wconfig in {
      desc = desc_of_config pconfig ;
      config = pconfig ;
      driver = load_driver_for_prover main wenv pconfig ;
      digest = digest_config main pconfig;
    }
  with _ ->
    Log.error "failed to load driver for %s"
      (Whyconf.prover_parseable_format pconfig.prover) ;
    exit 2

let all wconfig wenv =
  let folder _ c m =
    if c.Whyconf.prover.prover_altern = "" && not c.Whyconf.interactive
    then let p = load wconfig wenv c in Mdesc.add p.desc p m
    else m in
  let configs = Whyconf.get_provers wconfig in
  Whyconf.Mprover.fold folder configs Mdesc.empty

let filter_prover ~name ?version p =
  p.desc.name = name
  && (version = None || p.desc.version = Option.get version)

let find_exn ~pattern all =
  let (name, version) = split_pattern pattern in
  let filter _ p = filter_prover ~name ?version p in
  snd @@ Mdesc.min_binding @@ Mdesc.filter filter all

let find_default (env : Why3find.Config.env) name =
  try [ find_exn ~pattern:name @@ all env.wconfig env.wenv ] with Not_found -> []

let default env =
  find_default env "alt-ergo" @
  find_default env "z3" @
  find_default env "cvc4" @
  find_default env "cvc5"

let warn_prover_not_found =
  Why3.Wstdlib.Hstr.memo 5 (Log.warning "prover %s not found (why3)")

let warn_prover_not_configured =
  Why3.Wstdlib.Hstr.memo 5 (Log.warning "prover %s not configured (project)")

let select ~patterns all =
  let find pattern =
    try Some (find_exn ~pattern all) with
    | InvalidPattern s -> Log.warning "invalid prover pattern %s" s; None
    | Not_found -> warn_prover_not_found pattern; None in
  List.filter_map find patterns

let check_and_get_prover ~patterns all pr =
  let prmatch pattern =
    try pmatch ~pattern pr with
    | InvalidPattern _ -> false in
  let prover = Mdesc.find_opt pr all in
  if List.exists prmatch patterns
  then prover (* either the prover is available or we already complained *)
  else
    begin
      let s = Format.asprintf "%a@?" pp_desc pr in
      if prover = None then
        warn_prover_not_found s;
      warn_prover_not_configured s;
      None
    end
