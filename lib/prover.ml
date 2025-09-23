(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Whyconf = Why3.Whyconf
module Driver = Why3.Driver

(* -------------------------------------------------------------------------- *)
(* --- Provers                                                            --- *)
(* -------------------------------------------------------------------------- *)

type desc = {
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
  | Invalid_argument _ | InvalidPattern _ ->
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

module Mdesc = Why3.Extmap.Make (struct
    type t = desc
    let compare = compare_desc
  end)

type prover = {
  desc : desc ;
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
  try
    let (name, _) = split_pattern s in
    name
  with InvalidPattern _ -> s

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

let load (wenv : Config.why3env) (pconfig : Whyconf.config_prover) =
  let main = Whyconf.get_main wenv.config in
  {
    desc = desc_of_config pconfig ;
    config = pconfig ;
    driver = load_driver_for_prover main wenv.env pconfig ;
    digest = digest_config main pconfig;
  }

let all wenv =
  let folder _ c m =
    if c.Whyconf.prover.prover_altern = "" && not c.Whyconf.interactive
    then let p = load wenv c in Mdesc.add p.desc p m
    else m in
  let configs = Whyconf.get_provers wenv.config in
  Whyconf.Mprover.fold folder configs Mdesc.empty

let all = Config.Whtbl.memoize 5 all

let filter_prover ~name ?version p =
  p.desc.name = name
  && (version = None || p.desc.version = Option.get version)

let find_exn wenv ~pattern =
  let (name, version) = split_pattern pattern in
  let filter _ p = filter_prover ~name ?version p in
  snd @@ Mdesc.min_binding @@ Mdesc.filter filter @@ all wenv

let find_default (wenv : Config.why3env) name =
  try [ find_exn wenv ~pattern:name ] with Not_found -> []

let default wenv =
  find_default wenv "alt-ergo" @
  find_default wenv "z3" @
  find_default wenv "cvc4" @
  find_default wenv "cvc5"

let warn_prover_not_found =
  Why3.Wstdlib.Hstr.memo 5
    (fun s -> Log.warning "prover %s not found (why3)" s)

let warn_prover_not_configured =
  Why3.Wstdlib.Hstr.memo 5
    (fun s -> Log.warning "prover %s not configured (project)" s)

let select wenv ~patterns =
  let find pattern =
    try Some (find_exn wenv ~pattern) with
    | InvalidPattern s -> Log.warning "invalid prover pattern %s" s; None
    | Not_found -> warn_prover_not_found pattern; None in
  List.filter_map find patterns

let check_and_get_prover env ~patterns pr =
  let prmatch pattern =
    try pmatch ~pattern pr with
    | InvalidPattern _ -> false in
  let prover = Mdesc.find_opt pr @@ all env in
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
