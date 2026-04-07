(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Refs for Documentation                                             --- *)
(* -------------------------------------------------------------------------- *)

module Sid = Why3.Ident.Sid
module Hid = Why3.Ident.Hid
module Thy = Why3.Theory
module Pmod = Why3.Pmodule
module Mstr = Why3.Wstdlib.Mstr

(* -------------------------------------------------------------------------- *)
(* --- Keywords                                                           --- *)
(* -------------------------------------------------------------------------- *)

let keywords = Hashtbl.create 97
let is_keyword = Hashtbl.mem keywords

let () = List.iter (fun k -> Hashtbl.add keywords k ()) Why3.Keywords.keywords

let is_opening = function
  | "scope" | "match" | "try" | "begin" -> true
  | _ -> false

let is_closing = function
  | "end" -> true
  | _ -> false

(* Reference Parsing *)

let is_uppercased s =
  String.length s > 0 &&
  let c = s.[0] in 'A' <= c && c <= 'Z'

(* -------------------------------------------------------------------------- *)
(* --- Global References                                                  --- *)
(* -------------------------------------------------------------------------- *)

type position = Lexing.position * Lexing.position

type clone =
  | Inst of {
      order : int ; (* instance order *)
      cloned : Thy.theory ;
    }
  | Clone of {
      order : int ; (* declaration order *)
      source : Id.t ; (* from cloned theory *)
      target : Id.t ; (* declared in clone instance *)
    }

type instance = Instance of {
    order : int ; (* clone rank in the module *)
    container : string ; (* container module (or theory) *)
    scope : string list ; (* inside container module clone *)
    cloned : Thy.theory ; (* cloned theory *)
    clones : clone list ; (* clone substitution *)
  }

type instances = instance Mstr.t Hid.t

type theory = {
  path: string;
  theory: Thy.theory ;
  depends: Thy.theory list ;
  signature: Axioms.signature ;
  instances: instances ;
  iclones: clone list ; (* cloned instances (only [Inst ]) *)
  mutable proofs: Crc.crc Mstr.t ;
}

type source = {
  root: string option;
  filename: string;
  lib: string list;
  urlbase: string;
  profile: Calibration.profile ;
  theories: theory Mstr.t;
}

let extract ~infix position =
  let loc = Why3.Loc.extract position in
  if infix then
    let (f,p,s,q,e) = Why3.Loc.get loc in
    Why3.Loc.user_position f p (succ s) q (pred e)
  else loc

type href =
  | NoRef
  | Ref of Id.id
  | Def of Id.id * Crc.crc option

let find_proof id = function
  | None -> None
  | Some { proofs } -> Mstr.find_opt (Vc.id_name id) proofs

let strip_lemma id = function
  | None -> id
  | Some { theory } ->
    if not @@ Id.lemma id then id else
      begin
        let exception Found of Id.t in
        try
          List.iter
            (fun (td : Thy.tdecl) ->
               match td.td_node with
               | Use _ | Clone _ | Meta _ -> ()
               | Decl d ->
                 match d.d_node with
                 | Dprop(_, { pr_name = ax },_) ->
                   if ax.id_string ^ "'lemma" = id.id_string then
                     raise (Found ax)
                 | _ -> ()
            ) theory.th_decls ;
          id
        with Found id -> id
      end

let resolve ~src ~theory ~infix pos =
  try
    let loc = extract ~infix pos in
    let root = src.root in
    match Why3.Glob.find loc with
    | (id, Why3.Glob.Def, _) ->
      let id = strip_lemma id theory in
      let proof = find_proof id theory in
      Def (Id.resolve ~lib:src.lib ?root id, proof)
    | (id, Why3.Glob.Use, _) ->
      let id = strip_lemma id theory in
      Ref (Id.resolve ~lib:src.lib ?root id)
  with Not_found -> NoRef

(* -------------------------------------------------------------------------- *)
(* --- Clone Environment                                                  --- *)
(* -------------------------------------------------------------------------- *)

type cenv = {
  from : string ; (* container module or theory *)
  mutable order : int ;
  mutable used : Thy.theory list ;
  mutable clones : clone list ; (* i-clones *)
  ranks : int Hid.t ;
  cloned : instances ;
}

let is_ident a = function
  | None -> true
  | Some b -> Why3.Ident.id_equal a b

let is_clone ?source ?target = function
  | Inst _ -> false
  | Clone c -> is_ident c.source source && is_ident c.target target

let find_clone ?instance ?source ?target () =
  match instance with
  | None -> None
  | Some (Instance ci) -> List.find_opt (is_clone ?source ?target) ci.clones

(* -------------------------------------------------------------------------- *)
(* --- Instances                                                          --- *)
(* -------------------------------------------------------------------------- *)

let skey scope = String.concat "<" scope

let has_instance index ~scope id =
  try Mstr.mem (skey scope) @@ Hid.find index id
  with Not_found -> false

let find_instance index ~scope id =
  try Some (Mstr.find (skey scope) @@ Hid.find index id)
  with Not_found -> None

let register_instance index ~scope id s =
  let m = Hid.find_def index Mstr.empty id in
  let m = Mstr.add (skey scope) s m in
  Hid.replace index id m

let iter_instances fn index =
  Hid.iter (fun _id m -> Mstr.iter (fun _s inst -> fn inst) m) index

(* -------------------------------------------------------------------------- *)
(* --- Theory Iterators                                                   --- *)
(* -------------------------------------------------------------------------- *)

let order = function Inst { order } | Clone { order } -> order
let by_order a b = order a - order b
let next cenv = let order = succ cenv.order in cenv.order <- order ; order

let add_decl cenv (id : Id.t) = Hid.add cenv.ranks id @@ next cenv

let add_instance cenv scope (cloned : Thy.theory) clones =
  let order = next cenv in
  let clones = List.sort by_order @@ List.rev clones in
  let inst = Instance {
      container = cenv.from ; order ; scope ; cloned ; clones
    } in
  if not @@ has_instance cenv.cloned ~scope cloned.th_name then
    register_instance cenv.cloned ~scope cloned.th_name inst ;
  cenv.clones <- Inst { order ; cloned } :: cenv.clones

let add_clone cenv locals clones a b =
  if Sid.mem b locals && not @@ Id.lemma b then
    let order = Hid.find_def cenv.ranks 0 b in
    clones := Clone { order ; source = a ; target = b } :: !clones

let add_used cenv thy = cenv.used <- thy :: cenv.used

let iter_module cenv ~scope (m : Why3.Pmodule.pmodule) =
  let open Why3.Pmodule in
  let rec walk scope = function
    | Uscope(a,mus) -> List.iter (walk @@ a::scope) mus
    | Uclone mi ->
      let thy = mi.mi_mod.mod_theory in
      add_used cenv thy ;
      let clones = ref [] in
      Wutil.iter_mi (add_clone cenv m.mod_local clones) mi ;
      add_instance cenv scope thy !clones
    | Uuse m -> add_used cenv m.mod_theory
    | Udecl d -> Wutil.iter_pdecl (add_decl cenv) d
    | Umeta _ -> ()
  in List.iter (walk scope) m.mod_units

let iter_theory cenv ~scope thy =
  try
    iter_module cenv ~scope @@ Why3.Pmodule.restore_module thy
  with Not_found ->
    List.iter
      (fun d ->
         match d.Thy.td_node with
         | Use th ->
           add_used cenv th
         | Clone(th,sm) ->
           add_used cenv th ;
           let clones = ref [] in
           Wutil.iter_sm (add_clone cenv th.th_local clones) sm ;
           add_instance cenv scope th !clones
         | Thy.Decl d -> Wutil.iter_decl (add_decl cenv) d
         | Thy.Meta _ -> ()
      ) thy.th_decls

(* -------------------------------------------------------------------------- *)
(* --- Proofs                                                             --- *)
(* -------------------------------------------------------------------------- *)

let get_proofs thy proofs =
  Mstr.find_def Mstr.empty thy.Thy.th_name.id_string proofs

let get_proof a proofs = Mstr.find_def Crc.stuck a proofs

let zip_goals thy proofs =
  let proofs = get_proofs thy proofs in
  List.fold_left
    (fun cmap task ->
       let a = Vc.task_name task in
       Mstr.add a (get_proof a proofs) cmap)
    Mstr.empty
    (Why3.Task.split_theory thy None None)

let reload_proofs source =
  let _, proofs = Proofs.load source.filename in
  Mstr.iter
    (fun _ (th : theory) ->
       let proofs = get_proofs th.theory proofs in
       th.proofs <- Mstr.mapi (fun a _ -> get_proof a proofs) th.proofs
    ) source.theories

(* -------------------------------------------------------------------------- *)
(* --- Parsing                                                            --- *)
(* -------------------------------------------------------------------------- *)

let library_path file =
  let rec scan r p =
    let d = Filename.dirname p in
    if d = "." || d = "" || d = p
    then p::r
    else scan (Filename.basename p :: r) d
  in
  if Filename.is_relative file then
    scan [] (Filename.chop_extension file)
  else Filename.[chop_extension @@ basename file]

let derived src id =
  Printf.sprintf "%s.%s.html" (String.concat "." src.lib) id

let empty () = {
  filename = "" ;
  lib = [] ; urlbase = "" ; root = None ;
  profile = Calibration.create () ;
  theories = Mstr.empty ;
}

let source ~wenv ~henv ~lib ?root ~filename inc =
  let globals = Why3.Debug.test_flag Why3.Glob.flag in
  try
    if not globals then Why3.Debug.set_flag Why3.Glob.flag ;
    Why3.Glob.clear filename ;
    let path = String.concat "." lib in
    let thys = Why3.Env.read_channel Why3.Env.base_language wenv filename inc in
    let profile, proofs = Proofs.load filename in
    let profile = Calibration.of_json profile in
    let theories =
      Mstr.mapi
        (fun name (theory : Thy.theory) ->
           let cenv = {
             from = Printf.sprintf "%s.%s" path name ;
             cloned = Hid.create 0 ;
             ranks = Hid.create 0 ;
             order = 0 ; used = [] ; clones = [] ;
           } in
           iter_theory ~scope:[] cenv theory ;
           let proofs = zip_goals theory proofs in
           let signature = Axioms.signature henv theory in
           let path = Id.fullname ~lib theory.th_name in
           {
             path ; theory ; signature ; proofs ;
             depends = List.rev cenv.used ;
             instances = cenv.cloned ;
             iclones = List.sort by_order @@ List.rev cenv.clones ;
           }
        ) thys
    in
    if not globals then Why3.Debug.unset_flag Why3.Glob.flag ;
    { filename ; lib ; root ; urlbase = path ; profile ; theories }
  with exn ->
    if not globals then Why3.Debug.unset_flag Why3.Glob.flag ;
    raise exn

let parse ~wenv ~henv file =
  if not @@ String.ends_with ~suffix:".mlw" file then
    begin
      Log.error "invalid file name: %S" file ;
      exit 2
    end ;
  let lib = library_path file in
  try
    let inc = open_in file in
    let src = source ~wenv ~henv ~lib ~filename:file inc in
    close_in inc ; src
  with exn ->
    Log.error "%s" (Printexc.to_string exn) ;
    exit 1

(* -------------------------------------------------------------------------- *)
(* --- Global References                                                  --- *)
(* -------------------------------------------------------------------------- *)

(* Theory lookup *)

let ns_find_ts ns qid =
  try [(Thy.ns_find_ts ns qid).ts_name] with Not_found -> []

let ns_find_ls ns qid =
  try [(Thy.ns_find_ls ns qid).ls_name] with Not_found -> []

let ns_find_pr ns qid =
  try [(Thy.ns_find_pr ns qid).pr_name] with Not_found -> []

let ns_find ns kind qid =
  match kind with
  | 't' -> ns_find_ts ns qid
  | 'l' -> ns_find_ls ns qid
  | 'p' -> ns_find_pr ns qid
  | 'v' -> []
  | 'e' -> []
  | '?' ->
    List.concat [
      ns_find_ts ns qid ;
      ns_find_ls ns qid ;
      ns_find_pr ns qid ;
    ]
  | _ ->
    Utils.failwith
      "invalid reference kind '%c' (use 't', 'l', 'v', 'e' or 'p')" kind

(* Module lookup *)

let pns_find_ts ns tns qid =
  try [(Why3.Pmodule.ns_find_its ns qid).its_ts.ts_name]
  with Not_found -> ns_find_ts tns qid

let pns_find_rs ns tns qid =
  try [(Why3.Pmodule.ns_find_rs ns qid).rs_name]
  with Not_found -> ns_find_ls tns qid

let pns_find_xs ns qid =
  try [(Why3.Pmodule.ns_find_xs ns qid).xs_name] with Not_found -> []

let pns_find pm kind qid =
  let ns = pm.Why3.Pmodule.mod_export in
  let tns = pm.mod_theory.th_export in
  match kind with
  | 't' -> pns_find_ts ns tns qid
  | 'v' -> pns_find_rs ns tns qid
  | 'e' -> pns_find_xs ns qid
  | 'l' -> ns_find_ls tns qid
  | 'p' -> ns_find_pr tns qid
  | '?' ->
    List.concat [
      pns_find_ts ns tns qid ;
      pns_find_rs ns tns qid ;
      pns_find_xs ns qid ;
      ns_find_ls tns qid ;
      ns_find_pr tns qid ;
    ]
  | _ -> Utils.failwith
           "invalid reference kind '%c' (use 't', 'l', 'v', 'e' or 'p')" kind

(* Reference Lookup *)

let find kind qid thy =
  try pns_find (Why3.Pmodule.restore_module thy) kind qid
  with Not_found -> ns_find thy.Thy.th_export kind qid

let find_theory kind qid { theory } = find kind qid theory

let find_decl wenv (src:source) (id : Id.id) =
  let thy =
    if id.id_lib = src.lib then
      (Mstr.find id.id_mod src.theories).theory
    else
      Why3.Env.read_theory wenv id.id_lib id.id_mod
  in Id.find thy id.self

let lookup ~scope ~theories kind m qid =
  match if m <> "" then Some m else scope with
  | Some m0 ->
    begin
      try
        let thy = Mstr.find m0 theories in
        if qid = [] then [thy.theory.th_name] else
          find_theory kind qid thy
      with Not_found -> []
    end
  | None ->
    if qid = [] then [] else
      List.concat @@ List.map (find_theory kind qid) (Mstr.values theories)

let select ~name ids =
  let ids = List.sort_uniq Id.compare ids in
  match ids with
  | [id] -> id
  | [] -> Utils.failwith "reference '%s' not found" name
  | _ -> Utils.failwith "ambiguous reference '%s'" name

(* Global reference resolution *)

let reference ~wenv ~src ~current r =
  let kind,name =
    let n = String.length r in
    if n >= 2 && r.[1] = ':'
    then r.[0],String.sub r 2 (n-2)
    else '?',r in
  name,
  let lp,m,qid =
    let rec split rp = function
      | [] -> [], "", List.rev_map Id.to_infix rp
      | p::ps ->
        if is_uppercased p then List.rev rp,p,List.map Id.to_infix ps
        else split (p::rp) ps
    in split [] (String.split_on_char '.' name) in
  if lp = [] then
    select ~name @@ lookup ~scope:current ~theories:src.theories kind m qid
  else
    let thy =
      try Why3.Env.read_theory wenv lp m
      with Not_found ->
        Utils.failwith "unknown theory or module '%s.%s'"
          (String.concat "." lp) m
    in
    if qid = [] then
      thy.th_name
    else
      select ~name @@ find kind qid thy

(* -------------------------------------------------------------------------- *)
