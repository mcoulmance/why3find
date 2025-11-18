(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Why3 Identifiers                                                   --- *)
(* -------------------------------------------------------------------------- *)

module I = Why3.Ident
module Mstr = Why3.Wstdlib.Mstr

type t = I.ident
let hash = I.id_hash
let equal = I.id_equal
let compare = I.id_compare
let pp fmt (id : t) =
  Format.fprintf fmt "%s<%d>" id.id_string (Why3.Weakhtbl.tag_hash id.id_tag)

(* Location *)

let loc (id : t) =
  match id.id_loc with
  | None -> raise Not_found
  | Some loc -> loc

let line id =
  let _,line,_,_,_ = Why3.Loc.get (loc id) in line

let file id =
  let file,_,_,_,_ = Why3.Loc.get (loc id) in file

let path ?lib id =
  let path =
    try Why3.Pmodule.restore_path id
    with Not_found -> Why3.Theory.restore_path id
  in match lib with
  | None -> path
  | Some lib ->
    let lp,md,qid = path in
    if lp = [] then lib,md,qid else path

let cat = String.concat "."

(* Ident Absolute Name *)

type package = [ `Local | `Stdlib | `Package of Meta.pkg ]

type id = {
  self : t ;
  id_pkg : package ;
  id_lib : string list ;
  id_mod : string ;
  id_qid : string list ;
}

let lemma id = String.ends_with ~suffix:"'lemma" id.I.id_string

let is_relative ?root f =
  Filename.is_relative f ||
  match root with
  | None -> false
  | Some prefix -> String.starts_with ~prefix f

let resolve ~lib ?root id =
  let lp,id_mod,id_qid = path id in
  if lp = [] then
    { self = id ; id_pkg = `Local ; id_lib = lib ; id_mod ; id_qid }
  else
    let id_pkg =
      if is_relative ?root (file id) then `Local else
        try `Package (Meta.find (List.hd lp))
        with _ -> `Stdlib
    in { self = id ; id_pkg ; id_lib = lp ; id_mod ; id_qid }

let fullname ?lib id =
  let lp,id_mod,id_qid = path ?lib id in
  String.concat "." @@ lp @ id_mod :: id_qid

let ppr fmt id =
  Format.pp_print_string fmt @@ fullname id

let standard id =
  let lp,_,_ = path id in
  lp <> [] &&
  not @@ Filename.is_relative (file id) &&
  try let _ = Meta.find (List.hd lp) in false with _ -> true

(* List Printing *)

let pp_prefix fmt q =
  Format.pp_print_string fmt q ; Format.pp_print_char fmt '.'

let rec pp_last fmt pp = function
  | [] -> ()
  | [a] -> pp fmt a
  | q::qid -> pp_prefix fmt q ; pp_last fmt pp qid

(* Title Resolution *)

let to_infix s =
  let n = String.length s in
  if n > 2 && s.[0] = '(' && s.[n-1] = ')' then
    if String.index_opt s '[' <> None
    then "mixfix " ^ String.sub s 1 (n-2) else
    if n > 3 && s.[n-2] = '_'
    then "prefix " ^ String.sub s 1 (n-3)
    else "infix " ^ String.sub s 1 (n-2)
  else s

let rec unwrap_any s = function
  | [] -> s
  | prefix::others ->
    if String.starts_with ~prefix s then
      let n = String.length s in
      let p = String.length prefix in
      Printf.sprintf "(%s)" @@ String.sub s p (n-p)
    else unwrap_any s others

let of_infix s =
  unwrap_any s ["prefix ";"infix ";"mixfix "]

let pp_infix fmt a = Format.pp_print_string fmt (of_infix a)

let pp_qpath ~local fmt r =
  let rec pp fmt = function
    | [] | [_] -> ()
    | a::w -> pp_prefix fmt a ; pp fmt w
  in
  let rec pp2 fmt s r =
    match s,r with
    | x::s , y::r when x = y -> pp2 fmt s r
    | _ -> pp fmt r
  in pp2 fmt local r.id_qid

let pp_qname fmt r =
  let rec pp fmt = function
    | [] -> ()
    | [a] -> pp_infix fmt a
    | _::w -> pp fmt w
  in pp fmt r.id_qid

let pp_local fmt r =
  pp_last fmt pp_infix r.id_qid

let pp_qid fmt qid = pp_last fmt pp_infix qid

let pp_path fmt (lib,m,qid) =
  List.iter (pp_prefix fmt) lib ;
  if qid = [] then
    Format.pp_print_string fmt m
  else
    begin
      pp_prefix fmt m ;
      pp_qid fmt qid ;
    end

let pp_title fmt r = pp_path fmt (r.id_lib,r.id_mod,r.id_qid)

let name id =
  let rec last = function
    | [] -> ""
    | [a] -> of_infix a
    | _::qid -> last qid
  in last id.id_qid

(* URI Encoding *)

let charset_uri =
  let m = Array.make 256 true in
  let r = " !#$%&'()*+,/:;=?@[]<>" in
  String.iter (fun c -> m.(Char.code c) <- false) r ; m

let charset_why3 =
  let m = Array.make 256 false in
  let s = "-._~!$'()*+,;=:@/?" in (* authorised *)
  String.iter (fun c -> m.(Char.code c) <- true) s;
  let span m a b = for i = Char.code a to Char.code b do m.(i) <- true done in
  span m 'A' 'Z'; span m 'a' 'z'; span m '0' '9'; m

let encode_char m fmt c =
  let k = int_of_char c in
  if m.(k) then
    Format.pp_print_char fmt c
  else
    Format.fprintf fmt "%%%2X" k

let encode fmt a =
  String.iter (encode_char charset_uri fmt) a

let encode_why3 fmt a =
  String.iter (encode_char charset_why3 fmt) a

(* URL Resolution *)

let pp_anchor fmt qid = pp_last fmt encode qid

let pp_selector fmt qid =
  Format.pp_print_char fmt '#' ;
  pp_anchor fmt qid

let pp_htmlfile fmt r =
  List.iter (pp_prefix fmt) r.id_lib ;
  Format.pp_print_string fmt r.id_mod ;
  Format.pp_print_string fmt ".html"

let pp_aname fmt r = pp_anchor fmt r.id_qid

let pp_ahref ~current ~package_url fmt r =
  match r.id_pkg with
  | `Local ->
    let is_decl = r.id_qid <> [] in
    let is_extern = not (is_decl && current = Some r.id_mod) in
    begin
      if is_extern then pp_htmlfile fmt r ;
      if is_decl then pp_selector fmt r.id_qid ;
    end
  | `Package m ->
    begin
      match package_url with
      | None -> Format.fprintf fmt "file://%s/html/" m.path
      | Some prefix -> Format.fprintf fmt "%s/%s/" prefix m.name
    end ;
    pp_htmlfile fmt r ;
    pp_selector fmt r.id_qid
  | `Stdlib ->
    Format.pp_print_string fmt "https://www.why3.org/stdlib/" ;
    let id = r.self in
    let name = id.id_string in
    List.iter (pp_prefix fmt) r.id_lib ;
    if r.id_qid = [] then
      Format.fprintf fmt "html#%a_" encode_why3 name
    else
      Format.fprintf fmt "html#%a_%d" encode_why3 name (line id)

let pp_proof_aname fmt r =
  pp_anchor fmt (r.id_mod :: r.id_qid)

let pp_proof_ahref fmt r =
  List.iter (pp_prefix fmt) r.id_lib ;
  Format.pp_print_string fmt "proof.html#" ;
  pp_proof_aname fmt r

(* -------------------------------------------------------------------------- *)
(* --- Declaration Resolution                                             --- *)
(* -------------------------------------------------------------------------- *)

type decl =
  | Type of Why3.Ty.tysymbol * Why3.Decl.constructor list
  | Logic of Why3.Term.lsymbol
  | Var of Why3.Ity.pvsymbol
  | Let of Why3.Expr.rsymbol
  | Exn of Why3.Ity.xsymbol
  | Axiom of Why3.Decl.prsymbol
  | Lemma of Why3.Decl.prsymbol
  | Goal of Why3.Decl.prsymbol

let eq = Why3.Ident.id_equal

let find_logic id ds =
  fst @@ List.find (fun (ls,_) -> eq id @@ ls.Why3.Term.ls_name) ds

let rec find_data id (ds : Why3.Decl.data_decl list) =
  match ds with
  | [] -> raise Not_found
  | (ts,cs) :: ds ->
    if eq id ts.ts_name then Type(ts,cs) else
      try Logic (find_logic id cs)
      with Not_found -> find_data id ds

let find_decl id (d: Why3.Decl.decl) =
  match d.d_node with
  | Dtype ts when eq id ts.ts_name -> Type(ts,[])
  | Dparam ls when eq id ls.ls_name -> Logic ls
  | Ddata ds -> find_data id ds
  | Dlogic ds -> Logic (find_logic id ds)
  | Dind(_,ds) -> Logic (find_logic id ds)
  | Dprop(Paxiom,p,_) -> Axiom p
  | Dprop(Plemma,p,_) -> Lemma p
  | Dprop(Pgoal,_,_) | Dtype _ | Dparam _ -> raise Not_found

let rec find_letdefs id = function
  | [] -> raise Not_found
  | (d : Why3.Expr.rec_defn) :: ds ->
    let rs = d.rec_sym in
    if Why3.Ident.id_equal id rs.rs_name then rs else find_letdefs id ds

let find_pdecl id (pd : Why3.Pdecl.pdecl) =
  match pd.pd_node with
  | PDtype _ | PDpure ->
    begin
      match
        List.find_map
          (fun d -> try Some (find_decl id d) with Not_found -> None)
          pd.pd_pure
      with None -> raise Not_found | Some d -> d
    end
  | PDlet (LDvar(x,_)) -> Var x
  | PDlet (LDsym (s, _)) -> Let s
  | PDlet (LDrec ds) -> Let (find_letdefs id ds)
  | PDexn x -> Exn x

let find thy id : decl =
  match Why3.Pmodule.restore_module thy with
  | m ->
    assert (thy == (Why3.Pmodule.restore_module_id id).mod_intf.mod_theory);
    find_pdecl id (Why3.Ident.Mid.find id m.mod_intf.mod_known)
  | exception Not_found ->
    find_decl id (Why3.Ident.Mid.find id thy.th_known)

(* -------------------------------------------------------------------------- *)
