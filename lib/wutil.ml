(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Why3 Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

open Why3

(* -------------------------------------------------------------------------- *)
(* --- Theories                                                           --- *)
(* -------------------------------------------------------------------------- *)

let rec normalize = function
  | [] -> []
  | "."::path -> normalize path
  | _::".."::path -> normalize path
  | p :: path -> p :: normalize path

let exts () =
  let formats = Why3.Env.list_formats Why3.Env.base_language in
  let exts = List.concat_map (fun (_, exts, _) -> exts) formats in
  List.map (fun ext -> "." ^ ext) exts

let filepath file =
  let dir = Filename.chop_extension file in
  let lib =
    normalize @@ String.split_on_char '/' @@
    if Filename.is_relative file then dir else Filename.basename dir
  in dir, lib

let load_theories (env : Why3.Env.env) file =
  let byloc a b =
    match a.Theory.th_name.id_loc , b.Theory.th_name.id_loc with
    | None,None -> 0
    | Some _,None -> (-1)
    | None,Some _ -> (+1)
    | Some la, Some lb -> Why3.Loc.compare la lb
  in
  let tmap,format = Why3.Env.(read_file base_language env file) in
  Wstdlib.Mstr.bindings tmap |> List.map snd |> List.sort byloc , format

(* -------------------------------------------------------------------------- *)
(* --- Iterators                                                          --- *)
(* -------------------------------------------------------------------------- *)

let iter_ts f (ts : Ty.tysymbol) = f ts.ts_name
let iter_ls f (ls : Term.lsymbol) = f ls.ls_name
let iter_decl f (d : Decl.decl) =
  match d.d_node with
  | Dtype ts -> iter_ts f ts
  | Ddata ds -> List.iter (fun (ts,_) -> iter_ts f ts) ds
  | Dparam ls -> iter_ls f ls
  | Dlogic fs -> List.iter (fun (ls,_) -> iter_ls f ls) fs
  | Dind(_,cs) -> List.iter (fun (ls,_) -> iter_ls f ls) cs
  | Dprop(_,pr,_) -> f pr.pr_name

let iter_tdecl f (td : Theory.tdecl) =
  match td.td_node with
  | Decl d -> iter_decl f d
  | Use _ | Clone _ | Meta _ -> ()

let iter_pdecl f (pd : Pdecl.pdecl) =
  Ident.Sid.iter f pd.pd_news

let iter_mi f (mi : Pmodule.mod_inst) =
  begin
    let open Ty in
    let open Ity in
    let open Term in
    let open Decl in
    let open Expr in
    Mts.iter
      (fun a ity ->
         match ity.ity_node with
         | Ityreg b -> f a.ts_name b.reg_name
         | Ityapp(b,_,_) -> f a.ts_name b.its_ts.ts_name
         | Ityvar b -> f a.ts_name b.tv_name
      ) mi.mi_ty ;
    Mts.iter (fun a b  -> f a.ts_name b.its_ts.ts_name) mi.mi_ts ;
    Mls.iter (fun a b -> f a.ls_name b.ls_name) mi.mi_ls ;
    Mpr.iter (fun a b -> f a.pr_name b.pr_name) mi.mi_pr ;
    Mvs.iter (fun a b -> f a.vs_name b.pv_vs.vs_name) mi.mi_pv ;
    Mrs.iter (fun a b -> f a.rs_name b.rs_name) mi.mi_rs ;
    Mxs.iter (fun a b -> f a.xs_name b.xs_name) mi.mi_xs ;
  end

let iter_sm f (sm : Theory.symbol_map) =
  begin
    let open Ty in
    let open Term in
    let open Decl in
    Mts.iter
      (fun a ty ->
         match ty.ty_node with
         | Tyvar _ -> ()
         | Tyapp(b,_) -> f a.ts_name b.ts_name
      ) sm.sm_ty ;
    Mts.iter (fun a b -> f a.ts_name b.ts_name) sm.sm_ts ;
    Mls.iter (fun a b -> f a.ls_name b.ls_name) sm.sm_ls ;
    Mpr.iter (fun a b -> f a.pr_name b.pr_name) sm.sm_pr ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Debug Printers                                                     --- *)
(* -------------------------------------------------------------------------- *)

let pp_thy fmt (th : Theory.theory) = Id.pp fmt th.th_name
let pp_mod fmt (m : Pmodule.pmodule) = pp_thy fmt m.mod_theory

let pp_decl fmt (d : Decl.decl) =
  match d.d_node with
  | Dtype ts ->
    begin match ts.ts_def with
      | NoDef ->
        Format.fprintf fmt "@ type %a" Id.pp ts.ts_name
      | Alias _ | Range _ | Float _ ->
        Format.fprintf fmt "@ type %a = .." Id.pp ts.ts_name
    end
  | Ddata ds ->
    List.iter (fun (ty,_) ->
        Format.fprintf fmt "@ type %a = <data>" Id.pp ty.Ty.ts_name
      ) ds
  | Dparam ls -> Format.fprintf fmt "@ function %a" Id.pp ls.ls_name
  | Dlogic lds ->
    List.iter (fun (ls,_) ->
        Format.fprintf fmt "@ function %a = .." Id.pp ls.Term.ls_name
      ) lds
  | Dind(_,lds) ->
    List.iter (fun (ls,_) ->
        Format.fprintf fmt "@ inductive %a = .." Id.pp ls.Term.ls_name
      ) lds
  | Dprop(Plemma, pr, _) -> Format.fprintf fmt "@ lemma %a" Id.pp pr.pr_name
  | Dprop(Paxiom, pr, _) -> Format.fprintf fmt "@ axiom %a" Id.pp pr.pr_name
  | Dprop(Pgoal, pr, _) -> Format.fprintf fmt "@ goal %a" Id.pp pr.pr_name

let pp_let_defn fmt (d : Expr.let_defn) =
  match d with
  | LDsym (rs, _) -> Format.fprintf fmt "@ val %a" Id.pp rs.rs_name
  | LDvar (pv, _) -> Format.fprintf fmt "@ let %a = .." Id.pp pv.pv_vs.vs_name
  | LDrec ls ->
    List.iter (fun (rd : Expr.rec_defn) ->
        Format.fprintf fmt "@ let rec %a = .." Id.pp rd.rec_sym.rs_name
      ) ls

let pp_tdecl fmt (d : Theory.tdecl) =
  match d.td_node with
  | Meta(m,_) -> Format.fprintf fmt "@ meta %s" m.meta_name
  | Decl d -> pp_decl fmt d
  | Use thy -> Format.fprintf fmt "@ use %a" pp_thy thy
  | Clone(thy,sigma) ->
    Format.fprintf fmt "@ @[<v 0>@[<v 2>clone %a {" pp_thy thy ;
    iter_sm (fun a b ->
        Format.fprintf fmt "@ %a -> %a ;" Id.pp a Id.pp b
      ) sigma ;
    Format.fprintf fmt "@]@ }@]"

let pp_pdecl fmt (d : Pdecl.pdecl) =
  match d.pd_node with
  | PDtype ts ->
    List.iter
      (fun (t : Pdecl.its_defn) ->
         let its = t.itd_its in
         Format.fprintf fmt "@ type %a" Id.pp its.its_ts.ts_name ;
         match its.its_def with
         | Alias _ | Range _ | Float _ -> Format.fprintf fmt " = .."
         | NoDef ->
           if its.its_private then
             Format.fprintf fmt " = private {..}"
           else
             Format.fprintf fmt " = {..}"
      ) ts
  | PDlet d -> pp_let_defn fmt d
  | PDexn e -> Format.fprintf fmt "@ exception %a" Id.pp e.xs_name
  | PDpure -> List.iter (pp_decl fmt) d.pd_pure

let rec pp_munit fmt (m : Pmodule.mod_unit) =
  match m with
  | Umeta(m,_) -> Format.fprintf fmt "@ meta %s" m.meta_name
  | Uuse m -> Format.fprintf fmt "@ use %a" pp_mod m
  | Uscope(s,ms) ->
    Format.fprintf fmt "@ @[<v 0>@[<v 2>scope %s {" s ;
    List.iter (pp_munit fmt) ms ;
    Format.fprintf fmt "@]@ }@]"
  | Uclone mi ->
    Format.fprintf fmt "@ @[<v 0>@[<v 2>clone %a {" pp_mod mi.mi_mod ;
    iter_mi (fun a b ->
        Format.fprintf fmt "@ %a -> %a ;" Id.pp a Id.pp b
      ) mi ;
    Format.fprintf fmt "@]@ }@]"
  | Udecl d -> pp_pdecl fmt d

let pp_module fmt (m : Pmodule.pmodule) =
  begin
    Format.fprintf fmt "@[<hv 0>@[<hv 2>module %a {" pp_mod m ;
    List.iter (pp_munit fmt) m.mod_units ;
    Format.fprintf fmt "@]@ }@]" ;
  end

let pp_theory fmt (thy : Theory.theory) =
  begin
    Format.fprintf fmt "@[<hv 0>@[<hv 2>theory %a {" pp_thy thy ;
    List.iter (pp_tdecl fmt) thy.th_decls ;
    Format.fprintf fmt "@]@ }@]" ;
  end

let pp_position fmt loc =
  let file,l1,c1,l2,c2 = Why3.Loc.get loc in
  if l1=l2 then
    Format.fprintf fmt "File %S, line %d, characters %d-%d" file l1 (c1+1) (c2+1)
  else
    Format.fprintf fmt "File %S, lines %d-%d" file l1 l2

(* -------------------------------------------------------------------------- *)
