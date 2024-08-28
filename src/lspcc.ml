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

let init () = () (* for dependencies only *)

(* -------------------------------------------------------------------------- *)
(* ---  Source Management                                                 --- *)
(* -------------------------------------------------------------------------- *)

type source = {
  env : Wenv.env ;
  src : Docref.source ;
  hrefs : Docref.href Rangemap.t ;
  mutable reloaded : bool ;
  mutable updates : Lspserver.update list ;
}

let sources = Hashtbl.create 0

let set_source ~filename env src hrefs =
  Hashtbl.replace sources filename {
    env ; src ; hrefs ;
    reloaded = false ;
    updates = [] ;
  }

let find_decl (source:source) id =
  let env = source.env.wenv in
  let src = source.src in
  Docref.find_decl env src id

(* -------------------------------------------------------------------------- *)
(* --- Compiler for the LSP Server                                        --- *)
(* -------------------------------------------------------------------------- *)

let config_for ~filename =
  let rec lookup = function
    | "/" | "." -> None
    | dir ->
      let config = Filename.concat dir "why3find.json" in
      if Sys.file_exists config
      then Some (dir,config)
      else lookup (Filename.dirname dir)
  in lookup (Filename.dirname filename)

let of_loc (loc : Why3.Loc.position) : string * Lspserver.range =
  let file,l1,c1,l2,c2 = Why3.Loc.get loc in
  file , {
    start = { line = l1 - 1 ; char = c1 } ;
    stop = { line = l2 - 1 ; char = c2 }
  }

let of_range (rg : Range.range) : Lspserver.range =
  let ((l1,c1),(l2,c2)) = rg in
  {
    start = { line = l1 ; char = c1 } ;
    stop = { line = l2 ; char = c2 } ;
  }

type lexpos = Lexing.position * Lexing.position

let range_of_pos ((p,q) : lexpos) : string * Lspserver.range =
  p.pos_fname , {
    start = { line = p.pos_lnum ; char = p.pos_cnum - p.pos_bol } ;
    stop = { line = q.pos_lnum ; char = q.pos_cnum - q.pos_cnum } ;
  }

let library ~root ~filename =
  if String.starts_with ~prefix:root filename then
    let p = String.length root in
    let n = String.length filename in
    Docref.library_path @@ String.sub filename p (n-p)
  else
    Docref.library_path @@ filename

let preprocess filename diagnostics =
  try
    let warning (file,range) message =
      if file = filename then
        let severity = `Warning in
        let diag = Lspserver.diagnostic ~range ~severity ~message () in
        diagnostics := diag :: !diagnostics
      else
        Format.printf "[Warning] File %s, line %d: %s@."
          file range.start.line message
    in
    Why3.Loc.set_warning_hook
      begin fun ?loc message ->
        match loc with
        | None -> Format.printf "[Warning] %s@." message
        | Some loc -> warning (of_loc loc) message
      end ;
    Token.set_handler
      begin fun input message ->
        warning (range_of_pos @@ Token.position input) message
      end ;
    let root =
      match config_for ~filename with
      | None -> Filename.dirname filename
      | Some(dir,_) -> dir
    in
    Hashtbl.remove sources filename ;
    let config = Config.load_config root in
    let env = Wenv.init ~root config in
    let cenv = Docref.init () in
    let wenv = env.wenv in
    let henv = Axioms.init env in
    let lib = library ~root ~filename in
    let parser = Docref.source ~wenv ~cenv ~henv ~lib ~root ~filename in
    let src = Utils.input ~file:filename parser in
    let hrefs = Srcref.parse ~doc:false ~env ~src filename in
    set_source ~filename env src hrefs ;
  with
  | Why3.Loc.Located(loc,exn) ->
    let file,range = of_loc loc in
    if file = filename then
      let severity = `Error in
      let message = Printexc.to_string exn in
      let diag = Lspserver.diagnostic ~range ~severity ~message () in
      diagnostics := diag :: !diagnostics
    else
      Format.printf "[Warning]@[ File %a:@ %a@]@."
        Why3.Loc.pp_position loc Why3.Exn_printer.exn_printer exn
  | exn ->
    Format.printf "[Error] %s@." (Printexc.to_string exn)

let compile channel doc =
  let Lspserver.Doc { uri } = doc in
  match Lspserver.to_file ~uri with
  | None -> ()
  | Some filename ->
    if Filename.check_suffix filename ".mlw" then
      begin
        let diagnostics = ref [] in
        preprocess filename diagnostics ;
        Lspserver.publish_diagnostic channel ~uri !diagnostics
      end

let update _channel ~uri update =
  match Lspserver.to_file ~uri with
  | None -> ()
  | Some filename ->
    match Hashtbl.find sources filename with
    | exception Not_found -> ()
    | source -> source.updates <- update :: source.updates

let () = Lspserver.on_open compile
let () = Lspserver.on_save compile
let () = Lspserver.on_update update

(* -------------------------------------------------------------------------- *)
(* --- References                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec backward updates pos =
  match updates with
  | [] -> Some pos
  | upd::updates ->
    Option.bind (Lspserver.backward_position upd pos) (backward updates)

let rec forward updates pos =
  match updates with
  | [] -> Some pos
  | upd::updates ->
    Option.bind (Lspserver.forward_position pos upd) (forward updates)

let locate ~uri (pos : Lspserver.position) =
  match Lspserver.to_file ~uri with
  | None -> raise Not_found
  | Some filename ->
    let src = Hashtbl.find sources filename in
    match backward src.updates pos with
    | Some pos ->
      let range,href = Rangemap.find (pos.line, pos.char) src.hrefs in
      filename, src, range, href
    | None -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Goto Definition                                                    --- *)
(* -------------------------------------------------------------------------- *)

let goto = function
  | None -> raise Not_found
  | Some loc ->
    let filename,range = of_loc loc in
    let uri = Lspserver.to_uri ~filename in
    Lspserver.Loc { uri ; range }

let definition _channel ~uri (pos : Lspserver.position) =
  let _,_,_,href = locate ~uri pos in
  match href with
  | Ref id -> [goto id.self.id_loc]
  | _ -> []

let () = Lspserver.on_definition definition

let typeof_ty (ty : Why3.Ty.ty) =
  match ty.ty_node with
  | Why3.Ty.Tyvar _ -> raise Not_found
  | Why3.Ty.Tyapp (ts, _) -> ts

let typeof_decl = function
  | Id.Type(ts,_) -> ts
  | Id.Logic { ls_value = Some result } -> typeof_ty result
  | Id.Logic { ls_value = None } -> raise Not_found
  | Id.Var pv -> typeof_ty pv.pv_vs.vs_ty
  | Id.Let r -> typeof_ty @@ Why3.Ity.ty_of_ity r.rs_cty.cty_result
  | Id.Exn x -> typeof_ty @@ Why3.Ity.ty_of_ity x.xs_ity

let typedef _channel ~uri (pos : Lspserver.position) =
  let _,src,_,href = locate ~uri pos in
  match href with
  | Ref id ->
    begin
      try
        let ts = typeof_decl @@ find_decl src id in
        [goto ts.ts_name.id_loc]
      with Not_found ->
        [goto id.self.id_loc]
    end
  | _ -> []

let () = Lspserver.on_definition ~kind:`Typedef typedef

(* -------------------------------------------------------------------------- *)
(* --- Hover                                                              --- *)
(* -------------------------------------------------------------------------- *)

let pp_name fmt (id : Why3.Ident.ident) =
  Format.pp_print_string fmt id.id_string

let pp_infix fmt (id : Why3.Ident.ident) =
  Format.pp_print_string fmt (Id.of_infix id.id_string)

let pp_tvar fmt (x : Why3.Ty.tvsymbol) =
  Format.fprintf fmt "'%a" pp_name x.tv_name

let rec pp_type fmt (t : Why3.Ty.ty) =
  match t.ty_node with
  | Why3.Ty.Tyvar x -> pp_tvar fmt x
  | Why3.Ty.Tyapp (t, ts) when Why3.Ty.is_ts_tuple t -> pp_tuple fmt ts
  | Why3.Ty.Tyapp (t, [a;b]) when Why3.Ty.(ts_equal t ts_func) ->
    Format.fprintf fmt "@[<hov 2>(%a@ -> %a)@]" pp_type a pp_func b ;
  | Why3.Ty.Tyapp (t,[]) -> pp_name fmt t.ts_name
  | Why3.Ty.Tyapp (t, ts) ->
    Format.fprintf fmt "@[<hov 2>(%a" pp_name t.ts_name ;
    List.iter (Format.fprintf fmt "@ %a" pp_type) ts ;
    Format.fprintf fmt ")@]"

and pp_tuple fmt = function
  | [] -> Format.pp_print_string fmt "unit"
  | t::ts ->
    Format.fprintf fmt "@[<hov 2>(%a" pp_type t ;
    List.iter (Format.fprintf fmt ",@ %a" pp_type) ts ;
    Format.fprintf fmt ")@]"

and pp_func fmt t =
  match t.ty_node with
  | Why3.Ty.Tyapp (t, [a;b]) when Why3.Ty.(ts_equal t ts_func) ->
    Format.fprintf fmt "%a@ -> %a" pp_type a pp_func b
  | _ -> pp_type fmt t

let is_record (c : Why3.Decl.constructor) =
  List.for_all (function None -> false | Some _ -> true) @@ snd c

let is_unit ty = Why3.Ity.(ity_equal ty ity_unit)

let pp_ity fmt ty =
  pp_type fmt @@ Why3.Ity.ty_of_ity ty

let pp_field fmt (f : Why3.Term.lsymbol option) =
  match f with
  | None -> ()
  | Some ls ->
    Format.fprintf fmt "@ @[<hov 2>%a:" pp_name ls.ls_name ;
    List.iter (Format.fprintf fmt "@ %a" pp_type) ls.ls_args ;
    Format.fprintf fmt " ;@]"

let pp_constr fmt (c : Why3.Decl.constructor) =
  let ls = fst c in
  Format.fprintf fmt "@ | %a" pp_name ls.ls_name ;
  List.iter (Format.fprintf fmt " %a" pp_type) ls.ls_args

let pp_tdecl fmt (ts : Why3.Ty.tysymbol) =
  pp_name fmt ts.ts_name ;
  List.iter (Format.fprintf fmt " %a" pp_tvar) ts.ts_args

let pp_vdecl fmt (pv : Why3.Ity.pvsymbol) =
  if pv.pv_ghost then Format.fprintf fmt "ghost " ;
  let vs = pv.pv_vs in
  Format.fprintf fmt "%a : %a" pp_name vs.vs_name pp_type vs.vs_ty

let pp_varg fmt (pv : Why3.Ity.pvsymbol) =
  if pv.pv_ghost then Format.fprintf fmt "(ghost %a)" pp_ity pv.pv_ity
  else pp_ity fmt pv.pv_ity

let pp_decl fmt (d : Id.decl) =
  match d with
  | Id.Type (t, []) ->
    begin
      match t.ts_def with
      | Why3.Ty.NoDef ->
        Format.fprintf fmt "type %a" pp_tdecl t
      | Why3.Ty.Alias ty ->
        Format.fprintf fmt "type %a = %a" pp_tdecl t pp_type ty
      | Why3.Ty.Range { ir_lower ; ir_upper } ->
        Format.fprintf fmt "type %a = <range %s %s>" pp_name t.ts_name
          (Why3.BigInt.to_string ir_lower)
          (Why3.BigInt.to_string ir_upper)
      | Why3.Ty.Float ff ->
        Format.fprintf fmt "type %a = <float %d %d>" pp_name t.ts_name
          ff.fp_significand_digits ff.fp_exponent_digits
    end
  | Id.Type (t, [r]) when is_record r ->
    Format.fprintf fmt "@[<hv 0>@[<hv 2>type %a = {" pp_tdecl t ;
    List.iter (pp_field fmt) @@ snd r ;
    Format.fprintf fmt "@]@ }@]"
  | Id.Type (t, cs) ->
    Format.fprintf fmt "@[<hv 2>type %a =" pp_tdecl t ;
    List.iter (pp_constr fmt) cs ;
    Format.fprintf fmt "@]"
  | Id.Logic ls ->
    let kind = if ls.ls_value = None then "predicate" else "function" in
    Format.fprintf fmt "@[<hv 2>%s %a" kind pp_infix ls.ls_name ;
    List.iter (Format.fprintf fmt "@ %a" pp_type) ls.ls_args ;
    Option.iter (Format.fprintf fmt "@ : %a" pp_type) ls.ls_value ;
    if ls.ls_proj then Format.fprintf fmt "@ (* field *)" ;
    if ls.ls_constr > 0 then Format.fprintf fmt "@ (* constructor *)" ;
    Format.fprintf fmt "@]"
  | Id.Var x ->
    Format.fprintf fmt "@[<hov 2>val %a@]" pp_vdecl x
  | Id.Let r ->
    begin
      Format.fprintf fmt "@[<hov 2>val" ;
      if Why3.Expr.rs_ghost r then Format.fprintf fmt " ghost" ;
      begin match Why3.Expr.rs_kind r with
        | RKnone | RKlocal -> ()
        | RKfunc -> Format.fprintf fmt " function"
        | RKpred -> Format.fprintf fmt " predicate"
        | RKlemma -> Format.fprintf fmt " lemma"
      end ;
      Format.fprintf fmt " %a" pp_infix r.rs_name ;
      let args = r.rs_cty.cty_args in
      List.iter (Format.fprintf fmt "@ %a" pp_varg) args ;
      Format.fprintf fmt "@ : %a" pp_ity r.rs_cty.cty_result
    end
  | Id.Exn x ->
    Format.fprintf fmt "@[<hov 2>exception %a" pp_name x.xs_name ;
    let ty = x.xs_ity in
    if not @@ is_unit ty then Format.fprintf fmt " : %a" pp_ity ty ;
    Format.fprintf fmt "@]"

let code source id =
  try
    String.split_on_char '\n' @@ Format.asprintf "%a" pp_decl @@ find_decl source id
  with Not_found -> []

let hover _channel ~uri (pos : Lspserver.position) =
  let _,source,rg,href = locate ~uri pos in
  match href with
  | Ref id ->
    let title = Format.asprintf "%a" Id.pp_title id in
    let code = Lspserver.code ~lang:"why3" @@ code source id in
    Lspserver.hover ~range:(of_range rg) ~kind:`Markdown (title::code)
  | _ -> raise Not_found

let () = Lspserver.on_hover hover

(* -------------------------------------------------------------------------- *)
(* ---  Code Lenses                                                       --- *)
(* -------------------------------------------------------------------------- *)

module M = Map.Make(String)

let add k a =
  M.add k (try succ @@ M.find k a with Not_found -> 1) a

let rec provers m (crc : Crc.crc) =
  match crc.state with
  | Stuck -> ()
  | Prover (p, _) ->
    m := add (Prover.desc_name p) !m
  | Tactic { id ; children } ->
    m := add id !m ; List.iter (provers m) children

let pp_verdict fmt crc =
  match Crc.verdict crc with
  | `Valid 0 -> Format.pp_print_string fmt "Valid (no goal)"
  | `Valid _ -> Format.pp_print_string fmt "Valid"
  | `Failed n -> Format.fprintf fmt "Failed (%d goals)" n
  | `Partial (s,n) -> Format.fprintf fmt "Partial (%d/%d)" s n

let pp_time fmt crc =
  let t = Crc.get_time crc in
  if t > 0.0 then Format.fprintf fmt " (%a)" Utils.pp_time t

let pp_depth fmt crc =
  let d = Crc.get_depth crc in
  if d > 0 then Format.fprintf fmt " (depth %d)" d

let pp_prover fmt p n =
  if n > 1
  then Format.fprintf fmt " (%s %d)" p n
  else Format.fprintf fmt " (%s)" p

let pp_provers fmt crc =
  let m = ref M.empty in
  provers m crc ; M.iter (pp_prover fmt) !m

let pp_crc fmt (crc : Crc.crc) =
  begin
    pp_verdict fmt crc ;
    pp_time fmt crc ;
    pp_provers fmt crc ;
    pp_depth fmt crc ;
  end

let get_proof source = function
  | Docref.NoRef | Ref _ -> None
  | Def(_,proof) when not source.reloaded -> proof
  | Def(id,_) ->
    let thy = Docref.Mstr.find_opt id.id_mod source.src.theories in
    Docref.find_proof id.self thy

let lenses _channel ~uri =
  match Lspserver.to_file ~uri with
  | None -> raise Not_found
  | Some filename ->
    let source = Hashtbl.find sources filename in
    let pool = ref [] in
    Rangemap.iter
      begin fun rg (href : Docref.href) ->
        match get_proof source href with
        | None -> ()
        | Some crc ->
          let line = fst @@ fst rg in
          match forward source.updates Lspserver.{ line ; char = 0} with
          | None -> ()
          | Some { line } ->
            let title = Format.asprintf "%a" pp_crc crc in
            let cmd = Lspserver.Cmd { title ; command = "" ; args = [] } in
            pool := Lspserver.resolved ~line cmd :: !pool
      end source.hrefs ;
    !pool

let () = Lspserver.on_codelens lenses

(* -------------------------------------------------------------------------- *)
(* ---  Proof Watcher                                                     --- *)
(* -------------------------------------------------------------------------- *)

let watching = ref false

let () = Lspserver.on_open
    begin fun channel _doc ->
      if not !watching then
        begin
          watching := true ;
          ignore @@ Lspserver.watch channel ["**/proof.json"]
        end
    end

let () = Lspserver.on_file_event
    begin fun channel ~uri _evt ->
      match Lspserver.to_file ~uri with
      | None -> ()
      | Some file ->
        let suffix = "/proof.json" in
        if String.ends_with ~suffix file then
          let n = String.length suffix in
          let base = String.sub file 0 (String.length file - n) in
          let file = base ^ ".mlw" in
          match Hashtbl.find_opt sources file with
          | None -> ()
          | Some source ->
            Docref.reload_proofs source.src ;
            source.reloaded <- true ;
            Lspserver.refresh_codelens channel
    end


(* -------------------------------------------------------------------------- *)
