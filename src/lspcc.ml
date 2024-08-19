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

let self = ()

(* -------------------------------------------------------------------------- *)
(* ---  Source Management                                                 --- *)
(* -------------------------------------------------------------------------- *)

type source = {
  env : Wenv.env ;
  src : Docref.source ;
  hrefs : Docref.href Rangemap.t ;
  mutable updates : Lspserver.update list ;
}

let sources = Hashtbl.create 0

let set_source ~filename env src hrefs =
  Hashtbl.replace sources filename { env ; src ; hrefs ; updates = [] }

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

let library ~loadpath ~filename =
  if String.starts_with ~prefix:loadpath filename then
    let p = String.length loadpath in
    let n = String.length filename in
    Docref.library_path @@ String.sub filename p (n-p)
  else
    Docref.library_path @@ filename

let preprocess filename text diagnostics =
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
    let loadpath =
      match config_for ~filename with
      | None -> Filename.dirname filename
      | Some(dir,_) -> dir
    in
    let tmp,out = Filename.open_temp_file "lsp" ".mlw" in
    Ropes.to_channel out text ;
    close_out out ;
    Wenv.local ~loadpath () ;
    Hashtbl.remove sources filename ;
    let env = Wenv.init () in
    let cenv = Docref.init () in
    let wenv = env.wenv in
    let henv = Axioms.init env in
    let lib = library ~loadpath ~filename in
    let parser = Docref.source ~wenv ~cenv ~henv ~lib ~loadpath ~filename in
    let src = Utils.input ~file:tmp parser in
    let hrefs = Srcref.parse ~doc:false ~filename ~env ~src tmp in
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
  let Lspserver.Doc { uri ; text } = doc in
  match Lspserver.to_file ~uri with
  | None -> ()
  | Some filename ->
    if Filename.check_suffix filename ".mlw" then
      begin
        let diagnostics = ref [] in
        preprocess filename text diagnostics ;
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

let rec rollback updates pos =
  match updates with
  | [] -> Some pos
  | upd::updates ->
    Option.bind (Lspserver.backward_position upd pos) (rollback updates)

let locate ~uri (pos : Lspserver.position) =
  match Lspserver.to_file ~uri with
  | None -> raise Not_found
  | Some filename ->
    let src = Hashtbl.find sources filename in
    match rollback src.updates pos with
    | Some pos ->
      let range,href = Rangemap.find (pos.line, pos.char) src.hrefs in
      filename, src, range, href
    | None -> raise Not_found

let goto loc =
  let filename,range = of_loc loc in
  let uri = Lspserver.to_uri ~filename in
  Lspserver.Loc { uri ; range }

(* -------------------------------------------------------------------------- *)
(* --- Goto Definition                                                    --- *)
(* -------------------------------------------------------------------------- *)

let definition _channel ~uri (pos : Lspserver.position) =
  let _,_,_,href = locate ~uri pos in
  match href with
  | Ref { self = { id_loc = Some loc } } -> [goto loc]
  | _ -> []

let () = Lspserver.on_definition definition

(* -------------------------------------------------------------------------- *)
(* --- Hover                                                              --- *)
(* -------------------------------------------------------------------------- *)

let pp_name fmt (id : Why3.Ident.ident) =
  Format.pp_print_string fmt id.id_string

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
  | Id.Logic ( { ls_name ; ls_args ; ls_value = None } ) ->
    Format.fprintf fmt "@[<hv 2>predicate %a" pp_name ls_name ;
    List.iter (Format.fprintf fmt "@ %a" pp_type) ls_args ;
    Format.fprintf fmt "@]"
  | Id.Logic ( { ls_name ; ls_args ; ls_value = Some ls_result } ) ->
    Format.fprintf fmt "@[<hv 2>function %a" pp_name ls_name ;
    List.iter (Format.fprintf fmt "@ %a" pp_type) ls_args ;
    Format.fprintf fmt "@ : %a@]" pp_type ls_result
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
      let args = r.rs_cty.cty_args in
      List.iter (Format.fprintf fmt "@ %a" pp_varg) args ;
      Format.fprintf fmt "@ : %a" pp_ity r.rs_cty.cty_result
    end
  | Id.Exn x ->
    Format.fprintf fmt "@[<hov 2>exception %a" pp_name x.xs_name ;
    let ty = x.xs_ity in
    if not @@ is_unit ty then Format.fprintf fmt " : %a" pp_ity ty ;
    Format.fprintf fmt "@]"

let code env id =
  try
    String.split_on_char '\n' @@
    Format.asprintf "%a" pp_decl @@
    Id.find env id
  with Not_found -> []

let hover _channel ~uri (pos : Lspserver.position) =
  let _,src,rg,href = locate ~uri pos in
  match href with
  | Ref id ->
    let title = Format.asprintf "%a" Id.pp_title id in
    let code = Lspserver.code ~lang:"why3" @@ code src.env.wenv id in
    Lspserver.hover ~range:(of_range rg) ~kind:`Markdown (title::code)
  | _ -> raise Not_found

let () = Lspserver.on_hover hover

(* -------------------------------------------------------------------------- *)
