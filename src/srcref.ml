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
(* ---  Document Lightweight Parsing for LSP                              --- *)
(* -------------------------------------------------------------------------- *)

module Sid = Why3.Ident.Sid

type symbol =
  | Ref of Id.id
  | Def of Id.id
  | Clone of Id.id * Id.id list
  | Section of string

type frame = Frame of {
    name : string ;
    first : Token.position ;
    token : Token.position ;
    kind : Lspserver.kind ;
    stacked : Lspserver.symbol list ;
  }

type env = {
  env : Wenv.env ;
  src : Docref.source ;
  input : Token.input ;
  mutable blanklines : int ; (* number of consecutive blanklines so far *)
  mutable closing : Token.position ; (* last code-token before the first *)
  mutable first : Token.position ; (* fist code-token after last blankline *)
  mutable last : Token.position ; (* last code-token so far *)
  mutable current : string option ; (* theory name *)
  mutable thy : Docref.theory option ;
  mutable opened : string list ;
  mutable scopes : string list ;
  mutable declared : Sid.t ;
  mutable cloning : Cloning.state ;
  mutable sections : (string * Lexing.position) list ;
  mutable symbols : Lspserver.symbol list ;
  mutable frame : frame option ;
  mutable stack : frame list ;
  mutable refs : symbol Rangemap.t ;
}

(* -------------------------------------------------------------------------- *)
(* ---  Positions & Ranges                                                --- *)
(* -------------------------------------------------------------------------- *)

let init env src input =
  let here = Token.position input in
  {
    env ; src ; input ; thy = None ; current = None ;
    blanklines = 0 ; closing = here ; first = here ; last = here ;
    opened = [] ; scopes = [] ; sections = [] ;
    declared = Sid.empty ;
    cloning = Cloning.init ;
    symbols = [] ; frame = None ; stack = [] ;
    refs = Rangemap.empty ;
  }

let lexpos (pos : Lexing.position) : Range.pos =
  pos.pos_lnum - 1, pos.pos_cnum - pos.pos_bol

let lexrange ((p,q) : Token.position) : Range.range = lexpos p, lexpos q

let lspos (pos : Lexing.position) : Lspserver.position =
  let line,char = lexpos pos in { line ; char }

let lsrange ((p,q) : Token.position) : Lspserver.range =
  { start = lspos p ; stop = lspos q }

(* -------------------------------------------------------------------------- *)
(* ---  Document Symbols Stack                                            --- *)
(* -------------------------------------------------------------------------- *)

let flush env =
  match env.frame with
  | None -> ()
  | Some (Frame { first ; token ; name ; kind }) ->
    let range = lsrange (fst first,snd env.closing) in
    let selection = lsrange token in
    let symbol = Lspserver.symbol ~range ~name ~kind ~selection () in
    env.symbols <- symbol :: env.symbols ;
    env.frame <- None

let frame env name kind =
  flush env ;
  let token = Token.position env.input in
  env.last <- token ;
  env.closing <- token ;
  env.frame <- Some (Frame {
      first = env.first ;
      token ; name ; kind ; stacked = [] ;
    })

let push env name kind =
  begin
    flush env ;
    let token = Token.position env.input in
    env.last <- token ;
    env.closing <- token ;
    let frame =
      Frame {
        first = env.first ;
        token ; name ; kind ;
        stacked = env.symbols
      } in
    env.symbols <- [] ;
    env.stack <- frame :: env.stack ;
  end

let pop env =
  flush env ;
  match env.stack with
  | [] -> ()
  | Frame { first ; token ; name ; kind ; stacked } :: frames ->
    let children = List.rev env.symbols in
    let range = lsrange (fst first,snd env.last) in
    let selection = lsrange token in
    let symbol = Lspserver.symbol ~range ~name ~kind ~selection ~children () in
    begin
      env.stack <- frames ;
      env.symbols <- symbol :: stacked ;
    end

let add_symbol env id =
  try
    let kind : Lspserver.kind =
      match Docref.find_decl env.env.wenv.env env.src id with
      | Type _ -> `TypeParameter
      | Logic _ -> `Function
      | Var _ -> `Variable
      | Let _ -> `Method
      | Exn _ -> `Event
      | Id.Axiom _ | Id.Lemma _ | Id.Goal _ -> `Property in
    frame env (Id.name id) kind ;
  with Not_found -> ()

(* -------------------------------------------------------------------------- *)
(* ---  Clones & Identifiers                                              --- *)
(* -------------------------------------------------------------------------- *)

let resolve env id = Id.resolve ~lib:env.src.lib ~root:env.src.filename id

let process_clone_token env token =
  let state, action = Cloning.process env.cloning env.input token in
  env.cloning <- state ;
  match action with
  | `Nop | `Space | `Spaces _ | `Target _ -> ()
  | `Clone(_,export,pos,id) ->
    match env.thy with
    | None -> ()
    | Some thy ->
      let scope = Cloning.instance ~scope:env.scopes ~cloned:id export in
      match Docref.find_instance thy.instances ~scope id with
      | None -> ()
      | Some (Instance s) ->
        let id = resolve env id in
        let ids =
          List.filter_map
            (function
              | Docref.Inst _ -> None
              | Clone { target } ->
                if Sid.mem target env.declared then None else
                  ( env.declared <- Sid.add target env.declared ;
                    Some (resolve env target) )
            ) s.clones in
        let symbol = Clone(id,ids) in
        env.refs <- Rangemap.add (lexrange pos) symbol env.refs

(* -------------------------------------------------------------------------- *)
(* ---  References                                                        --- *)
(* -------------------------------------------------------------------------- *)

let process_reference env s =
  try
    let { src ; current ; env = { wenv }} = env in
    let _, id = Docref.reference ~wenv:wenv.env ~src ~current s in
    let id = Id.resolve ~lib:src.lib id in
    let pos = Token.position env.input in
    env.refs <- Rangemap.add (lexrange pos) (Ref id) env.refs ;
  with
  | Not_found -> Token.error env.input "unknown reference"
  | Failure msg -> Token.error env.input "%s" msg

let process_href env pos token (href : Docref.href) =
  match href with
  | NoRef ->
    process_clone_token env (`Name token)
  | Def(id,_) ->
    env.declared <- Sid.add id.self env.declared ;
    env.refs <- Rangemap.add (lexrange pos) (Def id) env.refs ;
    add_symbol env id
  | Ref id ->
    process_clone_token env (`Id id.self) ;
    env.refs <- Rangemap.add (lexrange pos) (Ref id) env.refs

let process_identifier ?(infix=false) env token =
  let pos = Token.position env.input in
  let href = Docref.resolve ~infix ~src:env.src ~theory:env.thy pos in
  process_href env pos token href

let process_open_module env kind =
  begin
    let id = Token.fetch_id env.input ~kind in
    env.thy <- Docref.Mstr.find_opt id env.src.theories ;
    env.current <- Some id ;
    env.opened <- [] ;
    env.scopes <- [] ;
    env.declared <- Sid.empty ;
    env.cloning <- Cloning.init ;
    push env id `Module ;
  end

let process_close_module env =
  begin
    pop env ;
    env.thy <- None ;
    env.current <- None ;
    env.opened <- [] ;
    env.scopes <- [] ;
    env.declared <- Sid.empty ;
    env.cloning <- Cloning.init ;
  end

let process_opening s env =
  begin
    env.opened <- s :: env.opened ;
    if s = "scope" then
      begin
        let id = Token.fetch_id ~kind:s env.input in
        push env id `Namespace ;
        env.scopes <- id :: env.scopes ;
      end
  end

let process_closing s env =
  match env.opened with
  | k0::ks ->
    env.opened <- ks ;
    if k0 = "scope" then
      begin
        pop env ;
        env.scopes <- List.tl env.scopes ;
      end
  | [] ->
    Token.error env.input "No matching opening keyword for '%s" s

let process_word env s =
  if Docref.is_keyword s then
    begin
      match s with
      | "module" | "theory" -> process_open_module env s
      | "end" when env.opened = [] -> process_close_module env
      | _ ->
        if Docref.is_opening s then process_opening s env ;
        if Docref.is_closing s then process_closing s env ;
        process_clone_token env (`Kwd s)
    end
  else
    process_identifier env s

let process_open_section env input s =
  let _,p = Token.position input in
  env.sections <- (s,p) :: env.sections

let process_close_section env input =
  match env.sections with
  | [] -> ()
  | (s,p) :: sections ->
    let q,_ = Token.position input in
    env.sections <- sections ;
    env.refs <- Rangemap.add (lexrange (p,q)) (Section s) env.refs

let process_block env = function
  | Token.Newline ->
    env.blanklines <- succ env.blanklines

  | Token.Infix _ | Token.Ident _ | Token.Ref _ ->
    let here = Token.position env.input in
    if env.blanklines > 0 then
      begin
        env.closing <- env.last ;
        env.first <- here ;
        env.blanklines <- 0 ;
      end ;
    env.last <- here

  | Token.Comment _ | Token.Space | Token.Eof
  | Token.Char _ | Token.Text _ | Token.Style _
  | Token.OpenDoc | Token.CloseDoc
  | Token.OpenSection _ | Token.CloseSection _
  | Token.Verb _ -> ()

let parse ?doc ?filename ~env ~src file =
  let input = Token.input ?doc ?filename file in
  let env = init env src input in
  try
    while not @@ Token.eof input do
      let tok = Token.token input in
      process_block env tok ;
      match tok with

      (* scanned *)
      | Token.Ident s -> process_word env s
      | Token.Infix s -> process_identifier ~infix:true env s
      | Token.Ref s -> process_reference env s
      | Token.Newline -> process_clone_token env `Newline
      | Token.Comment _ -> process_clone_token env `Comment

      (* folding *)
      | Token.OpenSection (_, s) -> process_open_section env input s
      | Token.CloseSection _ -> process_close_section env input

      (* skipped *)
      | Token.Eof
      | Token.Char _ | Token.Text _
      | Token.OpenDoc | Token.CloseDoc
      | Token.Space | Token.Style (_, _) | Token.Verb _ -> ()

    done ;
    Token.close input ;
    env.refs, env.symbols
  with exn ->
    Token.close input ;
    raise exn
