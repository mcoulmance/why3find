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

module Sid = Why3.Ident.Sid

type symbol =
  | Ref of Id.id
  | Def of Id.id
  | Clone of Id.id * Id.id list

type env = {
  env : Wenv.env ;
  src : Docref.source ;
  input : Token.input ;
  mutable thy : Docref.theory option ;
  mutable opened : string list ;
  mutable scopes : string list ;
  mutable cloning : Cloning.state ;
  mutable declared : Sid.t ;
  mutable symbols : symbol Rangemap.t ;
}

let init env src input = {
  env ; src ; input ; thy = None ;
  opened = [] ;
  scopes = [] ;
  declared = Sid.empty ;
  cloning = Cloning.init ;
  symbols = Rangemap.empty ;
}

let lexpos (pos : Lexing.position) : Range.pos =
  pos.pos_lnum - 1, pos.pos_cnum - pos.pos_bol

let lexrange ((p,q) : Token.position) : Range.range = lexpos p, lexpos q

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
        env.symbols <- Rangemap.add (lexrange pos) symbol env.symbols

let process_href env pos token (href : Docref.href) =
  match href with
  | NoRef ->
    process_clone_token env (`Name token)
  | Def(id,_) ->
    env.declared <- Sid.add id.self env.declared ;
    env.symbols <- Rangemap.add (lexrange pos) (Def id) env.symbols
  | Ref id ->
    process_clone_token env (`Id id.self) ;
    env.symbols <- Rangemap.add (lexrange pos) (Ref id) env.symbols

let process_identifier ?(infix=false) env token =
  let pos = Token.position env.input in
  let href = Docref.resolve ~infix ~src:env.src ~theory:env.thy pos in
  process_href env pos token href

let process_open_module env kind =
  begin
    let id = Token.fetch_id env.input ~kind in
    env.thy <- Docref.Mstr.find_opt id env.src.theories ;
    env.opened <- [] ;
    env.scopes <- [] ;
    env.declared <- Sid.empty ;
    env.cloning <- Cloning.init ;
  end

let process_close_module env =
  begin
    env.thy <- None ;
    env.opened <- [] ;
    env.scopes <- [] ;
    env.declared <- Sid.empty ;
    env.cloning <- Cloning.init ;
  end

let process_opening s env =
  begin
    env.opened <- s :: env.opened ;
    if s = "scope" then
      let id = Token.fetch_id ~kind:s env.input in
      env.scopes <- id :: env.scopes
  end

let process_closing s env =
  match env.opened with
  | k0::ks ->
    if k0 = "scope" then env.scopes <- List.tl env.scopes ;
    env.opened <- ks
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

let parse ?doc ?filename ~env ~src file =
  let input = Token.input ?doc ?filename file in
  let env = init env src input in
  try
    while not @@ Token.eof input do
      let tok = Token.token input in
      match tok with

      (* scanned *)
      | Token.Ident s -> process_word env s
      | Token.Infix s -> process_identifier ~infix:true env s
      | Token.Ref _ -> ()
      | Token.Newline -> process_clone_token env `Newline
      | Token.Comment _ -> process_clone_token env `Comment

      (* skipped *)
      | Token.Eof
      | Token.Char _ | Token.Text _
      | Token.OpenDoc | Token.CloseDoc
      | Token.OpenSection (_, _) | Token.CloseSection _
      | Token.Space | Token.Style (_, _) | Token.Verb _ -> ()

    done ;
    Token.close input ;
    env.symbols
  with exn ->
    Token.close input ;
    raise exn
