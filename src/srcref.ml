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

type env = {
  env : Wenv.env ;
  src : Docref.source ;
  input : Token.input ;
  mutable thy : Docref.theory option ;
  mutable opened : int ;
  mutable refs : Docref.href Rangemap.t ;
}

let init env src input = {
  env ; src ; input ; thy = None ; opened = 0 ; refs = Rangemap.empty
}

type lexpos = Lexing.position * Lexing.position

let lexpos (pos : Lexing.position) : Range.pos =
  pos.pos_lnum - 1, pos.pos_cnum - pos.pos_bol - 1

let lexrange ((p,q) : lexpos) : Range.range = lexpos p, lexpos q

let process_href (env:env) (pos:lexpos) (href : Docref.href) =
  if href <> NoRef then
    env.refs <- Rangemap.add (lexrange pos) href env.refs

let process_identifier ?(infix=false) env (_token:string) =
  let pos = Token.position env.input in
  let href = Docref.resolve ~infix ~src:env.src ~theory:env.thy pos in
  process_href env pos href

let process_open_module env kind =
  begin
    let id = Token.fetch_id env.input ~kind in
    env.thy <- Docref.Mstr.find_opt id env.src.theories ;
    env.opened <- 0 ;
  end

let process_close_module env =
  begin
    env.thy <- None ;
    env.opened <- 0 ;
  end

let process_word env s =
  if Docref.is_keyword s then
    begin
      match s with
      | "module" | "theory" -> process_open_module env s
      | "end" when env.opened = 0 -> process_close_module env
      | _ ->
        if Docref.is_opening s then env.opened <- succ env.opened ;
        if Docref.is_closing s then env.opened <- pred env.opened ;
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

      (* skipped *)
      | Token.Eof | Token.Newline
      | Token.Char _ | Token.Text _ | Token.Comment _
      | Token.OpenDoc | Token.CloseDoc
      | Token.OpenSection (_, _) | Token.CloseSection _
      | Token.Space | Token.Style (_, _) | Token.Verb _ -> ()

    done ;
    Token.close input ;
    env.refs
  with exn ->
    Token.close input ;
    raise exn
