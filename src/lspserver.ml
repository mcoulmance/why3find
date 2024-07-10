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
(* --- LSP Initialize                                                     --- *)
(* -------------------------------------------------------------------------- *)

let () = Lsp.register ~force:true "initialize"
    begin fun _channel _data ->
      `Assoc [
        "capabilities", `Assoc [
          "textDocumentSync", `Assoc [
            "openClose", `Bool true ;
            "change", `Int 2 ;
            "save", `Bool true ;
          ]
        ] ;
        "serverInfo", `Assoc [
          "name", `String "why3find" ;
          "version", `String Config.version ;
        ]
      ]
    end

(* -------------------------------------------------------------------------- *)
(* --- Documents                                                          --- *)
(* -------------------------------------------------------------------------- *)

type document = {
  uri: string ;
  lang: string ;
  text: Ropes.t ;
}

(* Indexed by uri *)
let documents : (string,document) Hashtbl.t = Hashtbl.create 0

type position = { line: int ; char : int }
type range = { start: position ; stop: position }
type change = Text of string | Update of range * string

let to_pos { line ; char } = line,char
let to_range { start ; stop } = to_pos start, to_pos stop

let jposition js = {
  line = Json.jfield "line" js |> Json.jint ;
  char = Json.jfield "character" js |> Json.jint ;
}

let wposition { line ; char } : Json.t =
  `Assoc [ "line", `Int line ; "character", `Int char ]

let jrange js = {
  start = Json.jfield "start" js |> jposition ;
  stop =  Json.jfield "end" js |> jposition ;
}

let wrange { start ; stop } : Json.t =
  `Assoc [ "start", wposition start ; "end", wposition stop ]
[@@ warning "-32"]

let jchange js =
  let text = Json.jfield "text" js |> Json.jstring in
  match Json.jfield "range" js |> Json.joption jrange with
  | None -> Text text
  | Some range -> Update(range,text)

let apply doc update =
  let text =
    match update with
    | Text text -> Ropes.of_string text
    | Update(range,text) -> Ropes.update (to_range range) text doc.text
  in { doc with text }

(* -------------------------------------------------------------------------- *)
(* --- Document Synchronization                                           --- *)
(* -------------------------------------------------------------------------- *)

let () = Lsp.register "textDocument/didOpen"
    begin fun _channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      let lang = Json.jfield "languageId" doc |> Json.jstring in
      let text = Json.jfield "text" doc |> Json.jstring |> Ropes.of_string in
      Hashtbl.replace documents uri { uri ; lang ; text } ; `Null
    end

let () = Lsp.register "textDocument/didClose"
    begin fun _channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      Hashtbl.remove documents uri ; `Null
    end

let () = Lsp.register "textDocument/didChange"
    begin fun _channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      let changes = Json.jfield "contentChanges" data |> Json.jmap jchange in
      let doc = Hashtbl.find documents uri in
      let newDoc = List.fold_left apply doc changes in
      Hashtbl.replace documents uri newDoc ; `Null
    end

let () = Lsp.register "textDocument/didSave"
    begin fun _channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      Format.eprintf "Saved %S@." uri ;
      `Null
    end

(* -------------------------------------------------------------------------- *)
(* --- Launch Server                                                      --- *)
(* -------------------------------------------------------------------------- *)

let run transport =
  let channel = Lsp.establish transport in
  while Lsp.running channel do Lsp.process channel done

(* -------------------------------------------------------------------------- *)
