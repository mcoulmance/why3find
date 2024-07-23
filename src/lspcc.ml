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
  mutable updates : Lspserver.update list ;
}

let sources = Hashtbl.create 0

let set_source ~filename env src =
  Hashtbl.replace sources filename { env ; src ; updates = [] }

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

let range_of_loc (loc : Why3.Loc.position) : string * Lspserver.range =
  let file,l1,c1,l2,c2 = Why3.Loc.get loc in
  file , {
    start = { line = l1 - 1 ; char = c1 } ;
    stop = { line = l2 - 1 ; char = c2 }
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
    Why3.Loc.set_warning_hook
      begin fun ?loc message ->
        match loc with
        | None -> Format.printf "[Warning] %s@." message
        | Some loc ->
          let file,range = range_of_loc loc in
          if file = filename then
            let severity = `Warning in
            let diag = Lspserver.diagnostic ~range ~severity ~message () in
            diagnostics := diag :: !diagnostics
          else
            Format.printf "[Warning] File %a: %s@."
              Why3.Loc.pp_position loc message
      end ;
    let loadpath =
      match config_for ~filename with
      | None ->
        Format.printf "File %s (no config)@." filename ;
        Filename.dirname filename
      | Some(dir,cfg) ->
        Format.printf "Config %s@." cfg ;
        Format.printf "Source %s@." filename ; dir
    in
    let tmp,out = Filename.open_temp_file "lsp" ".mlw" in
    Ropes.to_channel out text ;
    close_out out ;
    Wenv.local ~loadpath () ;
    let env = Wenv.init () in
    let cenv = Docref.init () in
    let henv = Axioms.init env in
    let lib = library ~loadpath ~filename in
    let inc = open_in tmp in
    try
      let src = Docref.source ~wenv:env.wenv ~cenv ~henv ~lib ~filename inc in
      set_source ~filename env src ;
      close_in inc ;
    with exn ->
      close_in inc ;
      Hashtbl.remove sources filename ;
      raise exn
  with
  | Why3.Loc.Located(loc,exn) ->
    let file,range = range_of_loc loc in
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
(* --- Compute References                                                 --- *)
(* -------------------------------------------------------------------------- *)
