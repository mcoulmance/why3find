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

(* Compiler for the LSP Server *)

let config_for ~filename =
  let rec lookup = function
    | "/" | "." -> None
    | dir ->
      let config = Filename.concat dir "why3find.json" in
      if Sys.file_exists config
      then Some (dir,config)
      else lookup (Filename.dirname dir)
  in lookup (Filename.dirname filename)

let compile (doc: Lspserver.document) =
  match Lspserver.to_file ~uri:doc.uri with
  | None -> ()
  | Some filename ->
    if Filename.check_suffix filename ".mlw" then
      try
        Format.printf "Compiling %s@." filename ;
        let loadpath =
          match config_for ~filename with
          | None ->
            Format.printf "No config found.@." ;
            Filename.dirname filename
          | Some(dir,cfg) ->
            Format.printf "Using %s@." cfg ; dir in
        let tmp,out = Filename.open_temp_file "lsp" ".mlw" in
        Ropes.to_channel out doc.text ;
        close_out out ;
        Wenv.local ~loadpath () ;
        let env = Wenv.init () in
        let inc = open_in tmp in
        let theories =
          Why3.Env.read_channel Why3.Env.base_language env.wenv filename inc in
        Why3.Wstdlib.Mstr.iter
          (fun tname _thy ->
             Format.printf " - Theory %s@\n" tname
          ) theories ;
        Format.printf "Done.@." ;
      with exn ->
        Format.printf "[EXN] %s@." (Printexc.to_string exn) ;
        Format.printf "[ERROR] %a@." Why3.Exn_printer.exn_printer exn

let () = Lspserver.on_save compile
let self = ()
