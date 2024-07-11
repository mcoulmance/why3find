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
      begin
        Format.printf "Compiling %s@." filename ;
        let loadpath =
          match config_for ~filename with
          | None ->
            Format.printf "No config found.@." ;
            Filename.dirname filename
          | Some(dir,cfg) ->
            Format.printf "Config %s@." cfg ; dir
        in
        Wenv.local ~loadpath () ;
      end

let () = Lspserver.on_save compile
let self = ()
