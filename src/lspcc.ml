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

let compile filename text diagnostics =
  try
    Format.printf "Compiling %s@." filename ;
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
        Format.printf "No config found.@." ;
        Filename.dirname filename
      | Some(dir,cfg) ->
        Format.printf "Using %s@." cfg ; dir in
    let tmp,out = Filename.open_temp_file "lsp" ".mlw" in
    Ropes.to_channel out text ;
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
  let Lspserver.{ uri ; text } = doc in
  match Lspserver.to_file ~uri with
  | None -> ()
  | Some filename ->
    if Filename.check_suffix filename ".mlw" then
      begin
        let diagnostics = ref [] in
        compile filename text diagnostics ;
        Lspserver.publish_diagnostic channel ~uri !diagnostics
      end

let () = Lspserver.on_open compile
let () = Lspserver.on_save compile
