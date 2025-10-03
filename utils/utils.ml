(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Terminal Facilities                                                --- *)
(* -------------------------------------------------------------------------- *)

let tty = Unix.isatty Unix.stdout
let lines = ref 0
let overflows () =
  let size = Option.value ~default:25 @@ Terminal_size.get_rows () in
  !lines + 2 > size

let term =
  match Sys.getenv_opt "TERM" with
  | None | Some "" | Some "dumb" -> false
  | _ -> true

let escape_codes fd =
  term && Unix.isatty fd

type show_progress_mode =
  | Never
  | Auto
  | Always

let show_progress = ref Auto

let set_show_progress = function
  | "never" -> show_progress := Never
  | "auto" -> show_progress := Auto
  | "always" -> show_progress := Always
  | _ -> assert false

let show_progress_opt =
  "--show-progress", Arg.Symbol (["never"; "auto"; "always"], set_show_progress),
  " When to show progress"

let progress msg =
  let width = Option.value (Terminal_size.get_columns ()) ~default:80 in
  let buffer = Buffer.create (max 80 width) in
  Format.kfprintf
    (fun fmt ->
       Format.pp_print_flush fmt () ;
       if !show_progress = Always
       || !show_progress <> Never && escape_codes Unix.stdout then
         let msg = Buffer.contents buffer in
         let len = String.length msg in
         if 3 + len <= width then
           Printf.printf "> %s\027[K\r%!" msg
         else
           Printf.printf "> %s…\027[K\r%!" @@
           String.sub msg 0 (max 0 (width - 3))
    ) (Format.formatter_of_buffer buffer) msg

let flush () =
  Format.pp_print_flush Format.std_formatter () ;
  Format.pp_print_flush Format.err_formatter () ;
  if escape_codes Unix.stdout then Printf.printf "\r\027[K%!"

let nop _ = ()

let mark_open_stag tag =
  let open Format in
  match tag with
  | String_tag "bold" -> "\027[1m"
  | String_tag "magenta" -> "\027[95m"
  | String_tag "bright red" -> "\027[91m"
  | String_tag "gray" -> "\027[37m"
  | String_tag "green" -> "\027[32m"
  | String_tag "orange" -> "\027[33m"
  | String_tag "red" -> "\027[31m"
  | _ -> ""

let mark_close_stag tag =
  let open Format in
  match tag with
  | String_tag "bold" -> "\027[22m"
  | String_tag ("magenta"|"bright red"|"gray"|"green"|"orange"|"red") -> "\027[39m"
  | _ -> ""

let set_ansi_tags fmt =
  begin
    Format.pp_set_tags fmt true ;
    Format.pp_set_formatter_stag_functions fmt {
      mark_open_stag ;
      mark_close_stag ;
      print_open_stag = nop ;
      print_close_stag = nop ;
    }
  end

let set_tty std fmt =
  if escape_codes std then
    begin
      let { Format.out_newline } as ff =
        Format.pp_get_formatter_out_functions fmt () in
      Format.pp_set_formatter_out_functions fmt {
        ff with out_newline = (fun () -> incr lines ; out_newline ())
      } ;
      set_ansi_tags fmt ;
    end

let () = set_tty Unix.stdout Format.std_formatter
let () = set_tty Unix.stderr Format.err_formatter

(* -------------------------------------------------------------------------- *)
(* --- System Utils                                                       --- *)
(* -------------------------------------------------------------------------- *)

module F = Filename

let readdir f p =
  let ds = Sys.readdir p in
  Array.sort String.compare ds ;
  Array.iter f ds

let rec iterpath ?(enter=ignore) ?(file=ignore) ?(leave=ignore)
    ?(ignored=Fun.const false) p =
  if not (ignored p) then
    if Sys.file_exists p then
      if Sys.is_directory p then
        begin
          enter p ;
          readdir
            (fun d ->
               let pd =
                 if p = Filename.current_dir_name then d else
                   Filename.concat p d
               in iterpath ~enter ~file ~leave ~ignored pd ;
            ) p ;
          leave p ;
        end
      else file p

let rmpath p = iterpath ~file:Sys.remove ~leave:Sys.rmdir p

let rec mkdirs = function
  | "/" | "." -> ()
  | path ->
    if not (Sys.file_exists path) then
      begin
        mkdirs (F.dirname path) ;
        Sys.mkdir path 0o755 ;
      end

let copy ~src ~tgt =
  mkdirs (F.dirname tgt) ;
  let buffer = Bytes.create 2048 in
  let inc = open_in src in
  let out = open_out tgt in
  let rec walk () =
    let n = Stdlib.input inc buffer 0 (Bytes.length buffer) in
    if n > 0 then
      ( Stdlib.output out buffer 0 n ; walk () )
  in walk () ; close_in inc ; close_out out

let rec lookup ~dir ~file ~path =
  match dir with
  | "/" | "." -> None
  | _ ->
    if Sys.file_exists (F.concat dir file)
    then Some (dir,path)
    else lookup ~file
        ~dir:(F.dirname dir)
        ~path:(F.concat (F.basename dir) path)

let locate file = lookup ~dir:(Sys.getcwd()) ~path:"" ~file
let is_local ~root ~file =
  let rec check f =
    let d = Filename.dirname f in
    d = root || (d <> "/" && d <> "." && check d)
  in check file

let chdir dir =
  let pwd = Sys.getcwd () in
  Sys.chdir dir ;
  let newdir = Sys.getcwd () in
  if pwd <> newdir then
    begin
      Format.printf "Entering directory '%s'@." newdir ;
      Stdlib.at_exit
        begin fun () ->
          flush () ;
          Format.printf "Leaving directory '%s'@." newdir
        end
    end
let absolute file =
  if Filename.is_relative file
  then Filename.concat (Sys.getcwd ()) file
  else file

let path ?root file =
  if Filename.is_relative file then
    match root with
    | None -> file
    | Some d -> Filename.concat d file
  else file

let load ~file buffer =
  let inc = open_in file in
  try
    while true do
      Buffer.add_channel buffer inc 2048
    done
  with End_of_file -> close_in inc

let output_and_close out output =
  match output out with
  | () -> close_out out
  | exception e ->
    let bt = Printexc.get_raw_backtrace () in
    close_out_noerr out;
    Printexc.raise_with_backtrace e bt

let outputfile ~file output =
  let out = open_out file in
  output_and_close out output

let writefile ~file data =
  outputfile ~file (fun out -> output_string out data)

let readfile ~file =
  let buffer = Buffer.create 2048 in
  load ~file buffer ; Buffer.contents buffer

let formatfile ~file pp =
  outputfile ~file begin fun out ->
    let fmt = Format.formatter_of_out_channel out in
    pp fmt ; Format.pp_print_flush fmt ()
  end

let input ~file fn =
  let inc = open_in file in
  try let r = fn inc in close_in inc ; r
  with exn -> close_in inc ; raise exn

let output ~file fn =
  let out = open_out file in
  try let r = fn out in close_out out ; r
  with exn -> close_out out ; raise exn

(* -------------------------------------------------------------------------- *)
(* --- Time Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

let round t =
  if t < 1e-3 then Float.round (t *. 1e4) *. 1e-4 else
  if t < 1.0 then Float.round (t *. 1e3) *. 1e-3 else
  if t < 20.0 then Float.round (t *. 1e1) *. 1e-1 else
    Float.round t

let pp_rtime fmt t =
  if t < 1e-3 then Format.fprintf fmt "%3dns" (int_of_float @@ t *. 1e6) else
  if t < 1.0 then Format.fprintf fmt "%3dms" (int_of_float @@ t *. 1e3) else
  if t < 20.0 then Format.fprintf fmt "%4.1fs" t else
  if t < 60.0 then Format.fprintf fmt "%4ds" (int_of_float t) else
  if t < 3600.0 then Format.fprintf fmt "%2dmin" (int_of_float @@ t /. 60.0) else
    Format.fprintf fmt "%3dh" (int_of_float @@ t /. 3600.0)

let pp_atime fmt t =
  if t < 1e-3 then Format.fprintf fmt "%dns" (int_of_float @@ t *. 1e6) else
  if t < 1.0 then Format.fprintf fmt "%dms" (int_of_float @@ t *. 1e3) else
  if t < 20.0 then Format.fprintf fmt "%.1fs" t else
  if t < 60.0 then Format.fprintf fmt "%ds" (int_of_float t) else
  if t < 3600.0 then Format.fprintf fmt "%dmin" (int_of_float @@ t /. 60.0) else
    Format.fprintf fmt "%dh" (int_of_float @@ t /. 3600.0)

let pp_time fmt t =
  if t < 0.0 then Format.pp_print_char fmt '-' ;
  pp_atime fmt (abs_float t)

let pp_delta fmt t =
  Format.pp_print_char fmt (if t < 0.0 then '-' else '+') ;
  pp_atime fmt (abs_float t)

let pa_time s =
  let n = String.length s in
  if String.ends_with ~suffix:"min" s then
    60.0 *. (float @@ int_of_string @@ String.sub s 0 (n-3))
  else
  if n > 1 then
    match s.[n-1] with
    | 'h' -> 3600.0 *. (float @@ int_of_string @@ String.sub s 0 (n-1))
    | 's' ->
      begin
        match s.[n-2] with
        | 'm' -> 1.e-3 *. (float @@ int_of_string @@ String.sub s 0 (n-2))
        | 'n' -> 1.e-6 *. (float @@ int_of_string @@ String.sub s 0 (n-2))
        | _ -> float @@ int_of_string @@ String.sub s 0 (n-1)
      end
    | _ -> float_of_string s
  else float_of_string s

(* -------------------------------------------------------------------------- *)
(* --- Pretty Printing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let pp_hex fmt hs =
  String.iter (fun c -> Format.fprintf fmt "%02x" @@ Char.code c) hs

let pp_arg fmt arg =
  let arg = String.escaped arg in
  if String.length arg <= 8 then
    Format.fprintf fmt " %-8s |" arg
  else
    Format.fprintf fmt " %s… |" (String.sub arg 0 7)

let pp_args fmt args = List.iter (pp_arg fmt) args

let pp_ok fmt = Format.fprintf fmt "@{<green>\u{2714}@}"
let pp_ko fmt = Format.fprintf fmt "@{<red>\u{2718}@}"
let pp_mark fmt b = if b then pp_ok fmt else pp_ko fmt
let pp_s fmt n = if n > 1 then Format.pp_print_char fmt 's'
let pp_yies fmt n =
  Format.pp_print_string fmt (if n > 1 then "ies" else "y")

let to_string pp v =
  begin
    let buffer = Buffer.create 80 in
    let fmt = Format.formatter_of_buffer buffer in
    pp fmt v ;
    Format.pp_print_flush fmt () ;
    Buffer.contents buffer
  end

(* -------------------------------------------------------------------------- *)
(* --- Failures                                                           --- *)
(* -------------------------------------------------------------------------- *)

let failwith msg = Format.kasprintf Stdlib.failwith msg

(* -------------------------------------------------------------------------- *)
