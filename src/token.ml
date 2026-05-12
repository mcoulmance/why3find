(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Documentation Generator Tokens                                     --- *)
(* -------------------------------------------------------------------------- *)

type position = Lexing.position * Lexing.position

type style =
  | Head
  | Emph
  | Bold
  | Dash
  | Ordered

type token =
  | Eof
  | Newline
  | Char of char
  | Text of string
  | Comment of string
  | Infix of string
  | Ident of string
  | OpenDoc
  | CloseDoc
  | OpenSection of bool * string
  | CloseSection of string
  | Space
  | Style of style * string
  | Ref of string
  | Verb of string

type context = Src | Doc | End

type input = {
  lexbuf : Lexing.lexbuf;
  channel : in_channel;
  mutable newlines : int; (* number of consecutive \n (space excluded) *)
  mutable tokcount : int; (* number of tokens on the line *)
  mutable context : context;
}

let src_lexer = ref (fun _ -> assert false)
let doc_lexer = ref (fun _ -> assert false)

let input ?(doc=false) ?(filename) file =
  let inc = open_in file in
  let lexbuf = Lexing.from_channel inc in
  Lexing.set_filename lexbuf @@ Option.value ~default:file filename ;
  {
    lexbuf ;
    channel = inc ;
    context = if doc then Doc else Src;
    tokcount = 0;
    newlines = 0;
  }

let close input =
  close_in_noerr input.channel ;
  input.context <- End

let handler : (input -> string -> unit) ref =
  ref begin fun input msg ->
    let p = Lexing.lexeme_start_p input.lexbuf in
    let q = Lexing.lexeme_end_p input.lexbuf in
    let a = p.pos_cnum - p.pos_bol in
    let b = q.pos_cnum - p.pos_bol in
    Format.printf "File \"%s\", line %d, characters %d-%d: %s@."
      p.pos_fname p.pos_lnum a b msg ;
    exit 1
  end

let set_handler fn = handler := fn

let error input msg =
  Format.kasprintf (fun msg -> !handler input msg ; raise Exit) msg

let eof input = input.context = End
let src input = input.context = Src
let doc input = input.context = Doc
let emptyline input = input.newlines > 1
let startline input = input.tokcount <= 1

let indent input =
  let Lexing.{ pos_cnum = c ; pos_bol = b } =
    Lexing.lexeme_start_p input.lexbuf
  in c - b

let position input =
  Lexing.lexeme_start_p input.lexbuf ,
  Lexing.lexeme_end_p input.lexbuf

let line input =
  (Lexing.lexeme_start_p input.lexbuf).pos_lnum

let fetch input =
  match input.context with
  | End -> Eof
  | Src -> !src_lexer input.lexbuf
  | Doc -> !doc_lexer input.lexbuf

let spaces input tk =
  begin
    match tk with
    | Space -> ()
    | Newline ->
      input.tokcount <- 0 ;
      input.newlines <- succ input.newlines ;
    | _ ->
      input.tokcount <- succ input.tokcount ;
      input.newlines <- 0 ;
  end

let context input tk =
  match tk with
  | OpenDoc -> input.context <- Doc
  | CloseDoc -> input.context <- Src
  | Eof -> input.context <- End
  | _ -> ()

let token input =
  let tk = fetch input in
  begin
    spaces input tk ;
    context input tk ;
  end ; tk

let rec fetch_id ~kind input =
  match token input with
  | Space | Newline -> fetch_id ~kind input
  | Ident s -> s
  | _ -> error input "missing %s name" kind

let pp fmt = function
  | Eof -> Format.pp_print_string fmt "Eof"
  | Space -> Format.pp_print_string fmt "Space"
  | Newline -> Format.pp_print_string fmt "Newline"
  | Char c -> Format.fprintf fmt "Char %S" (String.make 1 c)
  | Text s -> Format.fprintf fmt "Text %S" s
  | Comment s -> Format.fprintf fmt "Comment %S" s
  | Infix s -> Format.fprintf fmt "Infix %s" s
  | Ident s -> Format.fprintf fmt "Ident %s" s
  | OpenDoc -> Format.pp_print_string fmt "OpenDoc \"(**\""
  | CloseDoc -> Format.pp_print_string fmt "CloseDoc \"*)\""
  | OpenSection (o,s) -> Format.fprintf fmt "OpenSection %c%s" (if o then '+' else '-') s
  | CloseSection s -> Format.fprintf fmt "CloseSection %s" s
  | Style (_sty, s) -> Format.fprintf fmt "Styled %S" s
  | Ref s -> Format.fprintf fmt "Ref %S" s
  | Verb s -> Format.fprintf fmt "Verb %S" s

(* -------------------------------------------------------------------------- *)
