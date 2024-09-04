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
(* --- Cloning Substitution                                               --- *)
(* -------------------------------------------------------------------------- *)

type export = Cloned | Exported | Aliased of string

let instance ~cloned ~scope = function
  | Cloned -> cloned.Why3.Ident.id_string :: scope
  | Exported -> scope
  | Aliased a -> a :: scope

type state =
  | NotCloning
  | C_clone of int         (* [tab] clone *)
  | C_export of int        (* [tab] clone export *)
  | C_inst of int * export * Id.t (* [tab] clone [export] id [as id] *)
  | C_axiom                (* clone … with … axiom [….] *)
  | C_with                 (* clone … with *)
  | C_decl                 (* clone … with … kwd *)
  | C_seq                  (* clone … with … kwd a = *)
  | C_def                  (* clone … with … [kwd a = b | axiom a] *)
  | C_src of Id.t * int    (* clone … with … kwd a<nspace> *)

type token = [
  | `id of Id.t
  | `kwd of string
  | `name of string
  | `equal
  | `coma
  | `dot
  | `comment
  | `newline
]

type action = [
  | `nop
  | `space
  | `spaces of int
  | `target of Id.t * int
  | `clone of int * export * Id.t
]

let init = NotCloning

let space state =
  match state with
  | C_inst _ -> Some state
  | C_src(id,n) -> Some (C_src(id,succ n))
  | _ -> None

let process state input token =
  match state, token with
  | C_clone k, `kwd "export" -> C_export k , `nop
  | C_clone k, `id a -> C_inst(k,Cloned,a) , `nop
  | C_export k, `id a -> C_inst(k,Exported,a) , `nop
  | C_inst(k,_,id) , `kwd "as" -> C_inst(k,Aliased "?",id) , `space
  | C_inst(k,_,id) , `name a -> C_inst(k,Aliased a,id) , `space
  | C_inst(k,e,id) , `kwd "with" -> C_with , `clone(k,e,id)
  | C_inst(k,e,id) , `kwd "clone" -> C_clone (Token.indent input) , `clone(k,e,id)
  | C_inst(k,e,id) , (`comment | `newline | `kwd _) -> NotCloning , `clone(k,e,id)
  | _ , `kwd "clone" -> C_clone (Token.indent input) , `nop
  | C_with , `kwd "axiom" -> C_axiom , `nop
  | C_with , `kwd _ -> C_decl , `nop
  | C_axiom , `id _ -> C_def , `nop
  | C_axiom , `dot -> C_axiom , `nop
  | C_axiom , `coma -> C_with , `nop
  | C_decl , `dot -> C_decl , `nop
  | C_decl , `id a -> C_src(a,0) , `nop
  | C_src(_,n) , `equal -> C_seq , `spaces n
  | C_src(id,n) , `coma -> C_with , `target(id,n)
  | C_src(id,n) , `comment -> C_def , `target(id,n)
  | C_src(id,_) , `newline -> C_def , `target(id,0)
  | C_seq , `dot -> C_seq , `nop
  | C_seq , (`id _ | `name _) -> C_def , `nop
  | C_def , `coma -> C_with , `nop
  | _ , `kwd _ -> NotCloning , `nop
  | _ -> state , `nop
