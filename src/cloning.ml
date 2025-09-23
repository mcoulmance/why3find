(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
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
  | C_inst of int * export * Token.position * Id.t
  (* [tab] clone [export] id [as id] *)
  | C_axiom                (* clone … with … axiom [….] *)
  | C_with                 (* clone … with *)
  | C_decl                 (* clone … with … kwd *)
  | C_seq                  (* clone … with … kwd a = *)
  | C_def                  (* clone … with … [kwd a = b | axiom a] *)
  | C_src of Id.t * int    (* clone … with … kwd a<nspace> *)

type token = [
  | `Id of Id.t
  | `Kwd of string
  | `Name of string
  | `Equal
  | `Coma
  | `Dot
  | `Comment
  | `Newline
]

type action = [
  | `Nop
  | `Space
  | `Spaces of int
  | `Target of Id.t * int
  | `Clone of int * export * Token.position * Id.t
]

let init = NotCloning

let space state =
  match state with
  | C_inst _ -> Some state
  | C_src(id,n) -> Some (C_src(id,succ n))
  | _ -> None

let process state input token : state * action =
  match state, token with
  | C_clone k, `Kwd "export" -> C_export k , `Nop
  | C_clone k, `Id a -> C_inst(k,Cloned,Token.position input,a) , `Nop
  | C_export k, `Id a -> C_inst(k,Exported,Token.position input,a) , `Nop
  | C_inst(k,_,p,id) , `Kwd "as" -> C_inst(k,Aliased "?",p,id) , `Space
  | C_inst(k,_,_,id) , `Name a -> C_inst(k,Aliased a,Token.position input,id) , `Space
  | C_inst(k,e,p,id) , `Kwd "with" -> C_with , `Clone(k,e,p,id)
  | C_inst(k,e,p,id) , `Kwd "clone" -> C_clone (Token.indent input) , `Clone(k,e,p,id)
  | C_inst(k,e,p,id) , (`Comment | `Newline | `Kwd _) -> NotCloning , `Clone(k,e,p,id)
  | _ , `Kwd "clone" -> C_clone (Token.indent input) , `Nop
  | C_with , `Kwd "axiom" -> C_axiom , `Nop
  | C_with , `Kwd _ -> C_decl , `Nop
  | C_axiom , `Id _ -> C_def , `Nop
  | C_axiom , `Dot -> C_axiom , `Nop
  | C_axiom , `Coma -> C_with , `Nop
  | C_decl , `Dot -> C_decl , `Nop
  | C_decl , `Id a -> C_src(a,0) , `Nop
  | C_src(_,n) , `Equal -> C_seq , `Spaces n
  | C_src(id,n) , `Coma -> C_with , `Target(id,n)
  | C_src(id,n) , `Comment -> C_def , `Target(id,n)
  | C_src(id,_) , `Newline -> C_def , `Target(id,0)
  | C_seq , `Dot -> C_seq , `Nop
  | C_seq , (`Id _ | `Name _) -> C_def , `Nop
  | C_def , `Coma -> C_with , `Nop
  | _ , `Kwd _ -> NotCloning , `Nop
  | _ -> state , `Nop
