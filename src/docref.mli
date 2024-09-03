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
(* --- Global References                                                  --- *)
(* -------------------------------------------------------------------------- *)

val init : unit -> unit

module Thy = Why3.Theory
module Pmod = Why3.Pmodule
module Mstr = Why3.Wstdlib.Mstr

type clone =
  | Inst of {
      line : int ; (* last known line of declaration *)
      order : int ; (* instance order *)
      cloned : Thy.theory ;
    }
  | Clone of {
      source : Id.t ; (* from cloned theory *)
      target : Id.t ; (* to clone instance *)
    }

val line : clone -> int
val by_line : clone -> clone -> int

type instance = Instance of {
    line : int ; (* line of last declaration *)
    order : int ; (* clone rank in the module *)
    container : string ; (* container module (or theory) *)
    scope : string list ; (* inside container module clone *)
    cloned : Thy.theory ; (* cloned theory *)
    clones : clone list ; (* clone substitution *)
  }

type instances
val find_clone : ?instance:instance -> ?source:Id.t -> ?target:Id.t -> unit -> clone option
val find_instance : instances -> scope:string list -> Id.t -> instance option
val iter_instances : (instance -> unit) -> instances -> unit

type theory = {
  path: string ;
  theory: Thy.theory ;
  depends: Thy.theory list ;
  signature: Axioms.signature ;
  instances: instances ;
  iclones: clone list ; (* sorted instances (by line, only [Inst ]) *)
  mutable proofs: Crc.crc Mstr.t ;
}

type source = {
  root: string option;
  filename: string;
  lib: string list;
  urlbase: string;
  profile: Calibration.profile;
  theories: theory Mstr.t;
}

val library_path : string -> string list

val parse : wenv:Why3.Env.env -> henv:Axioms.henv -> string -> source

val source : wenv:Why3.Env.env -> henv:Axioms.henv ->
  lib:string list -> ?root:string -> filename:string ->
  in_channel -> source

val empty : unit -> source
val derived : source -> string -> string (* URL name *)

val is_keyword : string -> bool
val is_opening : string -> bool
val is_closing : string -> bool

type href =
  | NoRef
  | Ref of Id.id
  | Def of Id.id * Crc.crc option

type position = Lexing.position * Lexing.position

val find_proof : Id.t -> theory option -> Crc.crc option
val reload_proofs : source -> unit

val resolve :
  src:source -> theory:theory option -> infix:bool ->
  position -> href

val reference :
  wenv:Why3.Env.env ->
  src:source -> current:string option ->
  string -> string * Id.t

val find_decl : Why3.Env.env -> source -> Id.id -> Id.decl

(* -------------------------------------------------------------------------- *)
