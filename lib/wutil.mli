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

open Why3

(** All recognized extensions. Typically [".mlw"] and other from plugins. *)
val exts : unit -> string list
val filepath : string -> string * string list (* dir & lib *)
val load_theories : Env.env -> Env.filename -> Theory.theory list * Env.fformat

(** Iterates over declared id. *)
val iter_decl : (Ident.ident -> unit) -> Decl.decl -> unit

(** Iterates over declared id. *)
val iter_tdecl : (Ident.ident -> unit) -> Theory.tdecl -> unit

(** Iterates over declared id. *)
val iter_pdecl : (Ident.ident -> unit) -> Pdecl.pdecl -> unit

(** Iterates over mapped identifiers (source, target). *)
val iter_sm : (Ident.ident -> Ident.ident -> unit) -> Theory.symbol_map -> unit

(** Iterates over mapped identifiers (source, target). *)
val iter_mi : (Ident.ident -> Ident.ident -> unit) -> Pmodule.mod_inst -> unit

val pp_thy : Format.formatter -> Theory.theory -> unit (* name *)
val pp_mod : Format.formatter -> Pmodule.pmodule -> unit (* name *)
val pp_pdecl : Format.formatter -> Pdecl.pdecl -> unit
val pp_tdecl : Format.formatter -> Theory.tdecl -> unit
val pp_munit : Format.formatter -> Pmodule.mod_unit -> unit
val pp_theory : Format.formatter -> Theory.theory -> unit
val pp_module : Format.formatter -> Pmodule.pmodule -> unit

(**************************************************************************)
