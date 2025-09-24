(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
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
