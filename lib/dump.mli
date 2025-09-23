(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Dump AST                                                           --- *)
(* -------------------------------------------------------------------------- *)

open Why3
type 'a printer = Format.formatter -> 'a -> unit

val pp_id : Ident.ident printer

val pp_term : Term.term printer

val pp_pvsymbol_use : Ity.pvsymbol printer
val pp_pvsymbol_def : Ity.pvsymbol printer

val pp_ity : Ity.ity printer
val pp_cty : Ity.cty printer

val pp_rs_kind : Expr.rs_kind printer
val pp_rsymbol_use : Expr.rsymbol printer
val pp_rsymbol_def : Expr.rsymbol printer

val pp_expr : Expr.expr printer
val pp_cexp : Expr.cexp printer

(* -------------------------------------------------------------------------- *)
