(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Compute Parameterized Module Soundness *)
(* -------------------------------------------------------------------------- *)

type env

val init : unit -> env
val register : env -> Docref.source -> unit

type soundness =
  | Unsound
  | Sound of Docref.instance list
  | Unknown of Docref.instance list

val pretty : Format.formatter -> soundness -> unit

val free : soundness (* sound with no instance *)
val unknown : soundness (* unknown with any number of missing instances *)
val merge : soundness -> soundness -> soundness

val is_free : soundness -> bool
val is_sound : soundness -> bool
val is_unsound : soundness -> bool
val compute : env -> Docref.theory -> soundness
val instance : env -> Docref.instance -> soundness

(* -------------------------------------------------------------------------- *)
