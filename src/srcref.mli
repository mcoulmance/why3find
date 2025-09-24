(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type symbol =
  | Ref of Id.id
  | Def of Id.id
  | Clone of Id.id * Id.id list
  (* cloned theory, target scope and declared new symbols *)
  | Section of string
  (* foldable section *)

val parse :
  ?doc:bool -> ?filename:string ->
  env:Project.env -> src:Docref.source ->
  string -> symbol Rangemap.t * Lspserver.symbol list
