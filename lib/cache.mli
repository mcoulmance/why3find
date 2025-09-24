(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val digest : ?prover:Prover.prover -> Why3.Task.task -> string
(** In case a prover is given, the task digest is consolidated with
    the prover driver specification. *)

(** Cache data structure *)
type t

(** [~cdir] is the directory where the cache lives. *)
val create : cdir:string -> ?usecache:bool -> unit -> t

val get : t -> Why3.Task.task -> Prover.prover -> Result.t
val set : t -> Why3.Task.task -> Prover.prover -> Result.t -> unit
