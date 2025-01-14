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

val digest : ?prover:Prover.prover -> Why3.Task.task -> string
(** In case a prover is given, the task digest is consolidated with
    the prover driver specification. *)

(** Cache data structure *)
type t

(** [~cdir] is the directory where the cache lives. *)
val create : cdir:string -> ?usecache:bool -> unit -> t

val get : t -> Why3.Task.task -> Prover.prover -> Result.t
val set : t -> Why3.Task.task -> Prover.prover -> Result.t -> unit
