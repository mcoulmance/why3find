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

type transport =
  | PIPE of string
  | PORT of int

type channel

(** Establish the server or exit *)
val establish: transport -> channel

(** Process I/O (non-blocking) *)
val yield: channel -> unit

(** Process (at most, non-blocking) one pending LSP Request & Response *)
val process: channel -> unit

(** Register a request or notification handler *)
val register: string -> (Json.t -> Json.t) -> unit

(** Notify the server *)
val notify: channel -> string -> Json.t -> unit
