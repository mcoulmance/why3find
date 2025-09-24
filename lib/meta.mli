(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- META Package Infos                                                 --- *)
(* -------------------------------------------------------------------------- *)

type pkg = {
  name: string ;
  path: string ;
  depends: string list ;
  drivers: string list ;
  extracted: bool ;
}


(** [shared f] returns the path of file [f] installed in shared directory. *)
val shared : string -> string

(** [path pkg] returns the topmost installation path of package [pkg]. *)
val path : string -> string

(** [find pkg] returns the installed package META data, if installed. *)
val find : string -> pkg

(** [find_all pkgs] returns all listed packages and their dependencies,
    in dependency order. *)
val find_all : string list -> pkg list

(** [install pkg] install the package META data. *)
val install : pkg -> unit

(* -------------------------------------------------------------------------- *)
