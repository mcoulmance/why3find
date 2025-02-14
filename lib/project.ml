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

type env = {
  config : Config.config ;
  wenv : Config.wenv ;
  pkgs : Meta.pkg list ;
}

let create_env ?(root=".") ~config () =
  let pkgs = Meta.find_all config.Config.packages in
  let pkg_path = List.map (fun m -> m.Meta.path) pkgs in
  let wconfig = Why3.Whyconf.init_config ~extra_config:[] None in
  let wenv = Config.create_wenv ~loadpath:(root :: pkg_path) wconfig in
  { config ; wenv ; pkgs }
