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

type tactic = Tac of string [@@ ocaml.unboxed]
let name = function Tac id -> id
let pretty fmt = function Tac id -> Format.pp_print_string fmt id

let lookup (env : Why3.Env.env) tactic =
  try
    match Why3.Trans.lookup_trans env tactic with
    | Trans_one _ | Trans_list _ -> Some (Tac tactic)
    | Trans_with_args _ | Trans_with_args_l _ ->
      Log.warning "Transformation '%s' requires arguments (skipped)" tactic ;
      None
    with Why3.Trans.UnknownTrans tname ->
      Log.warning "Unknown transformation '%s' (skipped)" tname ;
      None

let select env = List.filter_map @@ lookup env
