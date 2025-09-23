(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* A tactic is an existing transformation without parameters *)

type tactic = Tac of string [@@ ocaml.unboxed]
let name = function Tac id -> id
let equal (Tac a) (Tac b) = String.equal a b
let pretty fmt = function Tac id -> Format.pp_print_string fmt id

let is_tactic = function
  | Why3.Trans.Trans_one _ | Trans_list _ -> true
  | Trans_with_args _ | Trans_with_args_l _ -> false

let warn_tactic_not_found =
  Why3.Wstdlib.Hstr.memo 5 @@
  fun s -> Log.warning "Tactic %s not found (skipped)" s

let warn_not_a_tactic =
  Why3.Wstdlib.Hstr.memo 5 @@
  fun s -> Log.warning "Transformation %s is not a tactic@ \
                        (requires arguments, skipped)" s

let lookup (env : Why3.Env.env) tr =
  try
    if is_tactic @@ Why3.Trans.lookup_trans env tr then
      Some (Tac tr)
    else
      (warn_not_a_tactic tr ; None)
  with Why3.Trans.UnknownTrans tname ->
    warn_tactic_not_found tname ; None

let select env = List.filter_map @@ lookup env

let iter fn =
  let iterd = List.iter (fun (t,d) -> fn t d) in
  iterd @@ Why3.Trans.list_transforms () ;
  iterd @@ Why3.Trans.list_transforms_l ()
