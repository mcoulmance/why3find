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

(* -------------------------------------------------------------------------- *)
(* ---  Task Colorization                                                 --- *)
(* -------------------------------------------------------------------------- *)

module Loc = Why3.Loc
module Term = Why3.Term
module Decl = Why3.Decl
module Thy = Why3.Theory
module Tsk = Why3.Task

type kind = Goal | When | WhenNot
type shape =
  | Empty
  | Cat of shape * shape (* each is non-empty *)
  | Range of kind * Loc.position

(* Collects all colored parts the two shapes *)
let (<|>) (a : shape) (b : shape) : shape =
  match a,b with
  | Empty,c | c,Empty -> c
  | _ -> Cat(a,b)

(* Combines a and b only if they are both colored shapes *)
let (<*>) (a : shape) (b : shape) : shape =
  match a,b with
  | Empty,_ | _,Empty -> Empty
  | _ -> Cat(a,b)

(* -------------------------------------------------------------------------- *)
(* ---  Range Extraction                                                  --- *)
(* -------------------------------------------------------------------------- *)

let range kind = function None -> Empty | Some p -> Range(kind,p)

let apply kind loc = function Empty -> range kind loc | _ as shape -> shape

let rec collect kind (t : Term.term) =
  apply kind t.t_loc @@
  Term.t_fold (fun rgs t -> rgs <*> collect kind t) Empty t

let rec hyp (t : Term.term) =
  apply When t.t_loc @@
  match t.t_node with
  | Tbinop(Tand,a,b) -> hyp a <*> hyp b
  | Term.Tlet (_,fb) ->
    let _,t = Term.t_open_bound fb in hyp t
  | Tnot p -> collect WhenNot p
  | _ -> collect When t

let rec goal (t : Term.term) =
  apply Goal t.t_loc @@
  match t.t_node with
  | Tbinop(Timplies,h,p) -> hyp h <*> goal p
  | Tquant(Tforall,bnd) ->
    let _,_,p = Term.t_open_quant bnd in goal p
  | Term.Tlet (_,fb) ->
    let _,t = Term.t_open_bound fb in goal t
  | _ -> collect Goal t

let decl (d: Decl.decl) : shape =
  match d.d_node with
  | Dprop(Pgoal,pr,f) -> apply Goal pr.pr_name.id_loc @@ goal f
  | Dprop(Paxiom,pr,f) -> apply When pr.pr_name.id_loc @@ hyp f
  | _ -> Empty

let tdecl (td : Thy.tdecl) : shape =
  match td.td_node with Decl d -> decl d | _ -> Empty

let rec task (t : Tsk.task) : shape =
  match t with
  | None -> Empty
  | Some { task_decl ; task_prev } ->
    tdecl task_decl <|> task task_prev

type chunk = { kind : kind ; range : Range.range }

type env = {
  file : string ;
  mutable goal : chunk list ;
  mutable hyps : chunk list ;
}

let rec collect env = function
  | Empty -> ()
  | Cat(a,b) -> collect env a ; collect env b
  | Range(kind,p) ->
    let f,a,b,c,d = Loc.get p in
    if f = env.file then
      let chunk = { kind ; range = ((a,b),(c,d)) } in
      match kind with
      | Goal ->
        env.goal <- chunk :: env.goal
      | When | WhenNot ->
        env.hyps <- chunk :: env.hyps

let by_range a b = Stdlib.compare a.range b.range
let disjoint a b = Range.disjoint a.range b.range

let chunks ~file (t : Tsk.task) =
  let env = { file ; goal = [] ; hyps = [] } in
  collect env (task t) ;
  let goal = env.goal in
  let hyps = List.filter (fun h -> List.for_all (disjoint h) goal) env.hyps in
  List.sort by_range (goal @ hyps)

(* -------------------------------------------------------------------------- *)
(* ---  Range Filtering                                                   --- *)
(* -------------------------------------------------------------------------- *)

let window ~context rs =
  (* collect the range of lines for the goals *)
  let rec goals p q = function
    | [] -> p, q
    | { kind = (When | WhenNot) } :: ws -> goals p q ws
    | { kind = Goal ; range = r } :: ws ->
      let p = min p (Range.first_line r) in
      let q = max q (Range.last_line r) in
      goals p q ws
  in
  let p,q = goals max_int 0 rs in
  let hp = p - 2*context in
  let hq = q + 2*context in
  let np = ref 0 in
  let nq = ref 0 in
  let rs =
    List.filter
      (fun r ->
         if Range.first_line r.range < hp then (incr np ; false) else
         if Range.last_line r.range > hq then (incr nq ; false) else
           true
      ) rs in
  p,q,!np,!nq,rs

(* -------------------------------------------------------------------------- *)
(* ---  Ranges Dumping                                                    --- *)
(* -------------------------------------------------------------------------- *)

type cursor = {
  text : string ;
  context : int ;
  mutable offset : int ;
  mutable pos : Range.pos ;
}

let forward print cursor =
  let k = cursor.offset in
  let c = cursor.text.[k] in
  if print then Format.print_char c ;
  cursor.pos <- Range.after cursor.pos c ;
  cursor.offset <- succ k

let skip cursor pos =
  while Range.(<<) cursor.pos pos do forward false cursor done

let print cursor pos =
  while Range.(<<) cursor.pos pos do forward true cursor done

let line n = (n,0)

let open_color = function
  | Goal -> Format.printf "@{<bold>@{<bright red>"
  | When -> Format.printf "@{<green>"
  | WhenNot -> Format.printf "@{<magenta>"
let close_color = function
  | Goal -> Format.printf "@}@}"
  | When | WhenNot -> Format.printf "@}"

(* Printing Automata *)

let rec decorate cursor head tail =
  print cursor @@ fst head.range ;
  open_color head.kind ;
  print cursor @@ snd head.range ;
  close_color head.kind ;
  match tail with
  | [] ->
    let q = Range.last_line head.range + cursor.context in
    print cursor @@ line q ;
    Format.printf "@{<orange>[...]@}@."
  | next :: tail ->
    let q = Range.last_line head.range in
    let p' = Range.first_line next.range in
    let ctxt = cursor.context in
    if q + 2 * ctxt + 2 < p' then
      begin
        print cursor @@ line (q + ctxt) ;
        Format.printf "@{<orange>[...]@}@." ;
        skip cursor @@ line (p' - ctxt) ;
        Format.printf "@{<orange>[...]@}@." ;
      end ;
    decorate cursor next tail

let pp_range fmt (p,q) =
  if p < q then Format.fprintf fmt "%d:%d" p q else Format.pp_print_int fmt p

let pp_expl fmt t =
  let expl = Session.task_expl t in
  if expl <> "" then Format.fprintf fmt ":%s" expl

let dump ~file ~context task =
  let p,q,_np,_nq,rs = chunks ~file task |> window ~context in
  match rs with [] -> () | head::tail ->
    Format.printf "@{<orange>[%s:%a%a]@}@\n"
      file pp_range (p,q) pp_expl task ;
    let text = Utils.readfile ~file in
    let cursor = { text ; context ; offset = 0 ; pos = Range.start } in
    try
      let p = Range.first_line head.range in
      skip cursor @@ line (p - cursor.context) ;
      if fst cursor.pos > 0 then
        Format.printf "@{<orange>[...]@}@." ;
      decorate cursor head tail
    with Invalid_argument _ | Exit ->
      if snd cursor.pos > 0 then Format.print_newline () ;
      Format.printf "@{<orange>[eof]@}@."
