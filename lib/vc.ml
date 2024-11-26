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

let range (c: kind) = function None -> Empty | Some p -> Range(c,p)

let apply c p = function Empty -> range c p | _ as shape -> shape

let rec hyp (t : Term.term) =
  apply When t.t_loc @@
  match t.t_node with
  | Tbinop(Tand,a,b) -> hyp a <*> hyp b
  | Tnot p -> range WhenNot p.t_loc
  | _ -> Empty

let rec goal (t : Term.term) =
  apply Goal t.t_loc @@
  match t.t_node with
  | Tbinop(Timplies,h,p) -> hyp h <*> goal p
  | Tlet(_,bnd) ->
    let _,p = Term.t_open_bound bnd in goal p
  | Tquant(Tforall,bnd) ->
    let _,_,p = Term.t_open_quant bnd in goal p
  | _ -> Empty

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

let by_range (_,r1) (_,r2) = Stdlib.compare r1 r2

type env = {
  file : string ;
  mutable goal : (kind * Range.range) list ;
  mutable hyps : (kind * Range.range) list ;
}

let rec collect env = function
  | Empty -> ()
  | Cat(a,b) -> collect env a ; collect env b
  | Range(kd,p) ->
    let f,a,b,c,d = Loc.get p in
    if f = env.file then
      let entry = kd,((a,b),(c,d)) in
      match kd with
      | Goal ->
        env.goal <- entry :: env.goal
      | When | WhenNot ->
        env.hyps <- entry :: env.hyps

let disjoint (_,a) (_,b) = Range.disjoint a b

let ranges ~file (t : Tsk.task) =
  let env = { file ; goal = [] ; hyps = [] } in
  collect env (task t) ;
  let goal = env.goal in
  let hyps = List.filter (fun h -> List.for_all (disjoint h) goal) env.hyps in
  List.sort by_range (goal @ hyps)

type cursor = {
  text : string ;
  mutable offset : int ;
  mutable pos : Range.pos ;
}

let forward print cursor =
  let k = cursor.offset in
  let c = cursor.text.[k] in
  if print then Format.print_char c ;
  cursor.pos <- Range.after cursor.pos c ;
  cursor.offset <- succ k

let skip = forward false
let print = forward true

let before cursor ~context u =
  let p = (Range.first_line @@ snd u) - context in
  while fst cursor.pos < p do skip cursor done

let after cursor ~context v =
  let p = (Range.last_line @@ snd v) + context in
  while fst cursor.pos <= p do print cursor done

let open_color = function
  | Goal -> Format.printf "@{<bold>@{<bright red>"
  | When -> Format.printf "@{<green>"
  | WhenNot -> Format.printf "@{<magenta>"
let close_color = function
  | Goal -> Format.printf "@}@}"
  | When | WhenNot -> Format.printf "@}"

let rec decorate cursor u w =
  let open Range in
  let kd,(a,_) = u in
  if cursor.pos << a then
    (print cursor ; decorate cursor u w)
  else
    (open_color kd ; print cursor ; colored cursor u w)

and colored cursor u w =
  let open Range in
  let kd,(_,b) = u in
  if cursor.pos << b then
    (print cursor ; colored cursor u w)
  else
    (close_color kd ;
     match w with
     | [] -> u
     | v::w -> decorate cursor v w)

(* Selects the ranges that are near the goal(s) *)
let window ~context rs =
  (* collect the range of lines for the goals *)
  let rec goals p q = function
    | [] -> p, q
    | ((When | WhenNot),_) :: ws -> goals p q ws
    | (Goal,r)::ws ->
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
      (fun (_,r) ->
         if Range.first_line r < hp then (incr np ; false) else
         if Range.last_line r > hq then (incr nq ; false) else
           true
      ) rs in
  p,q,!np,!nq,rs

let pp_range fmt (p,q) =
  if p < q then Format.fprintf fmt "%d:%d" p q else Format.pp_print_int fmt p
let pp_trail fmt n = if n > 0 then Format.fprintf fmt " @{<green>(+%d)@}" n

let pp_expl fmt t =
  let expl = Session.task_expl t in
  if expl <> "" then Format.fprintf fmt ":%s" expl

let flush cursor =
  if snd cursor.pos > 0 then Format.print_newline () ;
  Format.printf "@{<orange>[eof]@}@."

let dump ~file ~context task =
  let p,q,np,nq,rs = ranges ~file task |> window ~context in
  match rs with [] -> () | u::w ->
    let text = Utils.readfile ~file in
    let cursor = { text ; offset = 0 ; pos = Range.start } in
    try
      Format.printf "@{<orange>[%s:%a%a]@}@\n" file
        pp_range (p,q) pp_expl task ;
      before cursor ~context u ;
      if fst cursor.pos > 1 then
        Format.printf "@{<orange>[...]@}%a@." pp_trail np ;
      let v = decorate cursor u w in
      after cursor ~context v ;
      Format.printf "@{<orange>[...]@}%a@." pp_trail nq ;
    with Invalid_argument _ | Exit ->
      flush cursor
