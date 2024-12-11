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
let (<+>) (a : shape) (b : shape) : shape =
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

let default kind l s =
  match s, l with
  | Empty, Some p -> Range(kind,p)
  | shape, _ -> shape

let rec collect kind (t : Term.term) =
  match t.t_loc with
  | Some p -> Range(kind,p)
  | None -> Term.t_fold (fun rgs t -> rgs <+> collect kind t) Empty t

let rec hyp (t : Term.term) =
  default When t.t_loc @@
  match t.t_node with
  | Tbinop(Tand,a,b) -> hyp a <*> hyp b
  | Term.Tlet (_,fb) ->
    let _,t = Term.t_open_bound fb in hyp t
  | Tnot p -> collect WhenNot p
  | _ -> collect When t

let rec goal (t : Term.term) =
  default Goal t.t_loc @@
  match t.t_node with
  | Tbinop(Timplies,h,p) -> hyp h <*> goal p
  | Tquant(Tforall,bnd) ->
    let _,_,p = Term.t_open_quant bnd in goal p
  | Term.Tlet (_,fb) ->
    let _,t = Term.t_open_bound fb in goal t
  | _ -> collect Goal t

let decl (d: Decl.decl) : shape =
  match d.d_node with
  | Dprop(Pgoal,pr,f) -> default Goal pr.pr_name.id_loc @@ goal f
  | Dprop(Paxiom,pr,f) -> default When pr.pr_name.id_loc @@ hyp f
  | _ -> Empty

let tdecl (td : Thy.tdecl) : shape =
  match td.td_node with Decl d -> decl d | _ -> Empty

let rec task (t : Tsk.task) : shape =
  match t with
  | None -> Empty
  | Some { task_decl ; task_prev } ->
    let p = task task_prev in
    let d = tdecl task_decl in
    p <+> d

type chunk = { kind : kind ; range : Range.range }

type env = {
  file : string ;
  mutable goal : chunk list ;
  mutable hyps : chunk list ;
}

let make ~file = { file ; goal = [] ; hyps = [] }

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
  let env = make ~file in
  collect env (task t) ;
  let goal = env.goal in
  let hyps = List.filter (fun h -> List.for_all (disjoint h) goal) env.hyps in
  List.sort by_range (goal @ hyps)

(* -------------------------------------------------------------------------- *)
(* ---  Range Filtering                                                   --- *)
(* -------------------------------------------------------------------------- *)

let decls ~file (thy : Why3.Theory.theory) =
  let lines = ref [] in
  List.iter
    (fun (td : Why3.Theory.tdecl) ->
       match td.td_node with
       | Meta _ | Clone _ | Use _ -> ()
       | Decl d ->
         Why3.Ident.Sid.iter
           (fun id ->
              match id.id_loc with
              | None -> ()
              | Some loc ->
                let f,l,_,_,_ = Loc.get loc in
                if f = file then lines := l :: !lines
           ) d.d_news
    ) thy.th_decls ;
  Array.of_list @@ List.sort_uniq Int.compare !lines

let locate l lines =
  let n = Array.length lines in
  if n = 0 then
    0, Sys.max_string_length
  else
    let li = lines.(0) in
    if l < li then (0,li) else
    if n = 1 then li, Sys.max_string_length else
      let lj = lines.(n-1) in
      if l >= lj then lj, Sys.max_string_length else
        (* i < j /\ li <= l < lj *)
        let rec lookup i li j lj =
          if j - i <= 1 then li,lj else
            let k = i + (j-i) / 2 in
            let lk = lines.(k) in
            if l < lk
            then lookup i li k lk
            else lookup k lk j lj
        in lookup 0 li (n-1) lj

let window ~file thy chunks =
  let p = ref max_int in
  let q = ref min_int in
  List.iter
    (fun chunk ->
       match chunk.kind with
       | Goal ->
         p := min !p @@ Range.first_line chunk.range ;
         q := max !q @@ Range.last_line chunk.range ;
       | When | WhenNot -> ()
    ) chunks ;
  let lines = decls ~file thy in
  if !p > !q then 0,0,[] else
    let u,v =
      if !p < !q then
        let p1,p2 = locate !p lines in
        let q1,q2 = locate !q lines in
        min p1 q1, max p2 q2
      else locate !p lines
    in
    !p, !q,
    List.filter
      (fun chunk ->
         u <= Range.first_line chunk.range && Range.last_line chunk.range <= v
      ) chunks

(* -------------------------------------------------------------------------- *)
(* ---  Ranges Dumping                                                    --- *)
(* -------------------------------------------------------------------------- *)

type cursor = {
  text : string ;
  context : int ;
  mutable offset : int ;
  mutable pos : Range.pos ;
}

let lino cursor =
  Format.printf "@{<orange>%4d:@} " @@ fst cursor.pos

let forward print endline cursor =
  let k = cursor.offset in
  let c = cursor.text.[k] in
  if print then Format.print_char c ;
  cursor.pos <- Range.after cursor.pos c ;
  if print && endline && c = '\n' then lino cursor ;
  cursor.offset <- succ k

let skip cursor pos =
  while Range.(<<) cursor.pos pos do
    forward false false cursor done

let print cursor ?(endline=true) pos =
  while Range.(<<) cursor.pos pos do
    forward true (endline || fst cursor.pos + 1 < fst pos) cursor
  done

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
    let q = Range.last_line head.range + cursor.context + 1 in
    print cursor ~endline:false @@ line q
  | next :: tail ->
    let q = Range.last_line head.range in
    let p' = Range.first_line next.range in
    let ctxt = cursor.context in
    if q + 2 * ctxt + 3 < p' then
      begin
        print cursor ~endline:false @@ line (q + ctxt + 1) ;
        Format.printf " @{<orange>•••@}@." ;
        skip cursor @@ line (p' - ctxt) ;
        lino cursor ;
      end ;
    decorate cursor next tail

let pp_range fmt (p,q) =
  if p < q then Format.fprintf fmt "%d:%d" p q else Format.pp_print_int fmt p

let pp_expl fmt t =
  let expl = Session.task_expl t in
  if expl <> "" then Format.fprintf fmt ":%s" expl

let dump ~file ~context thy task =
  ignore thy ;
  let p,q,rs = chunks ~file task |> window ~file thy in
  match rs with [] -> () | head::tail ->
    Format.printf "@{<orange>[%s:%a%a]@}@\n"
      file pp_range (p,q) pp_expl task ;
    let text = Utils.readfile ~file in
    let cursor = { text ; context ; offset = 0 ; pos = Range.start } in
    try
      let p = Range.first_line head.range in
      skip cursor @@ line (p - cursor.context) ;
      lino cursor ;
      decorate cursor head tail ;
      if context > 1 then
        begin
          lino cursor ;
          Format.print_newline () ;
        end ;
    with Invalid_argument _ | Exit ->
      if snd cursor.pos > 0 then
        Format.print_newline ()
