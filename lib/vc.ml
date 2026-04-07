(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* ---  Task Colorization                                                 --- *)
(* -------------------------------------------------------------------------- *)

module Loc = Why3.Loc
module Term = Why3.Term
module Decl = Why3.Decl
module Thy = Why3.Theory
module Task = Why3.Task
module Wid = Why3.Ident
module Mid = Why3.Ident.Mid

type kind = Goal | When | WhenNot
type shape =
  | Empty
  | Cat of shape * shape (* each is non-empty *)
  | Range of kind * Loc.position * Term.term

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
(* ---  Chunk Extraction                                                  --- *)
(* -------------------------------------------------------------------------- *)

let default_shape kind loc term s =
  match s, loc with
  | Empty, Some pos -> Range(kind,pos,term)
  | shape, _ -> shape

let default_term kind term s = default_shape kind term.Term.t_loc term s

let rec collect kind (term : Term.term) =
  match term.t_loc with
  | Some pos -> Range(kind,pos,term)
  | None -> Term.t_fold (fun rgs t -> rgs <+> collect kind t) Empty term

let rec hyp (t : Term.term) =
  default_term When t @@
  match t.t_node with
  | Tbinop(Tand,a,b) -> hyp a <*> hyp b
  | Tquant(Texists,bnd) ->
    let _,_,p = Term.t_open_quant bnd in hyp p
  | Tlet (_,fb) ->
    let _,t = Term.t_open_bound fb in hyp t
  | Tnot p -> collect WhenNot p
  | _ -> collect When t

let rec goal (t : Term.term) =
  default_term Goal t @@
  match t.t_node with
  | Tbinop(Timplies,h,p) -> hyp h <*> goal p
  | Tquant(Tforall,bnd) ->
    let _,_,p = Term.t_open_quant bnd in goal p
  | Term.Tlet (_,fb) ->
    let _,t = Term.t_open_bound fb in goal t
  | _ -> collect Goal t

let shape_of_chunks (d: Decl.decl) : shape =
  match d.d_node with
  | Dprop(Pgoal,pr,p) -> default_shape Goal pr.pr_name.id_loc p @@ goal p
  | Dprop(Paxiom,pr,p) -> default_shape When pr.pr_name.id_loc p @@ hyp p
  | _ -> Empty

(* -------------------------------------------------------------------------- *)
(* --- Splitting Tasks                                                    --- *)
(* -------------------------------------------------------------------------- *)

let split_tdecl fn (td : Thy.tdecl) : shape =
  match td.td_node with Decl d -> fn d | _ -> Empty

let rec split_task fn acc (t : Task.task) : shape =
  match t with
  | None -> acc
  | Some { task_decl ; task_prev } ->
    let d = split_tdecl fn task_decl in
    (split_task[@tailcall]) fn (d <+> acc) task_prev

type chunk = { kind : kind ; range : Range.range ; term : Term.term }

type env = {
  file : string ;
  mutable goal : chunk list ;
  mutable hyps : chunk list ;
}

let rec dispatch env = function
  | Empty -> ()
  | Cat(a,b) -> dispatch env a ; dispatch env b
  | Range(kind,pos,term) ->
    let f,l1,c1,l2,c2 = Loc.get pos in
    if f = env.file then
      let p = Range.at ~line:(l1-1) ~col:c1 () in
      let q = Range.at ~line:(l2-1) ~col:c2 () in
      let chunk = { kind ; range = p,q ; term } in
      match kind with
      | Goal ->
        env.goal <- chunk :: env.goal
      | When | WhenNot ->
        env.hyps <- chunk :: env.hyps

let by_range a b = Stdlib.compare a.range b.range
let disjoint a b = Range.disjoint a.range b.range

let split ~file fn (t : Task.task) =
  let env = { file ; goal = [] ; hyps = [] } in
  dispatch env (split_task fn Empty t) ;
  let goal = env.goal in
  let hyps = List.filter (fun h -> List.for_all (disjoint h) goal) env.hyps in
  List.sort by_range (goal @ hyps)

(* -------------------------------------------------------------------------- *)
(* ---  Range Filtering                                                   --- *)
(* -------------------------------------------------------------------------- *)

let decls ~file (thy : Thy.theory) =
  let lines = ref [] in
  List.iter
    (fun (td : Thy.tdecl) ->
       match td.td_node with
       | Meta _ | Clone _ | Use _ -> ()
       | Decl d ->
         Wid.Sid.iter
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

let goal_range chunks =
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
  !p, !q

(* -------------------------------------------------------------------------- *)
(* ---  Task Naming                                                       --- *)
(* -------------------------------------------------------------------------- *)

let id_name : Wid.ident -> string =
  let names = ref Mid.empty in
  fun id ->
    try Mid.find id !names with Not_found ->
      let _,_,qid = Id.path id in
      let path = Id.cat qid in
      let name =
        if String.ends_with ~suffix:"'vc" path
        then String.sub path 0 (String.length path - 3) else path
      in names := Mid.add id name !names ; name

let task_name t = id_name (Task.task_goal t).pr_name
let task_expl t =
  let (_, expl, _) = Why3.Termcode.goal_expl_task ~root:false t in expl

(* -------------------------------------------------------------------------- *)
(* ---  Goal Naming                                                       --- *)
(* -------------------------------------------------------------------------- *)

(* 0: hyp-name, 1: expl *)
let set_attr_label lbl (a : Wid.attribute) ~prefix k =
  if lbl.(k) = "" && String.starts_with ~prefix a.attr_string then
    let p = String.length prefix in
    let n = String.length a.attr_string in
    lbl.(k) <- String.sub a.attr_string p (n-p)

let set_attr_labels lbl a =
  begin
    set_attr_label lbl a ~prefix:"hyp_name:" 0 ;
    set_attr_label lbl a ~prefix:"expl:" 1 ;
  end

let set_attributes lbl attrs =
  if lbl.(0) = "" || lbl.(1) = "" then
    Wid.Sattr.iter (set_attr_labels lbl) attrs

let (^^) u v =
  if u = "" then v else if v = "" then u else Printf.sprintf "%s.%s" u v

let rec term_head lbl flag (t : Term.term) =
  let flag = flag && not (Wid.Sattr.mem Term.stop_split t.t_attrs) in
  (if flag then set_attributes lbl t.t_attrs) ;
  match t.t_node with
  | Tvar _ | Tconst _ | Ttrue -> ""
  | Tfalse -> "absurd"
  | Tapp (f, ts) ->
    if Id.is_infix f.ls_name.id_string then
      List.fold_right (fun t w -> term_head lbl false t ^ w) ts ""
    else f.ls_name.id_string
  | Tif(e,_,_) -> "if:" ^ term_head lbl flag e
  | Tcase(e,_) -> "case:" ^ term_head lbl flag e
  | Tnot p | Tbinop(Timplies,_,p) -> term_head lbl flag p
  | Tbinop((Tand|Tor|Tiff), a, b) -> term_head lbl flag a ^^ term_head lbl flag b
  | Tlet (_, b) | Teps b ->
    let _,e = Term.t_open_bound b in term_head lbl flag e
  | Tquant (_, b) ->
    let _,_,p = Term.t_open_quant b in term_head lbl flag p

type fullname = { name : string ; expl : string ; head : string }

let task_fullname (task : Task.task) =
  match task with
  | Some { task_decl = { td_node = (Decl { d_node = Dprop(Pgoal, g, p)}) } } ->
    let lbl = [| "" ; "" |] in
    let head = term_head lbl true p in
    set_attributes lbl g.pr_name.id_attrs ;
    let expl = lbl.(1) in
    let name = if lbl.(0) = "" then id_name g.pr_name else lbl.(0) in
    { name ; expl ; head }
  | _ ->
    { name = "<goal>" ; expl = "" ; head = "" }

(* -------------------------------------------------------------------------- *)
(* ---  Dumping Tasks                                                     --- *)
(* -------------------------------------------------------------------------- *)

let pp_range fmt (p,q) =
  if p < q then
    Format.fprintf fmt "%d:%d" (p+1) (q+1)
  else
    Format.pp_print_int fmt (p+1)

let pp_expl fmt e =
  if e <> "" then Format.fprintf fmt " [%s]" e

let dump ~file task =
  let pp_where =
    let p,q = split ~file shape_of_chunks task |> goal_range in
    if p <= q then
      let name = task_name task in
      let expl = task_expl task in
      fun () -> Format.printf "@{<orange>[%s:%a] %s%a@}@\n"
          file pp_range (p,q) name pp_expl expl
    else ignore in
  pp_where () ;
  Why3.Pretty.print_sequent Format.std_formatter task ;
  pp_where () ;
  Format.printf "%s@." (String.make 69 '-')

(* -------------------------------------------------------------------------- *)
(* ---  Windowing Chunks                                                  --- *)
(* -------------------------------------------------------------------------- *)

type context = { file : string ; line : int ; end_line : int ; chunks : chunk list }
let empty = { file = "" ; line = 0 ; end_line = 0 ; chunks = [] }

let window ~file thy chunks =
  let lines = decls ~file thy in
  let p,q = goal_range chunks in
  if p > q then empty else
    (* declaration range *)
    let u,v =
      if p < q then
        let p1,p2 = locate p lines in
        let q1,q2 = locate q lines in
        min p1 q1, max p2 q2
      else locate p lines
    in
    let hp = ref p in
    let hq = ref q in
    let chunks =
      List.filter
        (fun chunk ->
           let cp = Range.first_line chunk.range in
           let cq = Range.last_line chunk.range in
           u <= cp && cq <= v &&
           begin
             hp := min !hp cp ;
             hq := max !hq cq ;
             true
           end
        ) chunks
    in { file ; line = !hp ; end_line = !hq ; chunks }

let context ~file thy task = window ~file thy @@ split shape_of_chunks ~file task

(* -------------------------------------------------------------------------- *)
(* ---  Printing Automaton                                                --- *)
(* -------------------------------------------------------------------------- *)

type style = Comment | Kind of kind | Text

type cursor = {
  text : string ;
  width : int ;
  mutable edge : bool ;
  mutable style : style ;
  mutable comment : int ;
  mutable offset : int ;
  mutable pos : Range.pos ;
}

let keywords = lazy
  begin
    let m = ref Dict.empty in
    List.iter
      (fun k -> m := Dict.add k k !m)
      Why3.Keywords.keywords ; !m
  end

let pp_open_style = function
  | Text -> ()
  | Comment -> Format.printf "@{<gray>"
  | Kind Goal -> Format.printf "@{<bold>@{<bright red>"
  | Kind When -> Format.printf "@{<green>"
  | Kind WhenNot -> Format.printf "@{<magenta>"

let pp_close_style = function
  | Text -> ()
  | Comment | Kind (When | WhenNot) -> Format.printf "@}"
  | Kind Goal -> Format.printf "@}@}"

let is_text cursor = (cursor.style = Text)

let open_style cursor sty =
  match cursor.style with
  | Text -> cursor.style <- sty ; pp_open_style sty
  | Comment | Kind _ -> ()

let close_style cursor sty =
  if sty = cursor.style then
    begin
      pp_close_style cursor.style ;
      cursor.style <- Text ;
    end

let lino cursor =
  begin
    pp_close_style cursor.style ;
    Format.printf "@{<orange>%4d:@} " @@ 1 + cursor.pos.line ;
    pp_open_style cursor.style ;
  end

let edge = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '\'' | '_' -> false
  | _ -> true

let move cursor ofs =
  begin
    let Range.{ line ; col } = cursor.pos in
    cursor.offset <- cursor.offset + ofs ;
    cursor.pos <- Range.at ~line ~col:(col+ofs) () ;
  end

let comment_mult text k =
  let n = String.length text in
  k + 2 < n && text.[k+1] = '*' && text.[k+2] = ')'

let comment_start text k =
  let n = String.length text in
  k + 1 < n && text.[k+1] = '*'

let comment_end text k =
  let n = String.length text in
  k + 1 < n && text.[k+1] = ')'

let skip cursor pos =
  if cursor.comment > 0 then close_style cursor Comment ;
  while Range.(<<) cursor.pos pos do
    let k = cursor.offset in
    let c = cursor.text.[k] in
    match c with
    | '\n' ->
      cursor.pos <- Range.after cursor.pos c ;
      cursor.offset <- succ k ;
    | '(' when comment_mult cursor.text k ->
      move cursor 3 ;
    | '(' when comment_start cursor.text k ->
      cursor.comment <- succ cursor.comment ;
      move cursor 2 ;
    | '*' when comment_end cursor.text k ->
      cursor.comment <- pred cursor.comment ;
      move cursor 2 ;
    | _ ->
      move cursor 1 ;
      cursor.edge <- edge c
  done

let print cursor ?(endline=true) pos =
  if cursor.comment > 0 then open_style cursor Comment ;
  while Range.(<<) cursor.pos pos do
    let k = cursor.offset in
    let c = cursor.text.[k] in
    begin
      match
        if cursor.edge && is_text cursor then
          Dict.find cursor.text k (Lazy.force keywords)
        else None
      with
      | None ->
        begin
          match c with
          (* endline *)
          | '\n' when (endline || cursor.pos.line + 1 < pos.line) ->
            cursor.pos <- Range.after cursor.pos c ;
            cursor.offset <- succ k ;
            cursor.edge <- true ;
            Format.print_char c ;
            lino cursor ;
            (* no-comment *)
          | '(' when comment_mult cursor.text k ->
            move cursor 3 ;
            cursor.edge <- true ;
            Format.print_string "(*)" ;
            (* comment start *)
          | '(' when comment_start cursor.text k ->
            move cursor 2 ;
            cursor.edge <- true ;
            if cursor.comment = 0 then open_style cursor Comment ;
            cursor.comment <- succ cursor.comment ;
            Format.print_string "(*"
          (* comment end *)
          | '*' when comment_end cursor.text k ->
            move cursor 2 ;
            cursor.edge <- true ;
            Format.print_string "*)" ;
            cursor.comment <- pred cursor.comment ;
            if cursor.comment = 0 then close_style cursor Comment ;
            (* normal text, including not-endline newlines *)
          | _ ->
            cursor.pos <- Range.after cursor.pos c ;
            cursor.offset <- succ k ;
            cursor.edge <- edge c ;
            Format.print_char c ;
        end
      | Some kwd ->
        (* keywords *)
        let n = String.length kwd in
        let s = String.length cursor.text in
        if k+n >= s || edge cursor.text.[k+n] then
          Format.printf "@{<bold>%s@}" kwd
        else
          Format.print_string kwd ;
        move cursor n ;
        cursor.edge <- false;
    end
  done

let line n = Range.at ~line:n ()

let rec decorate cursor head tail =
  print cursor @@ fst head.range ;
  let style = Kind head.kind in
  open_style cursor style ;
  print cursor @@ snd head.range ;
  close_style cursor style ;
  match tail with
  | [] ->
    let q = Range.last_line head.range + cursor.width + 1 in
    print cursor ~endline:false @@ line q
  | next :: tail ->
    let q = Range.last_line head.range in
    let p' = Range.first_line next.range in
    let ctxt = cursor.width in
    if q + 2 * ctxt + 2 < p' then
      begin
        print cursor ~endline:false @@ line (q + ctxt + 1) ;
        pp_close_style cursor.style ;
        Format.printf " @{<orange>•••@}@." ;
        pp_open_style cursor.style ;
        skip cursor @@ line (p' - ctxt) ;
        lino cursor ;
      end ;
    decorate cursor next tail

(* -------------------------------------------------------------------------- *)
(* ---  Highlighting Ranges                                               --- *)
(* -------------------------------------------------------------------------- *)

let highlight ~file ~context:width thy task =
  let w = context ~file thy task in
  match w.chunks with [] -> () | head::tail ->
    Format.printf "@{<orange>[%s:%a %s]@}@\n"
      file pp_range (w.line,w.end_line) (task_name task) ;
    let text = Utils.readfile ~file in
    let cursor = {
      text ; width ; offset = 0 ; pos = Range.start ;
      edge = false ; comment = 0 ; style = Text ;
    } in
    try
      let p = Range.first_line head.range in
      skip cursor @@ line (p - cursor.width) ;
      lino cursor ;
      decorate cursor head tail ;
      if width > 1 then
        begin
          lino cursor ;
          Format.print_newline () ;
        end ;
    with Invalid_argument _ | Exit ->
      if cursor.pos.col > 0 then
        Format.print_newline ()

(* -------------------------------------------------------------------------- *)
