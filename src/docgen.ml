(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- HTML Documentation Generator                                       --- *)
(* -------------------------------------------------------------------------- *)

module T = Token
module L = Lexer
module P = Pdoc
module Sid = Why3.Ident.Sid

(* -------------------------------------------------------------------------- *)
(* --- HTML Mode                                                          --- *)
(* -------------------------------------------------------------------------- *)

type mode =
  | Body
  | Pre
  | Div
  | Par
  | Emph
  | Bold
  | Head of Pdoc.buffer * int
  | List of int (* indent *)
  | Item of int (* indent *)

let pp_mode fmt = function
  | Body -> Format.pp_print_string fmt "BODY"
  | Pre -> Format.pp_print_string fmt "PRE"
  | Div -> Format.pp_print_string fmt "DIV"
  | Par -> Format.pp_print_string fmt "PAR"
  | Emph -> Format.pp_print_string fmt "EMPH"
  | Bold -> Format.pp_print_string fmt "BOLD"
  | Head(_,n) -> Format.fprintf fmt "HEAD%d" n
  | List n -> Format.fprintf fmt "LIST%d" n
  | Item n -> Format.fprintf fmt "ITEM%d" n
[@@ warning "-32"]

(* -------------------------------------------------------------------------- *)
(* --- Environment                                                        --- *)
(* -------------------------------------------------------------------------- *)

type block = Raw | Doc | Src
type indent = Code | Head of int | Lines of int * int

let pp_block fmt = function
  | Raw -> Format.pp_print_string fmt "RAW"
  | Doc -> Format.pp_print_string fmt "DOC"
  | Src -> Format.pp_print_string fmt "SRC"
[@@ warning "-32"]

type env = {
  dir : string ; (* destination directory *)
  src : Docref.source ; (* source file infos *)
  package_url : string option;
  input : Token.input ; (* input lexer *)
  out : Pdoc.output ; (* output buffer *)
  crc : Pdoc.output ; (* proofs buffer *)
  wenv : Why3.Env.env ; (* why3 env. *)
  henv : Axioms.henv ; (* axioms *)
  senv : Soundness.env ; (* instances *)
  mutable proof_href : int ; (* proof href *)
  mutable cloning : Cloning.state ; (* clone substitution *)
  mutable declared : Sid.t ; (* locally declared *)
  mutable current : string option ; (* current module name *)
  mutable theory : Docref.theory option ; (* current module theory *)
  mutable instance : Docref.instance option ; (* current module instance *)
  mutable cloned : int ; (* last instance order declared *)
  mutable space : bool ; (* leading space in Par mode *)
  mutable indent : indent ; (* leading space in Pre mode *)
  mutable block: block ; (* required block for text *)
  mutable mode : mode ; (* current output mode *)
  mutable file : mode ; (* current file mode *)
  mutable stack : mode list ; (* currently opened modes *)
  mutable opened : string list ; (* opening keywords *)
  mutable scopes : string list ; (* opened scopes *)
  mutable section : int ; (* code foldable section depth *)
}

let bsync ?wanted env =
  let wanted =
    match wanted with Some b -> b | None ->
    match env.mode with
    | Body -> Raw
    | Pre -> Src
    | Head _ | Div | Par | Emph | Bold | List _ | Item _ -> Doc
  in
  if env.block <> wanted then
    begin
      (* Need for closing *)
      begin match env.block with
        | Raw -> ()
        | Doc -> Pdoc.pp_print_string env.out "</div>\n"
        | Src ->
          match env.indent with
          | Code -> Pdoc.pp_print_string env.out "\n</pre>\n"
          | Lines _ -> Pdoc.pp_print_string env.out "</pre>\n"
          | Head _ -> ()
      end ;
      Pdoc.flush env.out ;
      env.indent <- Code ;
      env.block <- wanted ;
      (* Need for opening *)
      begin match wanted with
        | Raw -> ()
        | Doc -> Pdoc.pp_print_string env.out "<div class=\"doc\">\n"
        | Src -> env.indent <- Head 0
      end ;
    end

let clear env =
  env.space <- false

let pp_space env = Pdoc.pp_print_char env.out ' '
let pp_spaces env n = Pdoc.pp env.out Pdoc.pp_spaces n

let space env =
  if env.space then
    begin
      pp_space env ;
      clear env ;
    end

let push env m =
  env.stack <- env.mode :: env.stack ;
  env.mode <- m ;
  match m with
  | Body | Head _ | Div -> env.indent <- Code
  | Pre -> env.indent <- Head 0
  | Par -> bsync env ; Pdoc.pp_print_string env.out "<p>"
  | List _ -> bsync env ; Pdoc.pp_print_string env.out "<ul>\n"
  | Item _ -> bsync env ; Pdoc.pp_print_string env.out "<li>"
  | Emph -> bsync env ; space env ; Pdoc.pp_print_string env.out "<em>"
  | Bold -> bsync env ; space env ; Pdoc.pp_print_string env.out "<strong>"

let pop env =
  let m = env.mode in
  begin match env.stack with [] -> () | old :: stk ->
    env.mode <- old ;
    env.stack <- stk ;
  end ;
  match m with
  | Body | Head _ -> ()
  | Pre -> clear env
  | Div -> clear env
  | Par -> Pdoc.pp_print_string env.out "</p>\n" ; clear env
  | List _ -> Pdoc.pp_print_string env.out "</ul>\n" ; clear env
  | Item _ -> Pdoc.pp_print_string env.out "</li>\n" ; clear env
  | Emph -> Pdoc.pp_print_string env.out "</em>" (* keep space *)
  | Bold -> Pdoc.pp_print_string env.out "</strong>" (* keep space *)

let text env =
  match env.mode with
  | Body -> push env env.file ; bsync env
  | Div -> push env Par ; bsync env
  | List n -> push env (Item n) ; bsync env
  | Par | Item _ | Head _ | Emph | Bold -> space env ; bsync env
  | Pre ->
    match env.indent with
    | Code -> ()
    | Head n ->
      bsync env ;
      Pdoc.pp_print_string env.out "<pre class=\"src\">\n" ;
      Pdoc.pp_print_string env.out (String.make n ' ') ;
      env.indent <- Code
    | Lines(l,n) ->
      Pdoc.pp_print_string env.out (String.make l '\n') ;
      Pdoc.pp_print_string env.out (String.make n ' ') ;
      env.indent <- Code

let head env buffer level =
  begin
    let title = Pdoc.buffered env.out in
    Pdoc.pop env.out buffer ;
    Pdoc.header env.out ~level ~title () ;
  end

let rec close ?(parblock=false) env =
  match env.mode with
  | Body -> ()
  | Div | Pre when parblock -> ()
  | Div | Pre | Par | List _ | Item _ -> pop env ; close ~parblock env
  | Emph -> Token.error env.input "unclosed emphasis style"
  | Bold -> Token.error env.input "unclosed bold style"
  | Head(buffer,level) -> head env buffer level ; pop env ; close ~parblock env

(* -------------------------------------------------------------------------- *)
(* --- Icons                                                              --- *)
(* -------------------------------------------------------------------------- *)

let icon_nogoal = "icon remark icofont-check"
let icon_valid = "icon valid icofont-check"
let icon_partial = "icon warning icofont-error"
let icon_failed = "icon failed icofont-error"
let icon_complete = "icon small valid icofont-star"
let icon_parameter = "icon small remark icofont-question-circle"
let icon_hypothesis = "icon small warning icofont-warning-alt"
let icon_unsafe = "icon small failed icofont-warning"

let pp_mark ?href ~title ~cla fmt =
  match href with
  | None ->
    Format.fprintf fmt "<span title=\"%s\" class=\"%s\"></span>"
      title cla
  | Some href ->
    Format.fprintf fmt "<a href=\"%t\" title=\"%s\" class=\"%s\"></a>"
      href title cla

(* -------------------------------------------------------------------------- *)
(* --- Proof Report                                                       --- *)
(* -------------------------------------------------------------------------- *)

let pp_verdict_raw href fmt r =
  match r with
  | `Valid n ->
    let title = match n with
      | 0 -> "Valid (no goals)"
      | 1 -> "Valid (one goal)"
      | _ -> Printf.sprintf "Valid (%d goals)" n in
    let cla = if n > 0 then icon_valid else icon_nogoal in
    pp_mark ?href ~title ~cla fmt
  | `Partial(p,n) ->
    let title = Printf.sprintf "Partial proof (%d/%d goals)" p n in
    pp_mark ?href ~title ~cla:icon_partial fmt
  | `Failed _ ->
    let title = "Failed (no proof)" in
    pp_mark ?href ~title ~cla:icon_failed fmt

let pp_verdict = pp_verdict_raw None
let pp_vlink ~href fmt r = pp_verdict_raw (Some href) fmt r

let process_proofs_summary env ~crc id = function
  | None -> ()
  | Some Docref.{ proofs } ->
    let stuck,proved =
      Docref.Mstr.fold
        (fun _g c (s,p) -> s + Crc.get_stuck c , p + Crc.get_proved c)
        proofs (0,0) in
    let href fmt = Format.fprintf fmt "%s.proof.html#%s" env.src.urlbase id in
    let r = Crc.nverdict ~stuck ~proved in
    Pdoc.pp env.out (pp_vlink ~href) r ;
    if crc then Pdoc.pp env.crc pp_verdict r

let rec child n fmt crc =
  Format.fprintf fmt "@\n%a" Pdoc.pp_spaces n ;
  match crc with
  | Crc.Stuck ->
    Format.fprintf fmt "stuck<span class=\"%s\"></span>" icon_failed
  | Crc.Prover(p,None,t) ->
    Format.fprintf fmt "<span class=\"prover\" title=\"%s\">%s</span> %a"
      p p Utils.pp_time t
  | Crc.Prover(p,Some v,t) ->
    Format.fprintf fmt "<span class=\"prover\" title=\"%s@%s\">%s</span> %a"
      p v p Utils.pp_time t
  | Crc.Tactic { id ; children ; stuck ; proved } ->
    Format.fprintf fmt "%s%a" id pp_verdict (Crc.nverdict ~stuck ~proved) ;
    List.iter (child (n+2) fmt) children

let process_certif env id crc =
  Pdoc.printf env.crc
    "<pre class=\"src\">@\n  %a <a id=\"%a\" href=\"%a\">%a</a>%a%t</pre>"
    Pdoc.pp_keyword "goal"
    Id.pp_proof_aname id
    (Id.pp_ahref ~current:None ~package_url:env.package_url) id
    Id.pp_local id
    pp_verdict (Crc.verdict crc)
    begin fun fmt ->
      match crc with
      | Crc.Stuck -> ()
      | Crc.Prover _ -> child 4 fmt crc
      | Crc.Tactic _ -> child 4 fmt crc
    end

let process_proof env id ?(valid=false) = function
  | None ->
    if valid then Pdoc.ppt env.out (pp_mark ~title:"cloned" ~cla:icon_nogoal)
  | Some crc ->
    let href fmt = Id.pp_proof_ahref fmt id in
    Pdoc.pp env.out (pp_vlink ~href) (Crc.verdict crc) ;
    process_certif env id crc

(* -------------------------------------------------------------------------- *)
(* --- Axioms & Parameter                                                 --- *)
(* -------------------------------------------------------------------------- *)

let provers ops = String.concat "," (List.map (fun (p,_) -> Prover.name p) ops)

let process_axioms env (id : Id.id) =
  match env.theory with
  | None -> ()
  | Some theory ->
    match Axioms.parameter theory.signature id.self with
    | None -> ()
    | Some { kind ; builtin ; extern } ->
      match builtin, extern with
      | [],None ->
        let cloned_param kind =
          match Soundness.compute env.senv theory with
          | Sound _ -> icon_complete, kind ^ " with ground instance"
          | Unknown _ | Unsound -> icon_hypothesis, kind
        in
        let cla,title =
          match kind with
          | Type | Logic | Param -> icon_parameter, "Parameter"
          | Unsafe -> icon_unsafe, "Unsound Definition"
          | Value -> cloned_param "Value Parameter"
          | Axiom -> cloned_param "Hypothesis"
        in
        Pdoc.ppt env.out (pp_mark ~cla ~title)
      | [], Some ext ->
        let title = Printf.sprintf "External OCaml symbol (%s)" ext in
        Pdoc.ppt env.out (pp_mark ~cla:icon_hypothesis ~title)
      | ops, None ->
        let title = Printf.sprintf "Built-in symbol (%s)" (provers ops) in
        Pdoc.ppt env.out (pp_mark ~cla:icon_hypothesis ~title)
      | ops, Some ext ->
        let title = Printf.sprintf
            "Built-in symbol (%s), extracted to OCaml (%s)" (provers ops) ext
        in Pdoc.ppt env.out (pp_mark ~cla:icon_hypothesis ~title)

let process_hypothesis env (s : Soundness.soundness) (p : Axioms.parameter) =
  try
    let id = Id.resolve ~lib:env.src.lib p.name in
    let key = match p.kind with
      | Type -> "type"
      | Logic -> "logic"
      | Param -> "param"
      | Value -> "value"
      | Axiom -> "axiom"
      | Unsafe -> "unsound"
    in
    let qualif fmt =
      if Axioms.is_unsafe p then
        pp_mark ~cla:icon_unsafe ~title:"unsafe definition" fmt
      else
      if not @@ Axioms.is_free p then
        if Soundness.is_sound s then
          pp_mark ~cla:icon_complete ~title:"witnessed hypothesis" fmt
        else
          pp_mark ~cla:icon_hypothesis ~title:"uncloned hypothesis" fmt
    in
    Pdoc.printf env.crc
      "@\n  %a <a id=\"%a\" href=\"%a\">%a</a>%t"
      Pdoc.pp_keyword key
      Id.pp_proof_aname id
      (Id.pp_ahref ~current:None ~package_url:env.package_url) id
      Id.pp_local id qualif
  with Not_found -> ()

let process_instance env ~ok (Docref.Instance s) =
  Pdoc.printf env.crc "@\n  %a <a href=\"%s.html#clone-%d\">%s</a>"
    Pdoc.pp_keyword "instance" s.container s.order s.container ;
  let title = (if ok then "sound" else "incomplete") ^ " instance" in
  let cla = if ok then icon_complete else icon_hypothesis in
  Pdoc.ppt env.crc @@ pp_mark ~title ~cla

let process_module_axioms env =
  match env.theory with
  | None -> ()
  | Some thy ->
    let pre = lazy (Pdoc.printf env.crc "<pre class=\"src\">") in
    let sound = Soundness.compute env.senv thy in
    Axioms.iter env.henv ~self:true
      (fun (p : Axioms.parameter) ->
         if not @@ Axioms.is_free p &&
            not @@ Id.standard p.name then
           begin
             Lazy.force pre ;
             process_hypothesis env sound p ;
           end
      ) [thy.theory] ;
    begin match sound with
      | Unsound | Sound [] | Unknown [] -> ()
      | Sound ns ->
        Lazy.force pre ; List.iter (process_instance env ~ok:true) ns
      | Unknown ns ->
        Lazy.force pre ; List.iter (process_instance env ~ok:false) ns
    end ;
    if Lazy.is_val pre then Pdoc.printf env.crc "@\n</pre>@."

type axioms = {
  prm : int ;
  hyp :  int ;
  pvs : int ;
  unsafe : int ;
  sound : Soundness.soundness ;
}

let free = {
  prm = 0 ; hyp = 0 ; pvs = 0 ; unsafe = 0 ;
  sound = Soundness.free ;
}

let unknown =  { free with sound = Soundness.unknown }

let add_axiom hs (p : Axioms.parameter) =
  match p.kind with
  | Axiom -> { hs with hyp = succ hs.hyp }
  | Value -> { hs with pvs = succ hs.pvs }
  | Unsafe -> { hs with unsafe = succ hs.unsafe }
  | Type | Logic | Param -> { hs with prm = succ hs.prm }

let pp_axioms_link ?href fmt { prm ; hyp ; pvs ; unsafe ; sound } =
  if prm+hyp+pvs+unsafe > 0 then
    let cla =
      if unsafe > 0 then icon_unsafe else
      if hyp + pvs > 0 then
        match sound with
        | Unsound -> icon_unsafe
        | Unknown _ | Sound [] -> icon_hypothesis
        | Sound _ -> icon_complete
      else
      if prm > 0 then icon_parameter else icon_hypothesis
    in
    let title =
      let plural k single msg =
        if k = 0 then [] else if k = 1 then [single] else [msg k] in
      String.concat ", " @@ List.concat [
        plural pvs "1 value parameter" @@ Printf.sprintf "%d value parameters" ;
        plural prm "1 logic parameter" @@ Printf.sprintf "%d logic parameters" ;
        plural hyp "1 hypothesis" @@ Printf.sprintf "%d hypotheses" ;
        match sound with
        | Sound [] -> []
        | Sound ns ->
          plural (List.length ns) "1 ground instance" @@
          Printf.sprintf "%d ground instances"
        | Unsound ->
          plural unsafe "1 unsound definition" @@
          Printf.sprintf "%d unsafe definitions"
        | Unknown [] -> ["0 instance found"]
        | Unknown ns ->
          plural (List.length ns) "1 incomplete instance" @@
          Printf.sprintf "%d incomplete instances"
      ]
    in pp_mark ~cla ~title ?href fmt

let pp_axioms = pp_axioms_link ?href:None

let process_axioms_summary env id ~crc = function
  | None -> ()
  | Some (thy : Docref.theory) ->
    let href fmt = Format.fprintf fmt "%s.proof.html#%s" env.src.urlbase id in
    let hs = List.fold_left add_axiom unknown @@ Axioms.parameters thy.signature in
    let hs = { hs with sound = Soundness.compute env.senv thy } in
    Pdoc.pp env.out (pp_axioms_link ~href) hs ;
    if crc then Pdoc.pp env.crc pp_axioms hs

(* -------------------------------------------------------------------------- *)
(* --- Printing Declarations                                              --- *)
(* -------------------------------------------------------------------------- *)

let pp_ident ~env ~local ?anchor ?attr fmt id =
  match Id.resolve ~lib:env.src.lib id with
  | id ->
    Format.fprintf fmt "%a<a" (Id.pp_qpath ~local) id ;
    Option.iter (Format.fprintf fmt " id=\"%s\"") anchor ;
    Option.iter (Format.fprintf fmt " class=\"%s\"") attr ;
    Format.fprintf fmt " title=\"%a\" href=\"%a\">%a</a>"
      Id.pp_title id
      (Id.pp_ahref ~current:None ~package_url:env.package_url) id
      Id.pp_qname id
  | exception Not_found ->
    (* builtin *)
    Format.pp_print_string fmt id.id_string

let rec pp_type ~env ~local ~arg fmt (ty : Why3.Ty.ty) =
  if arg then Format.pp_print_char fmt ' ' ;
  match ty.ty_node with
  | Tyvar tv -> Format.pp_print_string fmt tv.tv_name.id_string
  | Tyapp(ts,[]) -> pp_ident ~env ~local fmt ts.ts_name
  | Tyapp(ts,args) ->
    begin
      if arg then Format.pp_print_char fmt '(' ;
      pp_ident ~env ~local fmt ts.ts_name ;
      List.iter (pp_type ~env ~local ~arg:true fmt) args ;
      if arg then Format.pp_print_char fmt ')' ;
    end

let attributes (rs : Why3.Expr.rsymbol) =
  (if Why3.Expr.rs_ghost rs then ["ghost"] else []) @
  (match Why3.Expr.rs_kind rs with
   | RKnone | RKlocal -> []
   | RKfunc -> ["function"]
   | RKpred -> ["predicate"]
   | RKlemma -> ["lemma"])

let defkind = function
  | { Why3.Expr.c_node = Cany } -> "val"
  | _ -> "let"

let declare env n kwd ?(attr=[]) ?(valid=false)
    ~local ~source ~target ?(more=false) () =
  begin
    let { package_url ; current ; src = { lib } } = env in
    let tgt = Id.resolve ~lib target in
    let src = Id.resolve ~lib source in
    let pp_href = Id.pp_ahref ~package_url ~current in    Pdoc.printf env.out "%a%a " Pdoc.pp_spaces n Pdoc.pp_keyword kwd ;
    List.iter (Pdoc.printf env.out "%a " Pdoc.pp_attribute) attr ;
    Pdoc.printf env.out
      "%a<a id=\"%a\" href=\"%a\" title=\"%a  (cloned from %a)\">%a</a>"
      (Id.pp_qpath ~local) tgt
      Id.pp_aname tgt pp_href src
      Id.pp_title tgt Id.pp_title src
      Id.pp_qname tgt ;
    process_axioms env tgt ;
    process_proof env tgt ~valid @@ Docref.find_proof tgt.self env.theory ;
    if not more then Pdoc.printf env.out "@\n" ;
  end

let symbol env n ?attr ~local ~source (ls : Why3.Term.lsymbol) =
  let target = ls.ls_name in
  match ls.ls_value with
  | None ->
    declare env n "predicate" ?attr ~local ~source ~target ~more:true () ;
    List.iter (Pdoc.pp env.out @@ pp_type ~env ~local ~arg:true) ls.ls_args ;
    Pdoc.printf env.out "@\n"
  | Some r ->
    declare env n "function" ?attr ~local ~source ~target ~more:true () ;
    List.iter (Pdoc.pp env.out @@ pp_type ~env ~local ~arg:true) ls.ls_args ;
    Pdoc.printf env.out " : %a@\n" (pp_type ~env ~local ~arg:false) r

let decl env n ~local ~source (d : Why3.Decl.decl) ~target =
  match d.d_node with
  | Dtype { ts_def = NoDef } -> declare env n "type" ~local ~source ~target ()
  | Dtype _ | Ddata _ -> declare env n "type" ~local ~source ~target ()
  | Dparam ls -> symbol env n ~local ~source ls
  | Dlogic ds ->
    List.iter
      (fun (ls,_) ->
         if ls.Why3.Term.ls_name = target then symbol env n ~local ls ~source
      ) ds
  | Dind _ -> declare env n "inductive" ~local ~source ~target ()
  | Dprop(Plemma,_,_) ->
    declare env n "lemma" ~local ~valid:true ~source ~target ()
  | Dprop(Paxiom,_,_) ->
    declare env n "axiom" ~local ~valid:false ~source ~target ()
  | Dprop(Pgoal,_,_) -> ()

let signature env n ~local ~source (rs : Why3.Expr.rsymbol) df =
  begin
    let kwd = defkind df in
    let attr = attributes rs in
    declare env n kwd ~attr ~local ~source ~target:rs.rs_name ~more:true () ;
    List.iter
      (fun x ->
         Pdoc.pp env.out (pp_type ~env ~local ~arg:true) Why3.Ity.(ty_of_ity x.pv_ity)
      ) rs.rs_cty.cty_args ;
    Pdoc.printf env.out " : %a@\n"
      (pp_type ~env ~local ~arg:false)
      Why3.Ity.(ty_of_ity rs.rs_cty.cty_result) ;
  end

let mdecl env n ~local ~source ~target (pd: Why3.Pdecl.pdecl) =
  match pd.pd_node with
  | PDpure -> List.iter (decl env n ~local ~source ~target) pd.pd_pure
  | PDtype _ -> declare env n "type" ~local ~source ~target ()
  | PDexn _ -> declare env n "exception" ~local ~source ~target ()
  | PDlet dlet ->
    begin
      match dlet with
      | LDvar(pv,_) ->
        declare env n "let" ~local ~source ~target ~more:true () ;
        Pdoc.printf env.out " : %a"
          (pp_type ~env ~local ~arg:false) pv.pv_vs.vs_ty ;
        Pdoc.printf env.out "@\n"
      | LDsym(rs,df) -> signature env n ~local ~source rs df
      | LDrec defs ->
        List.iter
          (fun (d : Why3.Expr.rec_defn) ->
             let rs = d.rec_sym in
             if Why3.Ident.id_equal target rs.rs_name then
               signature env n ~local ~source rs d.rec_fun
          ) defs
    end

(* Declare 'id' from cloned definition 'def' in theory 'th' *)
let declaration env n ~local ~source ~container ~target =
  if not @@ Sid.mem target env.declared then
    try
      env.declared <- Sid.add target env.declared ;
      let m = Why3.Pmodule.restore_module container in
      let pd = Why3.Ident.Mid.find target (Why3.Pmodule.mod_known_intf m) in
      mdecl env n ~local ~source pd ~target
    with Not_found ->
      let td = Why3.Ident.Mid.find target container.th_known in
      try decl env n ~local ~source td ~target
      with Not_found ->
        assert false

(* Declare 'id' to be cloned as instance 'order' *)
let instance env n order (cloned : Why3.Theory.theory) =
  begin
    env.cloned <- max order env.cloned ;
    let r = Id.resolve ~lib:env.src.lib cloned.th_name in
    let title = Format.asprintf "%a" Id.pp_title r in
    let { current ; package_url } = env in
    let pp_href = Id.pp_ahref ~current ~package_url in
    Pdoc.printf env.out
      "%a<span class=\"comment\">\
       (* cloned <a name=\"clone-%d\" href=\"%a\">%s</a> *)\
       </span>@\n"
      Pdoc.pp_spaces n order pp_href r title
  end

(* -------------------------------------------------------------------------- *)
(* --- Flushing Declarations                                              --- *)
(* -------------------------------------------------------------------------- *)

let process_clone_proofs env clones =
  let stuck,proved = List.fold_left
      (fun (s,p) clone ->
         match clone with
         | Docref.Inst _ -> s,p
         | Clone { target } ->
           match Docref.find_proof target env.theory with
           | None -> (s,p)
           | Some c -> s + Crc.get_stuck c, p + Crc.get_proved c
      ) (0,0) clones
  in
  if stuck + proved <> 0 then
    Pdoc.pp env.out pp_verdict (Crc.nverdict ~stuck ~proved)

let process_clone_axioms env clones =
  match env.theory with
  | None -> ()
  | Some theory ->
    let signature = theory.signature in
    let hs = List.fold_left
        (fun hs (clone : Docref.clone) ->
           match clone with
           | Inst _ -> hs
           | Clone { target } ->
             match Axioms.parameter signature target with
             | None -> hs
             | Some p -> add_axiom hs p
        ) free clones in
    Pdoc.pp env.out pp_axioms hs

let process_clone_section env ~local ~indent ~container ~kind clones =
  if clones <> [] then
    begin
      text env ;
      Pdoc.printf env.out
        "<span class=\"section\">\
         {<span class=\"section-toggle\">%s</span>\
         <span class=\"section-text\">@\n\
         %a<span class=\"comment section-toggle\">begin</span>@\n"
        kind Pdoc.pp_spaces (indent+2) ;
      List.iter
        (function
          | Docref.Inst { order ; cloned } ->
            instance env (indent+4) order cloned
          | Docref.Clone { order ; target ; source } ->
            env.cloned <- max order env.cloned ;
            declaration env (indent+4) ~local ~source ~container ~target
        ) clones ;
      Pdoc.printf env.out
        "%a<span class=\"comment section-toggle\">end</span>@\n\
         %a</span>}</span>"
        Pdoc.pp_spaces (indent+2) Pdoc.pp_spaces indent ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Clone Instance Parsing                                             --- *)
(* -------------------------------------------------------------------------- *)

let process_clone_target env id =
  begin
    match Docref.find_clone ?instance:env.instance ~source:id () with
    | None | Some (Inst _) -> ()
    | Some (Clone c) ->
      let tgt = Id.resolve ~lib:env.src.lib c.target in
      let title = Format.asprintf "%a" Id.pp_title tgt in
      let { current ; package_url } = env in
      let href fmt = Id.pp_ahref ~current ~package_url fmt tgt in
      Pdoc.ppt env.out (pp_mark ~href ~title ~cla:icon_parameter)
  end

let process_clone env ~indent ~export id =
  begin
    env.instance <- None ;
    match env.theory with
    | None -> ()
    | Some thy ->
      let scope = Cloning.instance ~cloned:id ~scope:env.scopes export in
      match Docref.find_instance thy.instances ~scope id with
      | None -> ()
      | Some (Instance s) as inst ->
        env.instance <- inst ;
        let filter = List.filter @@ function
          | Docref.Clone { target } -> not @@ Sid.mem target env.declared
          | Inst { order } -> env.cloned < order && order <= s.order in
        let iclones = filter thy.iclones in
        let dclones = filter s.clones in
        let local = List.rev scope in
        let container = thy.theory in
        process_clone_section env ~local ~indent ~container ~kind:"#" iclones ;
        process_clone_section env ~local ~indent ~container ~kind:"…" dclones ;
        process_clone_axioms env dclones ;
        process_clone_proofs env dclones ;
        pp_space env ;
  end

let process_clone_token env (tok : Cloning.token) =
  let next , action = Cloning.process env.cloning env.input tok in
  env.cloning <- next ;
  match action with
  | `Nop -> ()
  | `Space -> pp_space env
  | `Spaces n -> pp_spaces env n
  | `Target(id,n) -> process_clone_target env id ; pp_spaces env n
  | `Clone(indent,export,_,id) -> process_clone env ~indent ~export id

(* -------------------------------------------------------------------------- *)
(* --- References                                                         --- *)
(* -------------------------------------------------------------------------- *)

let resolve env ?(infix=false) () =
  Docref.resolve
    ~src:env.src ~theory:env.theory
    ~infix (Token.position env.input)

let process_href env (href : Docref.href) s =
  match href with

  | Def(id,proof) ->
    env.declared <- Sid.add id.self env.declared ;
    env.cloning <- Cloning.init ;
    if id.id_qid = [] then
      Pdoc.pp env.out Pdoc.pp_html s
    else
      Pdoc.printf env.out "<a id=\"%a\">%a</a>" Id.pp_aname id Pdoc.pp_html s ;
    process_axioms env id ;
    process_proof env id proof

  | Ref id ->
    let { current ; package_url } = env in
    Pdoc.printf env.out "<a title=\"%a\" href=\"%a\">%a</a>"
      Id.pp_title id
      (Id.pp_ahref ~current ~package_url) id
      Pdoc.pp_html s ;
    process_clone_token env (`Id id.self)

  | NoRef ->
    begin
      match s with
      | "=" -> process_clone_token env `Equal
      | "." -> process_clone_token env `Dot
      | _ -> process_clone_token env (`Name s)
    end ;
    Pdoc.pp_html_s env.out s

(* -------------------------------------------------------------------------- *)
(* --- Foldable Sections                                                  --- *)
(* -------------------------------------------------------------------------- *)

let pp_active fmt b =
  if b then Format.fprintf fmt " active"

let process_open_section env ~active title =
  text env ;
  env.section <- succ env.section ;
  Pdoc.printf env.out
    "<span class=\"section level%d\">\
     <span class=\"comment\">{</span>\
     <span class=\"attribute section-toggle\">%s</span>\
     <span class=\"comment section-text%a\">…</span>\
     <span class=\"comment\">}</span>\
     <span class=\"section-text%a\">"
    (min env.section 3) title pp_active (not active) pp_active active

let process_close_section env title =
  env.section <- pred env.section ;
  text env ;
  Pdoc.printf env.out
    "<span class=\"comment\">{</span>\
     <span class=\"attribute section-toggle\">%s</span>\
     <span class=\"comment\">}</span>\
     </span></span>"
    (if title = "" then "…" else title)

(* -------------------------------------------------------------------------- *)
(* --- Module & Theory Processing                                         --- *)
(* -------------------------------------------------------------------------- *)

let process_open_module env key =
  begin
    if not (env.mode = Body && env.opened = []) then
      Token.error env.input "unexpected %s" key ;
    let forking = env.block in
    let prelude = Pdoc.buffered env.out in
    let id = Token.fetch_id ~kind:key env.input in
    let kind = String.capitalize_ascii key in
    let url = Docref.derived env.src id in
    let path = String.concat "." env.src.lib in
    let theory = Docref.Mstr.find_opt id env.src.theories in
    bsync ~wanted:Raw env ;
    Pdoc.printf env.out
      "<pre class=\"src\">%a <a title=\"%s.%s\" href=\"%s\">%s</a>"
      Pdoc.pp_keyword key path id url id ;
    Pdoc.printf env.crc
      "<pre class=\"src\">%a <a id=\"%s\" href=\"%s\">%s.%s</a>"
      Pdoc.pp_keyword key id url path id ;
    process_axioms_summary env id ~crc:true theory ; (* out & crc *)
    process_proofs_summary env id ~crc:true theory ; (* out & crc *)
    Pdoc.printf env.out "</pre>@." ;
    Pdoc.printf env.crc "</pre>@." ;
    let file = Filename.concat env.dir url in
    let title = Printf.sprintf "%s %s.%s" kind path id in
    Pdoc.fork env.out ~file ~title ;
    Pdoc.printf env.out
      "<header>\
       <a href=\"index.html\">index</a> — \
       <code>library <a href=\"%s.index.html\">%s</a></code> — \
       <code>module %s</code>\
       </header>@."
      env.src.urlbase path id ;
    env.mode <- Body ;
    env.block <- Raw ;
    if prelude <> "" then
      begin
        bsync ~wanted:forking env ;
        Pdoc.pp env.out Format.pp_print_string prelude ;
        bsync ~wanted:Raw env ;
      end ;
    push env Pre ;
    env.block <- Raw ;
    env.file <- Pre ;
    bsync env ;
    text env ;
    env.current <- Some id ;
    env.theory <- theory ;
    env.instance <- None ;
    env.cloned <- 0 ;
    env.declared <- Sid.empty ;
    Pdoc.pp env.out Pdoc.pp_keyword key ;
    Pdoc.printf env.out " %a" Pdoc.pp_scope id ;
    process_axioms_summary env id ~crc:false theory ;
    process_proofs_summary env id ~crc:false theory ;
    process_module_axioms env ;
  end

let process_close_module env key =
  begin
    text env ;
    Pdoc.printf env.out "%a@\n</pre>@\n" Pdoc.pp_keyword key ;
    Pdoc.close env.out ;
    env.mode <- Body ;
    env.block <- Raw ;
    env.file <- Body ;
    env.current <- None ;
    env.theory <- None ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Identifiers                                                        --- *)
(* -------------------------------------------------------------------------- *)

let process_opening s env =
  begin
    env.opened <- s :: env.opened ;
    if s = "scope" then
      let id = Token.fetch_id ~kind:s env.input in
      begin
        Pdoc.printf env.out " %a" Pdoc.pp_scope id ;
        env.scopes <- id :: env.scopes ;
      end
  end

let process_closing s env =
  match env.opened with
  | k0::ks ->
    if k0 = "scope" then env.scopes <- List.tl env.scopes ;
    env.opened <- ks
  | [] ->
    Token.error env.input "No matching opening keyword for '%s" s

let process_ident env s =
  if Docref.is_keyword s then
    begin
      match s with
      | "module" | "theory" -> process_open_module env s
      | "end" when env.opened = [] -> process_close_module env s
      | "assume" ->
        Pdoc.pp env.out Pdoc.pp_admitted s
      | _ ->
        process_clone_token env (`Kwd s) ;
        if Docref.is_closing s then process_closing s env ;
        text env ;
        Pdoc.pp env.out Pdoc.pp_keyword s ;
        if Docref.is_opening s then process_opening s env ;
    end
  else
    begin
      text env ;
      let href = resolve env () in
      process_href env href s ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Style Processing                                                   --- *)
(* -------------------------------------------------------------------------- *)

let process_style env m =
  text env ;
  if env.mode = m then pop env else push env m

let rec list env n =
  match env.mode with
  | Body | Head _ | Pre -> ()
  | Emph -> Token.error env.input "unclosed bold style"
  | Bold -> Token.error env.input "unclosed emphasis style"
  | Par -> pop env ; push env (List n)
  | Div -> push env (List n)
  | Item _ -> pop env ; list env n
  | List m ->
    if n < m then
      begin
        pop env ;
        list env n ;
      end
    else
    if n > m then
      push env (List n)

let process_dash env s =
  let s = String.length s in
  if s = 1 && Token.startline env.input then
    list env (Token.indent env.input)
  else
    begin
      text env ;
      Pdoc.printf env.out "&%cdash;" (if s > 1 then 'm' else 'n')
    end

let process_header env h =
  begin
    bsync ~wanted:Doc env ;
    let level = String.length h in
    let buffer = Pdoc.push env.out in
    push env (Head(buffer,level))
  end

(* -------------------------------------------------------------------------- *)
(* --- Space & Newline Processing                                           --- *)
(* -------------------------------------------------------------------------- *)

let process_space env =
  match env.mode with
  | Body | Div | List _ -> ()
  | Par | Item _ | Head _ | Emph | Bold -> env.space <- true
  | Pre ->
    match env.indent with
    | Head n -> env.indent <- Head (succ n)
    | Lines(l,n) -> env.indent <- Lines(l,succ n)
    | Code ->
      bsync env ;
      match Cloning.space env.cloning with
      | Some state -> env.cloning <- state
      | None -> pp_space env

let process_newline env =
  match env.mode with
  | Body -> if Token.emptyline env.input then Pdoc.flush env.out
  | Div -> ()
  | Par | List _ | Item _ ->
    if Token.emptyline env.input then
      close ~parblock:true env
    else
      env.space <- true
  | Emph | Bold -> env.space <- true
  | Head(buffer,level) ->
    head env buffer level ;
    pop env
  | Pre ->
    match env.indent with
    | Lines(n,_) -> env.indent <- Lines(succ n,0)
    | Head _ -> env.indent <- Head 0
    | Code ->
      process_clone_token env `Newline ;
      env.indent <- Lines(1,0)

(* -------------------------------------------------------------------------- *)
(* --- References                                                         --- *)
(* -------------------------------------------------------------------------- *)

let process_reference ~wenv ~env r =
  try
    let { src ; current ; package_url } = env in
    let name, id = Docref.reference ~wenv ~src ~current r in
    let r = Id.resolve ~lib:src.lib id in
    text env ;
    Pdoc.printf env.out
      "<code class=\"src\"><a title=\"%a\" href=\"%a\">%s</a></code>"
      (* markdown reference might be emitted from any file *)
      Id.pp_title r (Id.pp_ahref ~current:None ~package_url) r name
  with
  | Not_found -> Token.error env.input "unknown reference"
  | Failure msg -> Token.error env.input "%s" msg

(* -------------------------------------------------------------------------- *)
(* --- Token Processing                                                   --- *)
(* -------------------------------------------------------------------------- *)

let parse env =
  let wenv = env.wenv in
  let out = env.out in
  while not (Token.eof env.input) do
    match Token.token env.input with
    | Eof -> close env ; bsync env
    | Char ',' ->
      process_clone_token env `Coma ;
      text env ;
      Pdoc.pp_print_char out ','
    | Char c -> text env ; Pdoc.pp_html_c out c
    | Text s -> text env ; Pdoc.pp_html_s out s
    | Comment s ->
      if env.mode = Pre then
        begin
          process_clone_token env `Comment ;
          text env ;
          Pdoc.pp_html_s out ~className:"comment" s ;
        end
    | Verb s ->
      text env ; Pdoc.printf out "<code class=\"src\">%a</code>" Pdoc.pp_html s
    | Ref s -> process_reference ~wenv ~env s
    | Style(Emph,_) -> process_style env Emph
    | Style(Bold,_) -> process_style env Bold
    | Style(Head,h) -> process_header env h
    | Style(Dash,n) -> process_dash env n
    | Style _ -> ()
    | OpenDoc ->
      begin
        Pdoc.flush ~onlyspace:false out ;
        close env ;
        push env Div ;
      end
    | CloseDoc ->
      begin
        close env ;
        push env env.file ;
      end
    | OpenSection(active,title) -> process_open_section env ~active title
    | CloseSection title -> process_close_section env title
    | Space -> process_space env
    | Newline -> process_newline env
    | Ident s -> process_ident env s
    | Infix s ->
      text env ;
      let href = resolve env ~infix:true () in
      process_href env href s
  done

(* -------------------------------------------------------------------------- *)
(* --- Chapters                                                           --- *)
(* -------------------------------------------------------------------------- *)

type chapter = { src: Docref.source option ; href: string ; title: string }
let chapters = ref []
let chapter ?src ~href ~title () =
  chapters := { src ; href ; title } :: !chapters

let documentation () =
  let rec docs cs = function
    | [] -> cs
    | c::rs -> docs (if c.src = None then c::cs else cs) rs
  in docs [] !chapters

let package () =
  let cmap = ref Docref.Mstr.empty in
  List.iter
    (fun c ->
       Option.iter
         (fun (src : Docref.source) ->
            let path = String.concat "." src.lib in
            cmap := Docref.Mstr.add path c !cmap ;
         ) c.src
    ) !chapters ;
  let mark = ref [] in
  let pool = ref [] in
  let rec walk c =
    Option.iter
      (fun (src : Docref.source) ->
         if not (List.memq c !mark) then
           begin
             mark := c :: !mark ;
             Docref.Mstr.iter
               (fun _ (thy : Docref.theory) ->
                  List.iter
                    (fun (thd : Why3.Theory.theory) ->
                       let lp,_,_ = Id.path ~lib:src.lib thd.th_name in
                       let path = String.concat "." lp in
                       try walk @@ Docref.Mstr.find path !cmap
                       with Not_found -> ()
                    ) thy.depends
               ) src.theories ;
             pool := c :: !pool
           end ;
      ) c.src
  in
  Docref.Mstr.iter (fun _ c -> walk c) !cmap ;
  List.rev !pool

(* -------------------------------------------------------------------------- *)
(* --- Titling Document                                                   --- *)
(* -------------------------------------------------------------------------- *)

let document_title ~title ~page =
  if title <> "" then Printf.sprintf "%s — %s" title page else page

(* -------------------------------------------------------------------------- *)
(* --- Markdown File Processing                                           --- *)
(* -------------------------------------------------------------------------- *)

let process_markdown ~wenv ~henv ~senv ~out:dir ~title ~package_url file =
  begin
    let src = Docref.empty () in
    let input = Token.input ~doc:true file in
    let page = Filename.chop_extension @@ Filename.basename file in
    let title = document_title ~title ~page in
    let href = page ^ ".html" in
    let ofile = Filename.concat dir href in
    let out = Pdoc.output ~file:ofile ~title in
    let crc = Pdoc.null () in
    let env = {
      dir ; src ; package_url; input ; out ; crc ; wenv ; henv ; senv ;
      space = false ;  indent = Code ;
      block = Raw; mode = Body ; file = Body ; stack = [] ;
      current = None ; theory = None ; instance = None ;
      cloning = Cloning.init ; cloned = 0 ;
      proof_href = 0 ;
      declared = Sid.empty ;
      opened = [] ; scopes = [] ;
      section = 0 ;
    } in
    chapter ~href ~title () ;
    Pdoc.printf env.out
      "<header>\
       <a href=\"index.html\">index</a> — %s
       </header>@." title ;
    Pdoc.flush out ;
    push env Div ;
    parse env ;
    Pdoc.close_all out ;
  end

(* -------------------------------------------------------------------------- *)
(* --- MLW File Processing                                                --- *)
(* -------------------------------------------------------------------------- *)

let process_source ~wenv ~henv ~senv ~out:dir ~package_url file
    (src : Docref.source) =
  let title = Printf.sprintf "Library %s" src.urlbase in
  let href = src.urlbase ^ ".index.html"in
  let pref = src.urlbase ^ ".proof.html"in
  let ofile = Filename.concat dir href in
  let pfile = Filename.concat dir pref in
  let out = Pdoc.output ~file:ofile ~title in
  let crc = Pdoc.output ~file:pfile ~title in
  let input = Token.input file in
  let env = {
    dir ; src ; package_url; input ; out ; crc ; wenv ; henv ; senv ;
    space = false ; indent = Code ;
    block = Raw ; mode = Body ; file = Body ; stack = [] ;
    current = None ; theory = None ; instance = None ;
    cloning = Cloning.init ; cloned = 0 ;
    proof_href = 0 ;
    declared = Sid.empty ;
    opened = [] ; scopes = [] ; section = 0 ;
  } in
  begin
    chapter ~src ~href ~title () ;
    Pdoc.printf out
      "<header>\
       <a href=\"index.html\">index</a> — <code>library %s</code>\
       </header>@." src.urlbase ;
    Pdoc.flush out ;
    Pdoc.printf crc
      "<header>\
       <a href=\"index.html\">index</a> — \
       <code>library <a href=\"%s.index.html\">%s</a></code> — \
       <code>proofs</code>\
       </header>@." src.urlbase src.urlbase ;
    Pdoc.flush crc ;
    Pdoc.printf crc "<h1>Provers</h1>@\n<pre class=\"src\">" ;
    Calibration.iter
      (fun p n t ->
         Pdoc.printf crc "@\n  %-12s @%-6s n=%-3d %a"
           (Prover.desc_name p) (Prover.desc_version p) n Utils.pp_time t
      ) src.profile ;
    Pdoc.printf crc "@\n</pre>@." ;
    Pdoc.printf crc "<h1>Proofs</h1>@\n" ;
    parse env ;
    Pdoc.close_all out ;
    Pdoc.close_all crc ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Generating Index                                                   --- *)
(* -------------------------------------------------------------------------- *)

let process_chapter_proofs out (src : Docref.source) =
  let (stuck,proved) =
    Docref.Mstr.fold
      (fun _ (thy : Docref.theory) acc ->
         Docref.Mstr.fold
           (fun _ (crc : Crc.crc) (stuck,proved) ->
              stuck + Crc.get_stuck crc,
              proved + Crc.get_proved crc
           ) thy.proofs acc
      ) src.theories (0,0)
  in Pdoc.pp out pp_verdict (Crc.nverdict ~stuck ~proved)

let process_chapter_axioms out (senv : Soundness.env) (src : Docref.source) =
  let hs =
    Docref.Mstr.fold
      (fun _ (thy : Docref.theory) hs ->
         let hs =
           List.fold_left add_axiom hs @@ Axioms.parameters thy.signature in
         let sound = Soundness.compute senv thy in
         { hs with sound = Soundness.merge hs.sound sound }
      ) src.theories free
  in Pdoc.pp out pp_axioms hs

let pp_chapters out ~senv ~title cs =
  if cs <> [] then
    begin
      Pdoc.printf out "<h1>%s</h1>@\n<div class=\"doc\">@\n<ul>@\n" title ;
      List.iter
        (fun c ->
           Pdoc.printf out "<li><a href=\"%s\">%s</a>" c.href c.title ;
           Option.iter (process_chapter_axioms out senv) c.src ;
           Option.iter (process_chapter_proofs out) c.src ;
           Pdoc.printf out "</li>@\n" ;
        ) cs ;
      Pdoc.printf out "</ul>@\n</div>@." ;
    end

let index ~out:dir ~senv ~title =
  let href = "index.html" in
  if List.for_all (fun c -> c.href <> href) !chapters then
    begin
      let ofile = Filename.concat dir href in
      let out = Pdoc.output ~file:ofile ~title in
      Pdoc.printf out
        "<header>index — %s</header>@." title ;
      pp_chapters out ~senv ~title:"Documentation" @@ documentation () ;
      pp_chapters out ~senv ~title:"Development" @@ package () ;
      Pdoc.flush out ;
      Pdoc.close_all out ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Main Doc Command                                                   --- *)
(* -------------------------------------------------------------------------- *)

let shared ~out ~file =
  let tgt = Filename.concat out file in
  let src = Meta.shared file in
  Utils.copy ~src ~tgt

let preprocess ~henv ~wenv ~senv file =
  file ,
  if Filename.check_suffix file ".md" then None else
  if Filename.check_suffix file ".mlw" then
    let src = Docref.parse ~henv ~wenv file in
    Soundness.register senv src ; Some src
  else
    Utils.failwith "Don't known what to do with %S" file

let process ~wenv ~henv ~out ~title (file,kind) =
  match kind with
  | None -> process_markdown ~wenv ~henv ~out ~title file
  | Some src -> process_source ~wenv ~henv ~out file src

let generate ~(env:Project.env) ~out ~title ~files ~url ~package_url =
  begin
    (* Why3 environment *)
    let wenv = env.why3.env in
    (* Axioms config *)
    let henv = Axioms.init env in
    (* Sounness config *)
    let senv = Soundness.init () in
    (* Shared resources *)
    Utils.mkdirs @@ Filename.concat out "fonts" ;
    shared ~out ~file:"style.css" ;
    shared ~out ~file:"script.js" ;
    shared ~out ~file:"icofont.min.css" ;
    shared ~out ~file:"fonts/icofont.woff" ;
    shared ~out ~file:"fonts/icofont.woff2" ;
    (* Pre-processing *)
    let sources = List.map (preprocess ~wenv ~henv ~senv) files in
    (* Doc generation *)
    List.iter (process ~wenv ~henv ~senv ~title ~out ~package_url) sources ;
    (* Indexing *)
    index ~out ~senv ~title ;
    (* Final output *)
    Log.emit "Generated %s%s/index.html"
      (if url then "file://" else "") (Utils.absolute out) ;
  end

(* -------------------------------------------------------------------------- *)
