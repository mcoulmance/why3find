(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Hammer Proof Strategy                                              --- *)
(* -------------------------------------------------------------------------- *)

open Fibers.Monad

type hammer = {
  level : int ;
  replay : bool ;
  minimize : bool ;
  env : Project.env ;
  client : Client.client option ;
  preprocess : Tactic.tactic option ;
  provers : Prover.prover list ;
  tactics : Tactic.tactic list ;
  profile : Calibration.profile ;
}

let provers h = h.provers
let tactics h = h.tactics

type node = {
  goal : Session.goal ;
  hint : Crc.crc ;
  depth : int ;
  hammer : hammer ;
  parent : parent ;
  mutable state : state ;
}

and parent =
  | Root of (Crc.crc -> unit)
  | Parent of node

and state =
  | Paused            (* in q_paused *)
  | Scheduled         (* in q_broken or q_proven *)
  | Closed of Crc.crc (* not in any queue *)
  | Stacked of Tactic.tactic * node list (* children still queued *)

let pp_node fmt n = Session.pp_goal fmt n.goal

let goal n = n.goal
let state n = n.state
let parent n = n.parent

let stuck _n = Crc.stuck

let rec proof n =
  match n.state with
  | Closed crc -> crc
  | Paused | Scheduled -> stuck n
  | Stacked(tactic,children) ->
    Crc.tactic tactic (List.map proof children)

let pp_state fmt = function
  | Paused | Scheduled -> Format.fprintf fmt "Paused"
  | Closed crc -> Crc.pp_proof fmt crc
  | Stacked(tactic,nodes) ->
    let proofs = List.map proof nodes in
    Format.fprintf fmt "[%a] %a" Tactic.pretty tactic Crc.pp_proofs proofs

let rec update result = function
  | Root callback ->
    callback result ;
  | Parent n ->
    try match n.state with
      | Stacked(tactic, children) ->
        let cexit = function { state = Closed crc } -> crc | _ -> raise Exit in
        let result = Crc.tactic tactic (List.map cexit children) in
        n.state <- Closed result ;
        update result n.parent
      | _ -> ()
    with Exit -> ()

(* -------------------------------------------------------------------------- *)
(* --- Queue Management                                                   --- *)
(* -------------------------------------------------------------------------- *)

let failed = Fibers.signal ()
let on_failed = Fibers.on failed
let off_failed = Fibers.off failed
let filters = Fibers.Queue.create ()
let on_filter fn = Fibers.Queue.push filters fn
let off_filter fn = Fibers.Queue.filter (fun f -> f != fn) filters
let p_proven = ref 0
let p_broken = ref 0
let q_paused = Fibers.Queue.create () (* Paused nodes *)
let q_broken = Queue.create () (* Stuck hints, scheduled first *)
let q_proven = Queue.create () (* Complete hints, scheduled last *)
let q_pause n =
  n.state <- Paused ; Fibers.Queue.push q_paused n
let q_close n crc =
  begin
    n.state <- Closed crc ;
    Session.commit n.goal crc ;
    update crc n.parent ;
  end

let q_schedule n =
  try
    Fibers.Queue.iter (fun fn -> if not (fn n) then raise Exit) filters ;
    n.state <- Scheduled ;
    Queue.push n @@
    if Crc.is_complete n.hint then q_proven else q_broken ;
  with Exit -> q_pause n

let iter f =
  begin
    Fibers.Queue.iter f q_paused ;
    Queue.iter f q_broken ;
    Queue.iter f q_proven ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Stepping                                                           --- *)
(* -------------------------------------------------------------------------- *)

type runmode = RUNNING | PAUSING | STOPPING
let runmode = ref RUNNING

let subgoal n =
  match !runmode with
  | STOPPING -> q_close n (proof n)
  | RUNNING -> q_schedule n
  | PAUSING -> q_pause n

let schedule n =
  match n.state with
  | Paused -> q_schedule n
  | Scheduled | Stacked _ | Closed _ -> ()

let terminate n =
  match n.state with
  | Closed _ -> ()
  | _ -> q_close n @@ proof n

let resume () =
  begin
    runmode := RUNNING ;
    Fibers.Queue.flush schedule q_paused ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Interrupt All                                                      --- *)
(* -------------------------------------------------------------------------- *)

let killall = Fibers.signal ()

let pause () =
  begin
    runmode := PAUSING ;
    Queue.iter q_pause q_proven ;
    Queue.iter q_pause q_broken ;
    Queue.clear q_proven ;
    Queue.clear q_broken ;
    Fibers.emit killall () ;
  end

let stop () =
  begin
    runmode := STOPPING ;
    iter terminate ;
    Queue.clear q_proven ;
    Queue.clear q_broken ;
    Fibers.Queue.clear q_paused ;
    Fibers.emit killall () ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Scheduling Nodes                                                   --- *)
(* -------------------------------------------------------------------------- *)

let node hammer ~parent ?hint goal =
  let hint = match hint with Some h -> h | None -> Crc.stuck in
  let depth = match parent with Root _ -> 0 | Parent n -> succ n.depth in
  {
    hammer ; goal ;
    parent ; depth ;
    hint ;
    state = Paused ;
  }

let push hammer ~parent goal hint =
  let node = node hammer ~parent ~hint goal in
  subgoal node ; node

let rec push_all hammer ~parent goals hints =
  match goals, hints with
  | [], _ -> []
  | g::gs, [] ->
    let s = Crc.stuck in
    push hammer ~parent g s :: push_all hammer ~parent gs []
  | g::gs, h::hs ->
    push hammer ~parent g h :: push_all hammer ~parent gs hs

let decompose node tactic gs hs =
  let children = push_all node.hammer ~parent:(Parent node) gs hs in
  if children = [] then
    q_close node @@ Crc.tactic tactic []
  else
    node.state <- Stacked(tactic,children)

let pop () =
  try Some (Queue.pop q_broken) with Queue.Empty ->
  try Some (Queue.pop q_proven) with Queue.Empty ->
    None

type resolution = Resolved | NotFound | Interrupted
type strategy = node -> resolution Fibers.t

let pp_resolution fmt = function
  | Resolved -> Format.pp_print_string fmt "Resolved"
  | NotFound -> Format.pp_print_string fmt "NotFound"
  | Interrupted -> Format.pp_print_string fmt "Interrupted"
[@@warning "-32"]

let notfound _ =
  Fibers.return @@
  match !runmode with STOPPING | PAUSING -> Interrupted | RUNNING -> NotFound

let (@>) (h : strategy) : strategy = fun n ->
  match !runmode with
  | STOPPING | PAUSING -> Fibers.return Interrupted
  | RUNNING -> try h n with Not_found -> notfound ()

let (>>>) (h1 : strategy) (h2 : strategy) : strategy = fun n ->
  let* r = h1 @> n in
  match r with
  | Resolved | Interrupted -> Fibers.return r
  | NotFound -> h2 @> n

let rec sequence (f : 'a -> strategy) (xs : 'a list) : strategy =
  match xs with
  | [] -> notfound
  | x::xs -> f x >>> sequence f xs

(* -------------------------------------------------------------------------- *)
(* --- Try Prover on Node                                                 --- *)
(* -------------------------------------------------------------------------- *)

type verdict = Valid of Prover.prover * float | Unknown | Killed

let pp_verdict fmt = function
  | Valid(p,t) ->
    Format.fprintf fmt "Valid %a %a" Prover.pp_prover p Utils.pp_time t
  | Unknown -> Format.pp_print_string fmt "Unknown"
  | Killed -> Format.pp_print_string fmt "Killed"
[@@warning "-32"]

let merge u v =
  match u, v with
  | Unknown , w | w , Unknown -> w
  | Killed , w | w , Killed -> w
  | Valid(_,t) , Valid(_,t') -> if t <= t' then u else v

let prover ?cancel timeout prv = fun n ->
  let h = n.hammer in
  let wmain = Why3.Whyconf.get_main h.env.why3.config in
  let name = Session.goal_name n.goal in
  let* alpha = Calibration.velocity wmain h.env.cache h.profile prv in
  let to_local = Result.map (fun t -> t *. alpha) in
  let to_profile = Result.map (fun t -> t /. alpha) in
  let runner_time = timeout *. alpha in
  let task = Session.goal_task n.goal in
  let callback = Session.result n.goal in
  let killed = ref false in
  let+ verdict =
    match Runner.prove_cached h.env.cache prv task runner_time with
    | Some result ->
      Runner.notify wmain prv result callback ;
      Fibers.return (to_profile result)
    | None ->
      let ptask = Runner.prepare prv task in
      let kill = Fibers.signal () in
      let handler () = killed := true ; Fibers.emit kill () in
      let runner =
        Fibers.monitor ?signal:cancel ~handler @@
        Fibers.monitor ~signal:killall ~handler @@
        Fibers.map to_profile @@
        Fibers.finally ~callback:(Cache.set h.env.cache task prv) @@
        Runner.prove_prepared wmain ~name ~cancel:kill ~callback
          prv ptask runner_time ~idename:(Session.goal_idename n.goal)
      in match h.client with
      | None -> runner
      | Some cl ->
        (* Don't ask server for too low timeouts *)
        if timeout < 0.2 *. h.env.config.time then runner else
          let signal = Client.request cl h.profile prv ptask timeout in
          let handler result =
            match Result.crop ~timeout result with
            | Some result ->
              let local = to_local result in
              (* TODO: cache this not here but in the prooftree cache *)
              (* Cache.set h.cache task prover local ; *)
              Runner.notify wmain prv local callback ;
              Fibers.set runner result ;
              Fibers.emit kill ()
            | None -> ()
          in Fibers.monitor ~signal ~handler runner
  in match verdict with
  | Valid t when t <= timeout -> Valid(prv,Utils.round t)
  | _ -> if !killed then Killed else Unknown

type result = Local of Result.t | Profile of Result.t

let result n prover =
  let cache = n.hammer.env.cache in
  let profile = n.hammer.profile in
  let result = Cache.get cache (Session.goal_task n.goal) prover in
  try
    let alpha = Calibration.observed profile prover in
    Profile (Result.map (fun t -> t /. alpha) result)
  with Not_found ->
    Local result

let results (n : node) = List.map (fun p -> p, result n p) n.hammer.provers

let pp_result fmt = function
  | Profile r -> Result.pp_result fmt r
  | Local r -> Format.fprintf fmt "%a (local)" Result.pp_result r

(* -------------------------------------------------------------------------- *)
(* --- Tactic Hint                                                        --- *)
(* -------------------------------------------------------------------------- *)

type hint = Tactic.tactic * Crc.crc list
let pp_hint fmt (h : hint) = Tactic.pretty fmt (fst h)

(* -------------------------------------------------------------------------- *)
(* --- Try Transformation on Node                                         --- *)
(* -------------------------------------------------------------------------- *)

let apply tac ?(hint : hint option) (n : node) : bool =
  let env = n.hammer.env in
  if n.depth > env.config.depth then false else
    match Session.apply env.why3.env tac n.goal with
    | None -> false
    | Some gs ->
      let hs =
        match hint with
        | Some(t,hs) when Tactic.equal t tac -> hs
        | _ -> [] in
      decompose n tac gs hs ; true

(* -------------------------------------------------------------------------- *)
(* --- Hammer Entry Points                                                --- *)
(* -------------------------------------------------------------------------- *)

let create
    ?(level=4)
    ?(replay=false)
    ?(minimize=false)
    ?(preprocess : Tactic.tactic option option)
    ?(provers : Prover.prover list option)
    ?(tactics : Tactic.tactic list option)
    ?(profile : Calibration.profile option)
    (env : Project.env) : hammer =
  let provers =
    match provers with Some ps -> ps | None ->
      Prover.select env.why3 ~patterns:env.config.provers in
  let tactics =
    match tactics with Some ts -> ts | None ->
      Tactic.select env.why3.env env.config.tactics in
  let preprocess =
    match preprocess with Some pr -> pr | None ->
      Option.bind env.config.preprocess (Tactic.lookup env.why3.env) in
  let profile =
    match profile with Some pr -> pr | None ->
      Calibration.default env in
  {
    level ; replay ; minimize ; env ;
    client = Client.connect env ;
    provers ; tactics ; profile ; preprocess ;
  }

let prove hammer ?(callback=ignore) goal hint =
  let parent = Root callback in
  match hammer.preprocess with
  | None -> push hammer ~parent goal hint
  | Some tactic ->
    match
      Session.apply hammer.env.why3.env tactic ~allow_no_effect:true goal
    with
    | None ->
      Log.warning "Ignored goal %s: preprocessing tactic %a failed"
        (Session.goal_name goal) Tactic.pretty tactic ;
      push hammer ~parent goal hint
    | Some goals ->
      let hints = match hint with
        | Tactic { id; children; _ } when id = Tactic.name tactic -> children
        | _ -> [] in
      let root = node hammer ~parent ~hint goal in
      decompose root tactic goals hints ; root

(* -------------------------------------------------------------------------- *)
(* --- Hammer Strategy                                                    --- *)
(* -------------------------------------------------------------------------- *)

let hammer0 : strategy = fun n ->
  let h = n.hammer in
  let t = h.env.config.fast in
  if h.level > 0 && t > 0.0 && List.length h.provers > 1
  then
    let hprove p n =
      let+ verdict = prover t p n in
      match verdict with
      | Killed -> Interrupted
      | Unknown -> NotFound
      | Valid(p,t) ->
        let crc = Crc.prover p t in
        q_close n crc ; Resolved
    in sequence hprove h.provers n
  else notfound ()

let hammer13 minlevel timefactor n =
  let h = n.hammer in
  if h.level <= minlevel then notfound () else
    let cancel = Fibers.signal () in
    let time = timefactor *. h.env.config.time in
    let callback = function Valid _ -> Fibers.emit cancel () | _ -> () in
    let+ verdicts =
      Fibers.all @@
      List.map
        (fun prv -> Fibers.finally ~callback @@ prover ~cancel time prv n)
        n.hammer.provers in
    match List.fold_left merge Unknown verdicts with
    | Unknown -> NotFound
    | Killed -> Interrupted
    | Valid(p,t) ->
      let crc = Crc.prover p t in
      q_close n crc ; Resolved

let hammer1 : strategy = hammer13 1 1.0
let hammer3 : strategy = hammer13 3 2.0

let hammer2 ?hint : strategy = fun n ->
  Fibers.return @@
  if n.hammer.level > 2 then
    let rec aux = function
      | [] -> NotFound
      | t :: ts ->
        if apply t ?hint n then Resolved else aux ts
    in aux n.hammer.tactics
  else NotFound

let hammer =
  hammer0 >>> (* level -H 1 *)
  hammer1 >>> (* level -H 2 *)
  hammer2 >>> (* level -H 3 *)
  hammer3 >>> (* level -H 4 *)
  notfound

(* -------------------------------------------------------------------------- *)
(* --- Node Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let try_prover p v t : strategy = fun n ->
  match Prover.find n.hammer.provers ~name:p ?version:v () with
  | None -> notfound ()
  | Some p ->
    let base = n.hammer.env.config.time in
    (* Takes into account the current strategy and possible fluctuations *)
    let time = min (max base (t +. 0.5)) (2.0 *. base) in
    let+ verdict = prover time p n in
    match verdict with
    | Unknown -> NotFound
    | Killed -> Interrupted
    | Valid(p,t) ->
      let crc = Crc.prover p t in
      q_close n crc ; Resolved

let replay_prover p v t = try_prover p v t >>> notfound
let update_prover p v t = try_prover p v t >>> hammer

let try_tactic ~hint n =
  Fibers.return @@
  let rec aux = function
    | [] ->
      Log.warning "Goal %S: tactic '%a' not in strategy"
        (Session.goal_name n.goal) pp_hint hint ;
      NotFound
    | t::ts ->
      if Tactic.equal t (fst hint) && apply t ~hint n then
        Resolved
      else
        match Session.apply n.hammer.env.why3.env t n.goal with
        | None -> aux ts
        | Some _ ->
          Log.warning "Goal %S: tactic '%a' shadowed by '%a'"
            (Session.goal_name n.goal) pp_hint hint Tactic.pretty t ;
          NotFound
  in aux n.hammer.tactics

let replay_tactic ~hint = try_tactic ~hint
let reduce_tactic ?hint = (hammer1 >>> hammer2 ?hint >>> hammer3)
let update_tactic ?hint = (hammer2 ?hint >>> hammer)

let explore : strategy =
  fun n -> if n.hammer.replay then notfound () else hammer n

let execute n =
  try
    match n.hint with
    | Stuck -> explore n
    | Prover(p,v,t) ->
      if n.hammer.replay then
        replay_prover p v t n
      else
        update_prover p v t n
    | Tactic { id ; children } ->
      match Tactic.lookup n.hammer.env.why3.env id with
      | Some tac ->
        let hint = tac, children in
        if n.hammer.replay then
          replay_tactic ~hint n
        else
        if n.hammer.minimize then
          reduce_tactic ~hint n
        else
          update_tactic ~hint n
      | None -> explore n
  with exn ->
    Log.emit "Process Error (%s)" @@ Printexc.to_string exn ;
    Fibers.return Interrupted

(* -------------------------------------------------------------------------- *)
(* --- Queue Status                                                       --- *)
(* -------------------------------------------------------------------------- *)

type progress = {
  broken  : int ;
  proven  : int ;
  pending : int ;
  running : int ;
}

let progress () = {
  proven = Queue.length q_proven + !p_proven ;
  broken = Queue.length q_broken + !p_broken ;
  pending = Runner.pending () ;
  running = Runner.running () ;
}

let pp_progress fmt s =
  if s.proven > 0 then Format.fprintf fmt "!%d/" s.proven ;
  if s.broken > 0 then Format.fprintf fmt "?%d/" s.broken ;
  if s.pending > 0 then Format.fprintf fmt "%d/" s.pending ;
  Format.pp_print_int fmt s.running

let proving () =
  begin
    !p_proven > 0  || !p_broken > 0
    || Queue.length q_proven > 0
    || Queue.length q_broken > 0
    || Runner.pending () > 0
    || Runner.running () > 0
  end

let pending () = proving () || Fibers.pending () > 0
let pausing () = !runmode = PAUSING
let completing () = !runmode = RUNNING

type status =
  | Proving
  | Pausing
  | Paused
  | Stopping
  | Stopped
  | Complete

let status () =
  match !runmode with
  | PAUSING -> if proving () then Pausing else Paused
  | STOPPING -> if pending () then Stopping else Stopped
  | RUNNING -> if pending () then Proving else Complete

(* -------------------------------------------------------------------------- *)
(* --- Task Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let process () =
  begin
    Client.yield () ;
    Fibers.yield () ;
    match pop () with
    | Some node ->
      let pr = if Crc.is_complete node.hint then p_proven else p_broken in
      let callback r =
        decr pr ;
        match r with
        | Resolved -> ()
        | NotFound ->
          q_close node Crc.stuck ;
          Fibers.emit failed node ;
        | Interrupted ->
          if pausing () then q_pause node else q_close node Crc.stuck
      in incr pr ; Fibers.background (execute node) ~callback ; true
    | None -> false
  end

let run () =
  runmode := RUNNING ;
  while pending () do
    let s = progress () in
    Utils.progress "%a%t" pp_progress s Runner.pp_goals ;
    if not @@ process () then Unix.sleepf 0.01 ;
  done ;
  Client.terminate ()

(* -------------------------------------------------------------------------- *)
(* --- Why3 Hammer Config                                                 --- *)
(* -------------------------------------------------------------------------- *)

let why3_ide_config ~tactics ~provers ~time ~mem file =
  let c_skip fmt = ignore fmt in
  let c_label l fmt = Format.fprintf fmt "%s:@\n" l in
  let c_goto l fmt = Format.fprintf fmt "g %s@\n" l in
  let c_prover ~time fmt =
    List.iter
      (fun p ->
         Format.fprintf fmt "c %s %d %d@\n" (Prover.why3_desc p) time mem
      ) provers in
  let c_tactic ~goto fmt =
    List.iter
      (fun t ->
         Format.fprintf fmt "t %s %s@\n" (Tactic.name t) goto
      ) tactics in
  let c_list pps fmt = List.iter (fun pp -> pp fmt) pps in
  let c_strategy fmt ~name ~descr ~shortcut pps =
    Format.fprintf fmt
      "[strategy]@\n\
       code = \"%t\"@\n\
       name = \"%s\"@\n\
       desc = \"%s\"@\n\
       shortcut = \"%c\"@\n@."
      (c_list pps) name descr shortcut
  in
  Utils.mkdirs @@ Filename.dirname file ;
  Utils.formatfile ~file
    begin fun fmt ->
      c_strategy fmt ~name:"hammer" ~shortcut:'H'
        ~descr:"Hammer (why3find provers & tactics)" [
        c_label "root" ;
        c_prover ~time ;
        c_tactic ~goto:"root" ;
        if time > 1 then c_prover ~time:(time * 2) else c_skip ;
      ] ;
      c_strategy fmt ~name:"provers" ~shortcut:'P'
        ~descr:"Hammer Prove (why3find provers)" [
        c_prover ~time ;
      ] ;
      c_strategy fmt ~name:"tactics" ~shortcut:'U'
        ~descr:"Hammer Unfold (why3find tactics)" [
        c_label "tactic" ;
        c_tactic ~goto:"prover" ;
        c_goto "final" ;
        c_label "prover" ;
        c_prover ~time ;
        c_goto "tactic" ;
        c_label "final" ;
        if time > 1 then c_prover ~time:(2 * time) else c_skip ;
      ] ;
    end

(* -------------------------------------------------------------------------- *)
