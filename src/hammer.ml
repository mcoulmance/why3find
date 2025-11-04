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
  parent : parent ;
  hammer : hammer ;
  result : Crc.crc Fibers.t ;
  cancel : unit Fibers.signal option ;
  mutable state : state ;
  mutable window : Vc.window option ;
}

and state =
  | Paused            (* in q_paused *)
  | Scheduled         (* in q_proven or q_broken *)
  | Failed of Crc.crc (* in q_failed *)
  | Closed of Crc.crc (* not in any queue *)
  | Stacked of Tactic.tactic * node list (* children still queued *)

and parent =
  | Thread of int
  | Node of node

let result n = n.result
let goal n = n.goal
let state n = n.state
let parent n = n.parent
let rec thread_id n =
  match n.parent with Thread id -> id | Node n -> thread_id n

let rec proof n =
  match n.state with
  | Paused | Scheduled -> Crc.stuck ()
  | Failed crc | Closed crc -> crc
  | Stacked(tactic,children) -> Crc.tactic tactic (List.map proof children)

(* -------------------------------------------------------------------------- *)
(* --- Node Window                                                        --- *)
(* -------------------------------------------------------------------------- *)

let window n =
  match n.window with Some w -> w | None ->
    let file = Session.goal_file n.goal in
    let thy = Session.goal_theory n.goal in
    let task = Session.goal_task n.goal in
    let window = Vc.window ~file thy task in
    n.window <- Some window ; window

(* -------------------------------------------------------------------------- *)
(* --- Queue Management                                                   --- *)
(* -------------------------------------------------------------------------- *)

let failed = Fibers.signal ()

let p_proven = ref 0
let p_broken = ref 0
let q_failed = Fibers.Queue.create () (* Failed nodes *)
let q_paused = Fibers.Queue.create () (* Paused nodes *)
let q_broken = Queue.create () (* Stuck hints, scheduled first *)
let q_proven = Queue.create () (* Complete hints, scheduled last *)
let q_dispatch ~stepping n =
  Queue.push n
    (if not stepping && Crc.is_complete n.hint then q_proven else q_broken)
let q_pause n = n.state <- Paused ; Fibers.Queue.push q_paused n
let q_fail n crc =
  n.state <- Failed crc ; Fibers.emit failed n ; Fibers.Queue.push q_failed n
let q_schedule ~stepping n = n.state <- Scheduled ; q_dispatch ~stepping n
let q_resume n = if n.state = Paused then q_schedule ~stepping:true n

let iter f =
  begin
    Fibers.Queue.iter q_failed f ;
    Fibers.Queue.iter q_paused f ;
    Queue.iter f q_broken ;
    Queue.iter f q_proven ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Stepping                                                           --- *)
(* -------------------------------------------------------------------------- *)

type runmode = Running | Interrupting | Pausing | StepIn | StepOut
let runmode = ref Running

let subgoal n =
  match !runmode with
  | Interrupting -> ()
  | Running -> q_schedule ~stepping:false n
  | StepOut -> q_schedule ~stepping:true n
  | Pausing | StepIn -> q_pause n

let close n crc =
  match Fibers.get n.result with
  | Some _ -> ()
  | None -> n.state <- Closed crc ; Fibers.set n.result crc

let finalize n crc =
  match !runmode with
  | Running -> close n crc
  | Interrupting | Pausing ->
    if Crc.is_complete crc then close n crc else q_pause n
  | StepIn | StepOut ->
    if Crc.is_complete crc then close n crc else q_fail n crc

let stuck n = Crc.stuck ~goal:n.goal ()

let rec terminate n =
  match n.state with
  | Closed _ -> ()
  | Failed crc -> close n crc
  | Stacked(_,ns) -> List.iter terminate ns
  | Paused | Scheduled -> close n (stuck n)

let schedule n =
  match n.state with
  | Paused -> q_resume n
  | Failed crc -> close n crc
  | Scheduled | Stacked _ | Closed _ -> ()

let step ns =
  begin
    List.iter schedule ns ;
    Fibers.Queue.filter q_paused (fun n -> n.state <> Scheduled) ;
    Fibers.Queue.filter q_failed (fun n -> n.state <> Scheduled) ;
  end

let step_in ns = runmode := StepIn ; step ns
let step_out ns = runmode := StepOut ; step ns

let resume () =
  begin
    runmode := StepOut ;
    Fibers.Queue.iter q_paused schedule ;
    Fibers.Queue.clear q_paused ;
  end

let resume_to_end () =
  begin
    runmode := Running ;
    Fibers.Queue.iter q_paused schedule ;
    Fibers.Queue.clear q_paused ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Interrupt All                                                      --- *)
(* -------------------------------------------------------------------------- *)

let killall = Fibers.signal ()
let pause () =
  begin
    runmode := Pausing ;
    Queue.iter q_pause q_proven ;
    Queue.iter q_pause q_broken ;
    Queue.clear q_proven ;
    Queue.clear q_broken ;
    Fibers.emit killall () ;
  end

let interrupt () =
  begin
    runmode := Interrupting ;
    iter terminate ;
    Queue.clear q_proven ;
    Queue.clear q_broken ;
    Fibers.Queue.clear q_paused ;
    Fibers.Queue.clear q_failed ;
    Fibers.emit killall () ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Scheduling Nodes                                                   --- *)
(* -------------------------------------------------------------------------- *)

let node hammer ~parent ?cancel ?hint goal =
  let hint = match hint with Some h -> h | None -> Crc.stuck ~goal () in
  let result = Fibers.var () in
  let depth = match parent with Thread _ -> 0 | Node n -> succ n.depth in
  {
    hammer ; goal ;
    parent ; depth ;
    hint ; cancel ; result ;
    state = Paused ;
    window = None ;
  }

let push hammer ~parent ?cancel goal hint =
  let node = node hammer ~parent ?cancel ~hint goal in
  subgoal node ; node

let rec push_all hammer ~parent ?cancel goals hints =
  match goals, hints with
  | [], _ -> []
  | g::gs, [] ->
    let s = Crc.stuck () in
    push hammer ~parent ?cancel g s :: push_all hammer ~parent ?cancel gs []
  | g::gs, h::hs ->
    push hammer ~parent ?cancel g h :: push_all hammer ~parent ?cancel gs hs

let pop () =
  try Some (Queue.pop q_broken) with Queue.Empty ->
  try Some (Queue.pop q_proven) with Queue.Empty ->
    None

type result = Crc.crc Fibers.t
type strategy = node -> Crc.crc Fibers.t
let fail : strategy = fun n -> Fibers.return (stuck n)

let (@>) (h : strategy) (n : node) : result =
  match !runmode with
  | Interrupting -> Fibers.return @@ Crc.stuck ()
  | _ -> try h n with Not_found -> fail n

let (>>>) (h1 : strategy) (h2 : strategy) : strategy = fun n ->
  let* r = h1 @> n in
  if not @@ Crc.is_stuck r then Fibers.return r else h2 @> n

let rec sequence (f : 'a -> strategy) (xs : 'a list) : strategy =
  match xs with
  | [] -> fail
  | x::xs -> f x >>> sequence f xs

let decompose node tactic gs hs =
  let parent = Node node in
  let children = push_all node.hammer ~parent ?cancel:node.cancel gs hs in
  node.state <- Stacked(tactic,children) ;
  let callback crc = node.state <- Closed crc in
  Fibers.finally ~callback @@
  Crc.tactic ~goal:node.goal tactic @+
  Fibers.all @@ List.map result children

(* -------------------------------------------------------------------------- *)
(* --- Try Prover on Node                                                 --- *)
(* -------------------------------------------------------------------------- *)

let prove ?cancel timeout prover : strategy = fun n ->
  let h = n.hammer in
  let wmain = Why3.Whyconf.get_main h.env.why3.config in
  let name = Session.goal_name n.goal in
  let* alpha = Calibration.velocity wmain h.env.cache h.profile prover in
  let to_local = Result.map (fun t -> t *. alpha) in
  let to_profile = Result.map (fun t -> t /. alpha) in
  let runner_time = timeout *. alpha in
  let callback = Session.result n.goal in
  let task = Session.goal_task n.goal in
  let+ verdict =
    match Runner.prove_cached h.env.cache prover task runner_time with
    | Some result ->
      Runner.notify wmain prover result callback ;
      Fibers.return (to_profile result)
    | None ->
      let ptask = Runner.prepare prover task in
      let kill = Fibers.signal () in
      let handler = Fibers.emit kill in
      let runner =
        Fibers.monitor ?signal:cancel ~handler @@
        Fibers.monitor ?signal:n.cancel ~handler @@
        Fibers.monitor ~signal:killall ~handler @@
        Fibers.map to_profile @@
        Fibers.finally ~callback:(Cache.set h.env.cache task prover) @@
        Runner.prove_prepared wmain ~name ~cancel:kill ~callback
          prover ptask runner_time ~idename:(Session.goal_idename n.goal)
      in match h.client with
      | None -> runner
      | Some cl ->
        (* Don't ask server for too low timeouts *)
        if timeout < 0.2 *. h.env.config.time then runner else
          let signal = Client.request cl h.profile prover ptask timeout in
          let handler result =
            match Result.crop ~timeout result with
            | Some result ->
              let local = to_local result in
              (* TODO: cache this not here but in the prooftree cache *)
              (* Cache.set h.cache task prover local ; *)
              Runner.notify wmain prover local callback ;
              Fibers.set runner result ;
              Fibers.emit kill ()
            | None -> ()
          in Fibers.monitor ~signal ~handler runner
  in match verdict with
  | Valid t when t <= timeout ->
    Crc.prover ~goal:n.goal prover.Prover.desc (Utils.round t)
  | _ -> stuck n

(* -------------------------------------------------------------------------- *)
(* --- Tactic Hint                                                        --- *)
(* -------------------------------------------------------------------------- *)

type hint = Tactic.tactic * Crc.crc list
let pp_hint fmt (h : hint) = Tactic.pretty fmt (fst h)

(* -------------------------------------------------------------------------- *)
(* --- Try Transformation on Node                                         --- *)
(* -------------------------------------------------------------------------- *)

let apply tac ?(hint : hint option) (n : node) : Crc.crc option Fibers.t =
  let env = n.hammer.env in
  if n.depth > env.config.depth then Fibers.return @@ Some (stuck n) else
    match Session.apply env.why3.env tac n.goal with
    | None -> Fibers.return None
    | Some gs ->
      let hs =
        match hint with
        | Some(t,hs) when Tactic.equal t tac -> hs
        | _ -> [] in
      Option.some @+ decompose n tac gs hs

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

let thread hammer ?(id=(-1)) ?cancel goal hint =
  let parent = Thread id in
  match hammer.preprocess with
  | None -> push hammer ~parent ?cancel goal hint
  | Some tactic ->
    match
      Session.apply hammer.env.why3.env tactic ~allow_no_effect:true goal
    with
    | None ->
      Log.warning "Ignored goal %s: preprocessing tactic %a failed"
        (Session.goal_name goal) Tactic.pretty tactic ;
      push hammer ~parent ?cancel goal hint
    | Some goals ->
      let hints = match hint.status with
        | Tactic { id; children; _ } when id = Tactic.name tactic -> children
        | _ -> [] in
      let root = node hammer ~parent ?cancel ~hint goal in
      Fibers.background ~callback:(Fibers.set root.result) @@
      decompose root tactic goals hints ; root

(* -------------------------------------------------------------------------- *)
(* --- Hammer Strategy                                                    --- *)
(* -------------------------------------------------------------------------- *)

let hammer0 : strategy = fun n ->
  let h = n.hammer in
  let t = h.env.config.fast in
  if h.level > 0 && t > 0.0 && List.length h.provers > 1
  then sequence (prove t) h.provers n else fail n

let hammer13 minlevel timefactor n : result =
  let h = n.hammer in
  if h.level <= minlevel then fail n else
    let cancel = Fibers.signal () in
    let watch r = if not @@ Crc.is_stuck r then Fibers.emit cancel () ; r in
    let time = timefactor *. h.env.config.time in
    let+ results =
      Fibers.all @@ List.map
        (fun prv -> watch @+ prove ~cancel time prv n)
        n.hammer.provers in
    try List.find (fun r -> not @@ Crc.is_stuck r) results
    with Not_found -> stuck n

let hammer1 : strategy = hammer13 1 1.0
let hammer3 : strategy = hammer13 3 2.0

let hammer2 ?hint : strategy = fun n ->
  let rec aux = function
    | [] -> fail n
    | t :: ts ->
      let* o = apply t ?hint n in
      match o with
      | Some r -> Fibers.return r
      | None -> aux ts in
  if n.hammer.level > 2
  then aux n.hammer.tactics
  else fail n

let hammer =
  hammer0 >>> (* level -H 1 *)
  hammer1 >>> (* level -H 2 *)
  hammer2 >>> (* level -H 3 *)
  hammer3     (* level -H 4 *)

(* -------------------------------------------------------------------------- *)
(* --- Node Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let replay_prover d t : strategy = fun n ->
  let env = n.hammer.env in
  match Prover.check_and_get_prover env.why3 ~patterns:env.config.provers d with
  | None -> fail n
  | Some p ->
    let base = env.config.time in
    (* Takes into account the current strategy and possible fluctuations *)
    let time = min (max base (t +. 0.5)) (2.0 *. base) in
    prove time p n

let update_prover p t = replay_prover p t >>> hammer

let replay_tactic ~hint n =
  let rec iter = function
    | [] ->
      Log.warning "Goal %S: tactic '%a' not in strategy"
        (Session.goal_name n.goal) pp_hint hint ;
      fail n
    | t::ts ->
      if Tactic.equal t (fst hint) then
        Option.value ~default:(stuck n) @+ apply t ~hint n
      else
        match Session.apply n.hammer.env.why3.env t n.goal with
        | None -> iter ts
        | Some _ ->
          Log.warning "Goal %S: tactic '%a' shadowed by '%a'"
            (Session.goal_name n.goal) pp_hint hint Tactic.pretty t ;
          fail n
  in iter n.hammer.tactics

let reduce_tactic ?hint n = (hammer1 >>> hammer2 ?hint >>> hammer3) n
let update_tactic ~hint n = (hammer2 ~hint >>> hammer) n

let explore n = if n.hammer.replay then fail n else hammer n

let execute n =
  try
    match n.hint.status with
    | Stuck -> explore n
    | Prover(p,t) ->
      if n.hammer.replay then
        replay_prover p t n
      else
        update_prover p t n
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
    fail n

(* -------------------------------------------------------------------------- *)
(* --- Queue Status                                                       --- *)
(* -------------------------------------------------------------------------- *)

type progress = {
  broken  : int ;
  proven  : int ;
  pending : int ;
  running : int ;
}

type status =
  | Proving of progress
  | Paused
  | Exited

let progress () = {
  proven = Queue.length q_proven + !p_proven ;
  broken = Queue.length q_broken + !p_broken ;
  pending = Runner.pending () ;
  running = Runner.running() ;
}

let pp_progress fmt s =
  if s.proven > 0 then Format.fprintf fmt "!%d/" s.proven ;
  if s.broken > 0 then Format.fprintf fmt "?%d/" s.broken ;
  Format.fprintf fmt "%d/%d" s.pending s.running

let pp_status fmt = function
  | Proving s -> pp_progress fmt s
  | Paused -> Format.pp_print_string fmt "paused"
  | Exited -> Format.pp_print_string fmt "exited"

let proving () =
  begin
    false
    || Queue.length q_proven > 0
    || Queue.length q_broken > 0
    || Runner.pending () > 0
    || Runner.running () > 0
  end

let paused () =
  false
  || Fibers.Queue.length q_paused > 0
  || Fibers.Queue.length q_failed > 0

let status () =
  let progress () = Proving (progress ()) in
  if proving () then progress ()
  else if paused () then Paused
  else if Fibers.pending () > 0 then progress ()
  else Exited

let running () = proving () || Fibers.pending () > 0
let interrupting () = !runmode = Interrupting

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
      let callback r = decr pr; finalize node r in
      incr pr ; Fibers.background (execute node) ~callback ; true
    | None -> false
  end

let run () =
  runmode := Running ;
  while running () do
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
