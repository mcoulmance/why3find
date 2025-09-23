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

open Crc
open Fibers.Monad

type hammer = {
  level : int ;
  replay : bool ;
  minimize : bool ;
  cache : Cache.t ;
  config : Config.config ;
  why3 : Config.why3env ;
  client : Client.client option ;
  preprocess : Tactic.tactic option ;
  provers : Prover.prover list ;
  tactics : Tactic.tactic list ;
  profile : Calibration.profile ;
}

type node = {
  goal : Session.goal ;
  hint : crc ;
  depth : int ;
  hammer : hammer ;
  cancel : unit Fibers.signal option ;
  result : crc Fibers.t ;
}

(* -------------------------------------------------------------------------- *)
(* --- Queue Management                                                   --- *)
(* -------------------------------------------------------------------------- *)

let q_broken = Queue.create () (* Stuck hints, scheduled first *)
let q_proven = Queue.create () (* Complete hints, scheduled last *)

(* idename should be the goal name as in Why3 IDE *)
let push hammer ~depth ~cancel goal hint =
  let result = Fibers.var () in
  Queue.push
    { goal ; hint ; depth ; cancel ; hammer ; result }
    (if is_complete hint then q_proven else q_broken) ;
  result

let rec push_all hammer ~depth ~cancel goals hints =
  match goals, hints with
  | [], _ -> []
  | g::gs, [] ->
    let s = Crc.stuck () in
    push hammer ~depth ~cancel g s :: push_all hammer ~depth ~cancel gs []
  | g::gs, h::hs ->
    push hammer ~depth ~cancel g h :: push_all hammer ~depth ~cancel gs hs

let pop () =
  try Some (Queue.pop q_broken) with Queue.Empty ->
  try Some (Queue.pop q_proven) with Queue.Empty ->
    None

let stuck n = stuck ~goal:n.goal ()

type strategy = node -> crc Fibers.t
let fail : strategy = fun n -> Fibers.return (stuck n)

let (@>) (h : strategy) (n : node) : crc Fibers.t =
  try h n with Not_found -> fail n

let (>>>) (h1 : strategy) (h2 : strategy) : strategy = fun n ->
  let* r = h1 @> n in
  if not @@ is_stuck r then Fibers.return r else h2 @> n

let rec sequence (f : 'a -> strategy) (xs : 'a list) : strategy =
  match xs with
  | [] -> fail
  | x::xs -> f x >>> sequence f xs

(* -------------------------------------------------------------------------- *)
(* --- Interrupt All                                                      --- *)
(* -------------------------------------------------------------------------- *)

let killall = Fibers.signal ()

let interrupt () =
  begin
    Fibers.emit killall () ;
    let killed = Crc.stuck () (* anonymous CRC *) in
    let dequeue n = Fibers.set n.result killed in
    Queue.iter dequeue q_proven ;
    Queue.iter dequeue q_broken ;
    Queue.clear q_proven ;
    Queue.clear q_broken ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Try Prover on Node                                                 --- *)
(* -------------------------------------------------------------------------- *)

let prove ?cancel timeout prover : strategy = fun n ->
  let h = n.hammer in
  let wmain = Why3.Whyconf.get_main h.why3.config in
  let name = Session.goal_name n.goal in
  let* alpha = Calibration.velocity wmain h.cache h.profile prover in
  let to_local = Result.map (fun t -> t *. alpha) in
  let to_profile = Result.map (fun t -> t /. alpha) in
  let runner_time = timeout *. alpha in
  let callback = Session.result n.goal in
  let task = Session.goal_task n.goal in
  let+ verdict =
    match Runner.prove_cached h.cache prover task runner_time with
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
        Fibers.finally ~callback:(Cache.set h.cache task prover) @@
        Runner.prove_prepared wmain ~name ~cancel:kill ~callback
          prover ptask runner_time ~idename:(Session.goal_idename n.goal)
      in match h.client with
      | None -> runner
      | Some cl ->
        (* Don't ask server for too low timeouts *)
        if timeout < 0.2 *. h.config.time then runner else
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
  | Valid t when t <= timeout -> Crc.prover ~goal:n.goal prover.Prover.desc (Utils.round t)
  | _ -> Crc.stuck ~goal:n.goal ()

(* -------------------------------------------------------------------------- *)
(* --- Tactic Hint                                                        --- *)
(* -------------------------------------------------------------------------- *)

type hint = Tactic.tactic * Crc.crc list
let pp_hint fmt (h : hint) = Tactic.pretty fmt (fst h)

(* -------------------------------------------------------------------------- *)
(* --- Try Transformation on Node                                         --- *)
(* -------------------------------------------------------------------------- *)

let apply tac ?(hint : hint option) : node -> crc option Fibers.t = fun n ->
  if n.depth > n.hammer.config.depth then Fibers.return @@ Some (stuck n) else
    match Session.apply n.hammer.why3.env tac n.goal with
    | None -> Fibers.return None
    | Some gs ->
      let hs =
        match hint with
        | Some(t,hs) when Tactic.equal t tac -> hs
        | _ -> [] in
      Option.some @+
      Crc.tactic ~goal:n.goal tac @+
      Fibers.all @@ push_all n.hammer ~depth:(succ n.depth) ~cancel:n.cancel gs hs

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
    level ; replay ; minimize ;
    why3 = env.Project.why3 ;
    cache = env.cache ;
    config = env.config ;
    client = Client.connect env ;
    provers ; tactics ; profile ; preprocess ;
  }

let schedule hammer ?cancel goal hint =
  match hammer.preprocess with
  | None -> push hammer ~depth:0 ~cancel goal hint
  | Some tactic ->
    match Session.apply hammer.why3.env tactic goal with
    | None ->
      Log.warning "Ignored goal %s: preprocessing tactic %a failed"
        (Session.goal_name goal) Tactic.pretty tactic ;
      Fibers.return (Crc.stuck ~goal ())
    | Some goals ->
      let hints = match hint.status with
        | Tactic { id; children; _ } when id = Tactic.name tactic -> children
        | _ -> []
      in Crc.tactic ~goal tactic @+
         Fibers.all @@ push_all hammer ~depth:0 ~cancel goals hints

(* -------------------------------------------------------------------------- *)
(* --- Hammer Strategy                                                    --- *)
(* -------------------------------------------------------------------------- *)

let hammer0 : strategy = fun n ->
  let h = n.hammer in
  if h.level > 0 && h.config.fast > 0.0 && List.length h.provers > 1
  then sequence (prove h.config.fast) h.provers n else fail n

let hammer13 minlevel timefactor n : crc Fibers.t =
  if n.hammer.level <= minlevel then fail n else
    let cancel = Fibers.signal () in
    let watch r = if not @@ Crc.is_stuck r then Fibers.emit cancel () ; r in
    let time = timefactor *. n.hammer.config.time in
    let+ results =
      Fibers.all @@ List.map
        (fun prv -> watch @+ prove ~cancel time prv n)
        n.hammer.provers in
    try List.find (fun r -> not @@ Crc.is_stuck r) results
    with Not_found -> Crc.stuck ~goal:n.goal ()

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
  let h = n.hammer in
  match Prover.check_and_get_prover h.why3 ~patterns:h.config.provers d with
  | None -> fail n
  | Some p ->
    let base = h.config.time in
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
        match Session.apply n.hammer.why3.env t n.goal with
        | None -> iter ts
        | Some _ ->
          Log.warning "Goal %S: tactic '%a' shadowed by '%a'"
            (Session.goal_name n.goal) pp_hint hint Tactic.pretty t ;
          fail n
  in iter n.hammer.tactics

let reduce_tactic ?hint n = (hammer1 >>> hammer2 ?hint >>> hammer3) n
let update_tactic ~hint n = (hammer2 ~hint >>> hammer) n

let explore n = if n.hammer.replay then fail n else hammer n

let process_node n =
  try
    match n.hint.status with
    | Stuck -> explore n
    | Prover(p,t) ->
      if n.hammer.replay then
        replay_prover p t n
      else
        update_prover p t n
    | Tactic { id ; children } ->
      match Tactic.lookup n.hammer.why3.env id with
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

(* Number of currently processed tasks from their respective queues *)
let p_proven = ref 0
let p_broken = ref 0

type status = {
  broken  : int ;
  proven  : int ;
  pending : int ;
  running : int ;
}

let status () = {
  proven = Queue.length q_proven + !p_proven ;
  broken = Queue.length q_broken + !p_broken ;
  pending = Runner.pending () ;
  running = Runner.running() ;
}

let running () =
  !p_proven > 0 ||
  !p_broken > 0 ||
  Queue.length q_proven > 0 ||
  Queue.length q_broken > 0 ||
  Runner.pending () > 0 ||
  Runner.running () > 0 ||
  Fibers.pending () > 0

let pp_status fmt s =
  if s.proven > 0 then Format.fprintf fmt "!%d/" s.proven ;
  if s.broken > 0 then Format.fprintf fmt "?%d/" s.broken ;
  Format.fprintf fmt "%d/%d" s.pending s.running

(* -------------------------------------------------------------------------- *)
(* --- Task Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let process () =
  Client.yield () ;
  Fibers.yield () ;
  match pop () with
  | Some node ->
    let pr = if is_complete node.hint then p_proven else p_broken in
    incr pr;
    Fibers.background (process_node node)
      ~callback:(fun r -> decr pr; Fibers.set node.result r);
    true
  | None -> false

let run () =
  while running () do
    let s = status () in
    Utils.progress "%a%t" pp_status s Runner.pp_goals ;
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
