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
(* --- Hammer Proof Strategy                                              --- *)
(* -------------------------------------------------------------------------- *)

open Crc
open Fibers.Monad

type henv = {
  env : Config.env ;
  fast : float ;
  time : float ;
  client : Client.client option ;
  level : int ;
  maxdepth : int ;
  patterns : string list ;
  provers : Prover.prover list ;
  tactics : Tactic.tactic list ;
  minimize : bool ;
}

type node = {
  depth : int ;
  profile : Calibration.profile ;
  goal : Session.goal ;
  hint : crc ;
  replay : bool ;
  result : crc Fibers.t ;
}

(* -------------------------------------------------------------------------- *)
(* --- Queue Management                                                   --- *)
(* -------------------------------------------------------------------------- *)

let q1 : node Queue.t = Queue.create ()
let q2 : node Queue.t = Queue.create ()

(* idename should be the goal name as in Why3 IDE *)
let schedule profile ~replay ~depth goal hint =
  let result = Fibers.var () in
  Queue.push
    { profile ; goal ; hint ; replay ; depth ; result }
    (if is_complete hint then q2 else q1) ;
  result

let pop () =
  try Some (Queue.pop q1) with Queue.Empty ->
  try Some (Queue.pop q2) with Queue.Empty ->
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
(* --- Try Prover on Node                                                 --- *)
(* -------------------------------------------------------------------------- *)

let prove env ?client ?cancel prover timeout : strategy = fun n ->
  let wmain = Why3.Whyconf.get_main env.Config.wenv.config in
  let name = Session.goal_name n.goal in
  let* alpha = Calibration.velocity wmain n.profile prover in
  let to_profile = Runner.map (fun t -> t /. alpha) in
  let runner_time = timeout *. alpha in
  let callback = Session.result n.goal in
  let+ verdict =
    match Runner.prove_cached prover n.goal runner_time with
    | Some result ->
      Runner.notify wmain prover result callback ;
      Fibers.return (to_profile result)
    | None ->
      let task = Runner.prepare prover n.goal in
      let kill = Fibers.signal () in
      let runner =
        Fibers.monitor ?signal:cancel ~handler:(Fibers.emit kill) @@
        Fibers.map to_profile @@
        Fibers.finally ~callback:(Runner.update prover n.goal) @@
        Runner.prove_prepared wmain ~name ~cancel:kill ~callback
          prover task runner_time ~idename:(Session.goal_idename n.goal)
      in match client with
      | None -> runner
      | Some cl ->
        let signal = Client.request cl n.profile prover task timeout in
        let handler r =
          match Runner.crop ~timeout r with
          | Some r ->
            Fibers.set runner r ;
            Fibers.emit kill ()
          | None -> ()
        in Fibers.monitor ~signal ~handler runner
  in match verdict with
  | Valid t -> Crc.prover ~goal:n.goal prover.Prover.desc (Utils.round t)
  | _ -> Crc.stuck ~goal:n.goal ()


(* -------------------------------------------------------------------------- *)
(* --- Tactic Hint                                                        --- *)
(* -------------------------------------------------------------------------- *)

type hint = Tactic.tactic * Crc.crc list
let pp_hint fmt (h : hint) = Tactic.pretty fmt (fst h)

(* -------------------------------------------------------------------------- *)
(* --- Try Transformation on Node                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec subgoals ({ profile ; replay ; depth } as n) goals hints =
  match goals, hints with
  | [], _ -> []
  | g::gs, [] ->
    let h = Crc.stuck () in
    schedule profile ~replay ~depth:(succ depth) g h :: subgoals n gs []
  | g::gs, h::hs ->
    schedule profile ~replay ~depth:(succ depth) g h :: subgoals n gs hs

let apply h tac ?(hint : hint option) : node -> crc option Fibers.t = fun n ->
  if n.depth > h.maxdepth then Fibers.return @@ Some (stuck n) else
    match Session.apply h.env.Config.wenv.env tac n.goal with
    | None -> Fibers.return None
    | Some gs ->
      let hs =
        match hint with
        | Some(t,hs) when Tactic.equal t tac -> hs
        | _ -> []
      in Option.some @+ Crc.tactic ~goal:n.goal tac @+ Fibers.all @@ subgoals n gs hs

(* -------------------------------------------------------------------------- *)
(* --- Hammer Strategy                                                    --- *)
(* -------------------------------------------------------------------------- *)

let hammer0 h : strategy =
  if h.level > 0 && h.fast > 0.0 && List.length h.provers > 1
  then sequence (fun prv -> prove h.env prv h.fast) h.provers
  else fail

let hammer13 h time : strategy = fun n ->
  let cancel = Fibers.signal () in
  let watch r = if not @@ Crc.is_stuck r then Fibers.emit cancel () ; r in
  let+ results =
    Fibers.all @@ List.map
      (fun prv -> watch @+ prove h.env ?client:h.client ~cancel prv time n)
      h.provers in
  try List.find (fun r -> not @@ Crc.is_stuck r) results
  with Not_found -> Crc.stuck ~goal:n.goal ()

let hammer1 h = if h.level > 1 then hammer13 h h.time else fail
let hammer3 h = if h.level > 3 then hammer13 h (2.0 *. h.time) else fail

let hammer2 ?hint h : strategy = fun n ->
  let rec aux = function
    | [] -> fail n
    | t :: ts ->
      let* o = apply h t ?hint n in
      match o with
      | Some r -> Fibers.return r
      | None -> aux ts in
  if h.level > 2
  then aux h.tactics
  else fail n

let hammer henv =
  hammer0 henv >>> (* level -H 1 *)
  hammer1 henv >>> (* level -H 2 *)
  hammer2 henv >>> (* level -H 3 *)
  hammer3 henv     (* level -H 4 *)

(* -------------------------------------------------------------------------- *)
(* --- Node Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let replay_prover h d t =
  match Prover.check_and_get_prover h.env.wenv ~patterns:h.patterns d with
  | None -> fail
  | Some p ->
    (* Do not ask the server for too short hint times *)
    let client = if t > h.time *. 0.2 then h.client else None in
    (* Taking into account the current strategy *)
    let time = min (max h.time t) (2.0 *. h.time) in
    (* Applying an overhead against possible fluctuations *)
    prove h.env ?client p (time +. 0.5)

let update_prover h p t = replay_prover h p t >>> hammer h

let replay_tactic ~hint h n =
  let rec iter = function
    | [] ->
      Log.warning "Goal %S: tactic '%a' not in strategy"
        (Session.goal_name n.goal) pp_hint hint ;
      fail n
    | t::ts ->
      if Tactic.equal t (fst hint) then
        Option.value ~default:(stuck n) @+ apply h t ~hint n
      else
        match Session.apply h.env.Config.wenv.env t n.goal with
        | None -> iter ts
        | Some _ ->
          Log.warning "Goal %S: tactic '%a' shadowed by '%a'"
            (Session.goal_name n.goal) pp_hint hint Tactic.pretty t ;
          fail n
  in iter h.tactics

let reduce_tactic ?hint h =
  hammer1 h >>>
  hammer2 ?hint h >>>
  hammer3 h

let update_tactic ~hint h =
  hammer2 ~hint h >>> hammer h

let explore h n =
  if n.replay then fail n else hammer h n

let process h : strategy = fun n ->
  try
    match n.hint.status with
    | Stuck -> explore h n
    | Prover(p,t) ->
      if n.replay then
        replay_prover h p t n
      else
        update_prover h p t n
    | Tactic { id ; children } ->
      match Tactic.lookup h.env.wenv.env id with
      | Some tac ->
        let hint = tac, children in
        if n.replay then
          replay_tactic ~hint h n
        else
        if h.minimize then
          reduce_tactic ~hint h n
        else
          update_tactic ~hint h n
      | None ->
        explore h n
  with exn ->
    Log.emit "Process Error (%s)" @@ Printexc.to_string exn ;
    fail n

(* -------------------------------------------------------------------------- *)
(* --- Main Loop                                                          --- *)
(* -------------------------------------------------------------------------- *)

let run henv =
  let exception Break in
  try
    let p2 = ref 0 in
    let p1 = ref 0 in
    while true do
      Fibers.yield () ;
      Option.iter Client.yield henv.client ;
      let n2 = Queue.length q2 + !p2 in
      let n1 = Queue.length q1 + !p1 in
      let nq = Runner.pending () in
      let nr = Runner.running () in
      let total = n2 + n1 + nq + nr in
      Utils.progress "%t%d/%d%t"
        (fun fmt ->
           if n2 > 0 then Format.fprintf fmt "!%d/" n2 ;
           if n1 > 0 then Format.fprintf fmt "?%d/" n1 ;
        ) nq nr Runner.pp_goals ;
      match pop () with
      | Some node ->
        Fibers.background @@
        begin
          let pr = if is_complete node.hint then p2 else p1 in
          incr pr ;
          let* r = process henv node in
          decr pr ;
          Fibers.set node.result r ;
          Fibers.return ()
        end
      | None ->
        if total > 0
        then Unix.sleepf 0.01
        else raise Break
    done
  with Break ->
    Option.iter Client.terminate henv.client

(* -------------------------------------------------------------------------- *)
(* --- Why3 Hammer Config                                                 --- *)
(* -------------------------------------------------------------------------- *)

let session = ".why3find"
let hammer = ".why3find/hammer.cfg"

let config ~tactics ~provers ~time ~mem =
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
  begin
    Utils.mkdirs session ;
    Utils.formatfile ~file:hammer
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
      end ;
    hammer
  end

(* -------------------------------------------------------------------------- *)
