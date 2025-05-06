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
(* --- Why3 Runner                                                        --- *)
(* -------------------------------------------------------------------------- *)

module Task = Why3.Task
module Driver = Why3.Driver
module Whyconf = Why3.Whyconf
module Call_provers = Why3.Call_provers

type callback =
  Why3.Whyconf.prover ->
  Why3.Call_provers.resource_limits ->
  Why3.Call_provers.prover_result ->
  unit

(* -------------------------------------------------------------------------- *)
(* --- Proof Tasks                                                        --- *)
(* -------------------------------------------------------------------------- *)

type prooftask = {
  task : Task.task;
  buffer : Buffer.t;
}

let prepare_task =
  Timer.timed2 ~name:"Task preparation" Driver.prepare_task

let print_task_prepared =
  Timer.timed3 ~name:"Task printing" Driver.print_task_prepared

let prepare prover task =
  let task = prepare_task prover.Prover.driver task in
  let buffer = Buffer.create 2048 in
  let fmt = Format.formatter_of_buffer buffer in
  ignore @@ print_task_prepared prover.Prover.driver fmt task ;
  Format.pp_print_flush fmt () ;
  { task ; buffer }

let digest task = Cache.digest task.task

let data task = Buffer.contents task.buffer

(* -------------------------------------------------------------------------- *)
(* --- Running Prover                                                     --- *)
(* -------------------------------------------------------------------------- *)

let jobs = ref 0
let clients = ref 0
let pending = ref 0
let running = ref 0
let goals = ref []

let rec ginc a = function
  | [] -> [a,1]
  | ((a0,n) as hd)::tl ->
    if a = a0 then (a0,n+1)::tl else hd::ginc a tl

let rec gdec a = function
  | [] -> []
  | ((a0,n) as hd)::tl ->
    if a = a0 then
      if n > 1 then (a0,n-1)::tl else tl
    else hd :: gdec a tl

let schedule () = incr pending
let unschedule () = decr pending

let start ?name () =
  decr pending ;
  incr running ;
  match name with
  | None -> ()
  | Some a -> goals := ginc a !goals

let stop ?name () =
  decr running ;
  match name with
  | None -> ()
  | Some a -> goals := gdec a !goals

let pending () = !pending
let running () = !running

let pp_goals fmt =
  List.iter
    (fun (a,n) ->
       if n > 1 then
         Format.fprintf fmt " %s(%d)" a n
       else
         Format.fprintf fmt " %s" a
    ) !goals

let set_jobs n = jobs := n
let get_jobs wmain =
  if !jobs > 0 then !jobs else Whyconf.running_provers_max wmain

let update_client wmain =
  let njobs = get_jobs wmain in
  if njobs <> !clients then
    begin
      clients := njobs ;
      Why3.Prove_client.set_max_running_provers njobs ;
    end

let memlimit wmain = Whyconf.memlimit wmain

let limits wmain t =
  Why3.Call_provers.{
    limit_time = t ;
    limit_mem = memlimit wmain ;
    limit_steps = (-1) ;
  }

let notify wmain prover (result: Result.t) (cb: callback) =
  let fire cb p l r = cb p.Prover.config.prover l r in
  let pr ~s ?(t = 0.0) ans =
    Why3.Call_provers.{
      pr_answer = ans ;
      pr_status = Unix.WEXITED s ;
      pr_time = t ;
      pr_steps = 0 ;
      pr_output = "Cached" ;
      pr_models = [] ;
    }
  in
  let limits = limits wmain in
  match result with
  | NoResult -> ()
  | Valid t -> fire cb prover (limits t) (pr ~s:0 ~t Valid)
  | Timeout t -> fire cb prover (limits t) (pr ~s:1 ~t Timeout)
  | Unknown t -> fire cb prover (limits t) (pr ~s:1 ~t (Unknown "why3find"))
  | Failed -> fire cb prover (limits 1.0) (pr ~s:2 (Failure "why3find"))

let notify_pr prv limits pr cb =
  cb prv.Prover.config.prover limits pr

let launch_proof_timer = Timer.create_global "Launch proof"

let logchannel = ref None

let set_logfile = function
  | "-" -> logchannel := Some stdout
  | s ->
    let oc = open_out s in
    at_exit (fun () -> close_out_noerr oc);
    logchannel := Some oc

module type WRunner = sig

  type prover_call

  val prove_task : command:string -> config:Whyconf.main
    -> limits:Call_provers.resource_limits -> Driver.driver -> Buffer.t
    -> prover_call

  val interrupt_call : config:Whyconf.main -> prover_call -> unit

  val query_call : prover_call -> Call_provers.prover_update

end

module WRunner : WRunner = struct

  type prover_call = Call_provers.prover_call

  let prove_task ~command ~config ~limits driver buffer =
    Driver.prove_buffer_prepared ~command ~config ~limits driver buffer

  let interrupt_call ~config c =
    Call_provers.interrupt_call ~config c

  let query_call c =
    Call_provers.query_call c

end

module MockRunner : WRunner = struct

  type state =
    | Unstarted
    | Started
    | Interrupted
    | Done

  type prover_call = {
    mutable state : state;
  }

  let prove_task ~command:_ ~config:_ ~limits:_ _ _ =
    { state = Unstarted }

  let interrupt_call ~config:_ c =
    match c.state with
    | Started -> c.state <- Interrupted
    | _ -> assert false

  let interrupted = (* expected answer, see below *)
    let highfailure = Call_provers.{
        pr_answer = HighFailure "interrupt";
        pr_status = Unix.WEXITED 0 ;
        pr_time = 0. ;
        pr_steps = 0 ;
        pr_output = "" ;
        pr_models = [] ;
      } in
    Call_provers.ProverFinished highfailure

  let query_call c =
    match c.state with
    | Unstarted -> c.state <- Started; Call_provers.ProverStarted
    | Started -> Call_provers.NoUpdates
    | Interrupted -> c.state <- Done; interrupted
    | Done -> assert false

end

let wrunner = ref (module WRunner : WRunner)

let mock_runner () =
  wrunner := (module MockRunner)

let _ = Debug.create_flag ~callback:mock_runner "fake-local-timeouts"

let debug_print_task fmt buf =
  let f, oc = Filename.open_temp_file "why3find_" "" in
  Buffer.output_buffer oc buf;
  close_out oc;
  Format.pp_print_string fmt f

let call_prover (config : Why3.Whyconf.main)
    ?(name : string option)
    ?(idename : string option)
    ?(cancel : unit Fibers.signal option)
    ?(callback : callback option)
    ~(prepared : Buffer.t)
    ~(prover : Prover.prover)
    ~(timeout : float) () =
  let module WRunner = (val !wrunner) in
  let limits = limits config timeout in
  let timer = Timer.create () in
  let clockwall = ref 0.0 in
  let started = ref false in
  let killed = ref false in
  let runover = ref false in
  let canceled = ref false in
  let timedout = ref false in
  update_client config ;
  schedule () ;
  Timer.start launch_proof_timer;
  let call =
    WRunner.prove_task ~config ~limits prover.driver prepared
      ~command:(Whyconf.get_complete_command prover.config ~with_steps:false)
  in
  Timer.stop launch_proof_timer;
  let kill () =
    if not !killed then
      begin
        try
          WRunner.interrupt_call ~config call ;
          killed := true ;
        with _ -> ()
      end in
  let interrupt () =
    begin
      canceled := true ;
      if !started then kill () ;
    end in
  Fibers.monitor ?signal:cancel ~handler:interrupt @@
  Fibers.async @@
  begin fun () ->
    let now = Unix.gettimeofday () in
    if !started then
      begin
        if !canceled then kill ()
        else if not !timedout && now > !clockwall then
          (timedout := true ; kill ())
        else if !timedout && now > !clockwall +. 1.0 then
          runover := true
      end ;
    let query =
      try WRunner.query_call call with e -> InternalFailure e in
    match query with
    | NoUpdates | ProverInterrupted ->
      if !runover then
        begin
          stop ?name () ;
          Log.warning "Prover %a did not stop properly on goal %s.@ \
                       See the prover input at %a."
            Prover.pp_prover prover (Option.value ~default:"<unnamed>" name)
            debug_print_task prepared;
          Some Result.Failed
        end
      else
        None
    | ProverStarted ->
      if not !started then
        begin
          start ?name () ;
          Timer.start timer;
          clockwall := Unix.gettimeofday () +. timeout *. 1.5 ;
          started := true ;
        end ;
      None
    | InternalFailure e ->
      Log.warning "Why3 internal failure: %a" Why3.Exn_printer.exn_printer e;
      begin
        if !started then stop ?name () ;
        Some Failed
      end
    | ProverFinished pr ->
      let result, precise =
        match pr.pr_answer with
        | Valid ->
          Result.Valid pr.pr_time, true
        | Timeout ->
          Result.Timeout timeout, false
        | Invalid | Unknown _ | OutOfMemory | StepLimitExceeded ->
          Result.Unknown pr.pr_time, true
        | Failure _ | HighFailure _ ->
          let result =
            if !canceled then Result.NoResult
            else if !timedout then Result.Timeout timeout
            else Result.Failed
          in result, false
      in
      if !started then (Timer.stop timer; stop ?name ()) else unschedule () ;
      Option.iter (fun log ->
          if log = stdout then Utils.flush ();
          Printf.fprintf log "%a\n" (Yojson.pretty_to_channel ~std:false)
            (`Assoc [
                ("goal", `String (match idename with None -> "<none>" | Some s -> s));
                ("prover", `String (Prover.fullname prover));
                ("timeout", `Float timeout);
                ("time", `Float pr.pr_time);
                ("why3find time", `Float (Timer.total_time timer));
                ("result", `String (Format.asprintf "%a" Result.pp_verdict result));
              ])
        ) !logchannel;
      begin match callback with
        | Some cb ->
          if precise then
            notify_pr prover limits pr cb
          else
            notify config prover result cb
        | None -> ()
      end ;
      Some result
  end

(* -------------------------------------------------------------------------- *)
(* --- Running Prover with Cache                                          --- *)
(* -------------------------------------------------------------------------- *)

let hits = ref 0
let miss = ref 0

let prove_prepared config ?name ?idename ?cancel ?callback prover task timeout =
  call_prover config ?name ?idename ?cancel ?callback
    ~prepared:task.buffer ~prover ~timeout ()

let prove config ?name ?idename ?cancel ?callback cache prover task timeout =
  let cached = Cache.get cache task prover in
  let prooftask = prepare prover task in
  match Result.crop ~timeout cached with
  | Some cached ->
    incr hits ;
    Option.iter (notify config prover cached) callback ;
    Fibers.return cached
  | None ->
    incr miss ;
    Fibers.map
      (fun result -> Cache.set cache task prover result ; result)
      (prove_prepared config ?name ?idename ?cancel ?callback
         prover prooftask timeout)

let prove_cached cache prover task timeout =
  let cached = Cache.get cache task prover in
  let cropped = Result.crop ~timeout cached in
  incr (if cropped = None then miss else hits);
  cropped

let prove_buffer config ?cancel prover buffer timeout =
  call_prover config ?cancel ~prepared:buffer ~prover ~timeout ()

(* -------------------------------------------------------------------------- *)
(* --- Options                                                            --- *)
(* -------------------------------------------------------------------------- *)

let print_stats () =
  let hit = !hits in
  let all = hit + !miss in
  if all > 0 then Format.printf "Cache %d/%d@." hit all

(* -------------------------------------------------------------------------- *)
