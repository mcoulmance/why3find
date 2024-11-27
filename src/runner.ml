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
  Why3.Call_provers.resource_limit ->
  Why3.Call_provers.prover_result ->
  unit

(* -------------------------------------------------------------------------- *)
(* --- Prover Result                                                      --- *)
(* -------------------------------------------------------------------------- *)

type result =
  | NoResult | Failed | Unknown of float | Timeout of float | Valid of float

let merge a b =
  match a,b with
  | NoResult,c | c,NoResult -> c
  | Failed,c | c,Failed -> c
  | Valid ta , Valid tb -> Valid (min ta tb)
  | Valid _ , (Unknown _ | Timeout _) -> a
  | (Unknown _ | Timeout _) , Valid _ -> b
  | Unknown ta , Unknown tb -> Unknown (min ta tb)
  | Unknown _ , Timeout _ -> a
  | Timeout _ , Unknown _ -> b
  | Timeout ta , Timeout tb -> Timeout (max ta tb)

let map f = function
  | NoResult -> NoResult
  | Failed -> Failed
  | Valid t -> Valid (f t)
  | Unknown t -> Unknown (f t)
  | Timeout t -> Timeout (f t)

let pp_result fmt = function
  | NoResult -> Format.pp_print_string fmt "No Result"
  | Failed -> Format.pp_print_string fmt "Failed"
  | Unknown t -> Format.fprintf fmt "Unknown (%a)" Utils.pp_time t
  | Timeout t -> Format.fprintf fmt "Timeout (%a)" Utils.pp_time t
  | Valid t -> Format.fprintf fmt "Valid (%a)" Utils.pp_time t

let pp_verdict fmt = function
  | NoResult -> Format.pp_print_string fmt "No Result"
  | Failed -> Format.pp_print_string fmt "Failed"
  | Unknown _ -> Format.pp_print_string fmt "Unknown"
  | Timeout _ -> Format.pp_print_string fmt "Timeout"
  | Valid _ -> Format.pp_print_string fmt "Valid"

let of_json (js : Json.t) =
  let open Json in
  let status = jfield "status" js |> jstring in
  let time = jfield "time" js |> jfloat in
  match status with
  | "Failed" -> Failed
  | "Unknown" -> Unknown time
  | "Timeout" -> Timeout time
  | "Valid" -> Valid time
  | _ -> NoResult

let to_json (r : result) : Json.t =
  let status st = `Assoc [ "status", `String st ] in
  let timed st t = `Assoc [ "status", `String st ; "time", `Float t ] in
  match r with
  | NoResult -> status "NoResult"
  | Failed -> status "Failed"
  | Unknown t -> timed "Unknown" t
  | Timeout t -> timed "Timeout" t
  | Valid t -> timed "Valid" t

(* -------------------------------------------------------------------------- *)
(* --- Prover Cache                                                       --- *)
(* -------------------------------------------------------------------------- *)

let digest task = Why3.Termcode.(task_checksum task |> string_of_checksum)

let digest = Timer.timed ~name:"Task checksum" digest

module Cache : sig
  val cache : bool ref
  val get : Task.task * Prover.prover -> result
  val set : Task.task * Prover.prover -> result -> unit
end = struct

  let cachedir = ".why3find"
  let cachever = ".why3find/v3"
  let why3vers = ".why3find/why3version"

  let destroycache () =
    begin
      if Utils.tty then
        Log.emit "Upgrading cache (%s, why3 %s)" (Filename.basename cachever)
          Why3.Config.version ;
      Utils.rmpath cachedir ;
    end

  let preparecache () =
    begin
      Utils.mkdirs cachedir ;
      Utils.outputfile ~file:cachever begin fun out ->
        output_string out "why3find cache " ;
        output_string out (Filename.basename cachever) ;
        output_string out "\n" ;
      end ;
      Utils.writefile ~file:why3vers (Why3.Config.version ^ "\n")
    end

  let checkcache = lazy
    begin
      let cd = Sys.file_exists cachedir in
      let cv = Sys.file_exists cachever in
      let wv =
        Sys.file_exists why3vers
        && String.trim (Utils.readfile ~file:why3vers) = Why3.Config.version in
      Utils.flush () ;
      if cd && (not cv || not wv) then destroycache () ;
      if not cd || not cv || not wv then preparecache () ;
    end

  module Cache = Hashtbl.Make
      (struct
        type t = Task.task * Prover.prover
        let hash (t,p) = Hashtbl.hash (Task.task_hash t , p.Prover.digest)
        let equal (t1,p1) (t2,p2) = (p1 == p2) && (Task.task_equal t1 t2)
      end)

  let digest (t, p) =
    Digest.(to_hex (string (digest t ^ p.Prover.digest)))

  let file (t,p) =
    Lazy.force checkcache ;
    let hash = digest (t, p) in
    let h2 = String.sub hash 0 2 in
    Printf.sprintf ".why3find/%s/%s/%s.json" Prover.(desc_to_string p.desc) h2 hash

  let read e =
    let f = file e in
    if Sys.file_exists f then
      try Json.of_file f |> of_json with _ -> NoResult
    else NoResult

  let write e r =
    let f = file e in
    Utils.mkdirs (Filename.dirname f) ;
    Json.to_file f (to_json r)

  let hash = Cache.create 0
  let cache = ref true

  let get e =
    try Cache.find hash e with Not_found ->
      let r = if !cache then read e else NoResult in
      Cache.add hash e r ; r

  let set e r1 =
    let r0 = try Cache.find hash e with Not_found -> NoResult in
    let r = merge r0 r1 in
    if r <> r0 then
      match r with
      | NoResult -> ()
      | Failed ->
        Cache.replace hash e Failed
      | Valid _ | Unknown _ | Timeout _ ->
        Cache.replace hash e r ; write e r

end

type prooftask = Prepared of Why3.Task.task [@@ocaml.unboxed]

let digest (Prepared task) =
  digest task

let data prv (Prepared task) =
  let buffer = Buffer.create 2048 in
  let fmt = Format.formatter_of_buffer buffer in
  ignore @@ Driver.print_task_prepared prv.Prover.driver fmt task ;
  Format.pp_print_flush fmt () ;
  Buffer.contents buffer

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

let is_modified () = !jobs > 0

let save_config (env : Why3find.Config.env) =
  let j = !jobs in
  if j > 0 then
    let open Why3 in
    let file = Whyconf.get_conf_file env.wenv.config in
    if file <> "" then
      let main = Whyconf.get_main env.wenv.config in
      let time = Whyconf.timelimit main in
      let mem = Whyconf.memlimit main in
      let config = Whyconf.User.set_limits ~time ~mem ~j env.wenv.config in
      Whyconf.save_config config ;
      Format.printf "Why3 config saved to %s@." file

let maxjobs wmain =
  if !jobs > 0 then !jobs else
    Whyconf.running_provers_max wmain

let update_client wmain =
  let njobs = maxjobs wmain in
  if njobs <> !clients then
    begin
      clients := njobs ;
      Why3.Prove_client.set_max_running_provers njobs ;
    end

let memlimit wmain = Whyconf.memlimit wmain

let limit wmain t =
  Why3.Call_provers.{
    limit_time = t ;
    limit_mem = memlimit wmain ;
    limit_steps = (-1) ;
  }

let notify wmain prover result cb =
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
  let limit = limit wmain in
  match result with
  | NoResult -> ()
  | Valid t -> fire cb prover (limit t) (pr ~s:0 ~t Valid)
  | Timeout t -> fire cb prover (limit t) (pr ~s:1 ~t Timeout)
  | Unknown t -> fire cb prover (limit t) (pr ~s:1 ~t (Unknown "why3find"))
  | Failed -> fire cb prover (limit 1.0) (pr ~s:2 (Failure "why3find"))

let notify_pr prv limit pr cb =
  cb prv.Prover.config.prover limit pr

type prepared_task = Prooftask of prooftask | Buffered of Buffer.t

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
    -> limit:Call_provers.resource_limit -> Driver.driver -> prepared_task
    -> prover_call

  val interrupt_call : config:Whyconf.main -> prover_call -> unit

  val query_call : prover_call -> Call_provers.prover_update

end

module WRunner : WRunner = struct

  type prover_call = Call_provers.prover_call

  let prove_task ~command ~config ~limit driver = function
    | Prooftask (Prepared task) ->
      Driver.prove_task_prepared ~command ~config ~limit driver task
    | Buffered buffer ->
      Driver.prove_buffer_prepared ~command ~config ~limit driver buffer

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

  let prove_task ~command:_ ~config:_ ~limit:_ _ _ =
    { state = Unstarted }

  let interrupt_call ~config:_ c =
    match c.state with
    | Started -> c.state <- Interrupted
    | _ -> assert false

  let interrupted = (* expected answer, see below *)
    let highfailure = Call_provers.{
        pr_answer = HighFailure ;
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

let call_prover (config : Why3.Whyconf.main)
    ?(name : string option)
    ?(idename : string option)
    ?(cancel : unit Fibers.signal option)
    ?(callback : callback option)
    ~(prepared : prepared_task)
    ~(prover : Prover.prover)
    ~(timeout : float) () =
  let module WRunner = (val !wrunner) in
  let limit = limit config timeout in
  let timer = Timer.create () in
  let clockwall = ref 0.0 in
  let started = ref false in
  let killed = ref false in
  let canceled = ref false in
  let timedout = ref false in
  update_client config ;
  schedule () ;
  Timer.start launch_proof_timer;
  let call =
    WRunner.prove_task ~config ~limit prover.driver prepared
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
    let t0 = Unix.gettimeofday () in
    if !started then
      begin
        if !canceled then kill ()
        else if not !timedout && !clockwall < t0 then
          (timedout := true ; kill ()) ;
      end ;
    let query =
      try WRunner.query_call call with e -> InternalFailure e in
    match query with
    | NoUpdates
    | ProverInterrupted ->
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
    | InternalFailure _ ->
      begin
        if !started then stop ?name () ;
        Some Failed
      end
    | ProverFinished pr ->
      let result, precise =
        match pr.pr_answer with
        | Valid ->
          Valid pr.pr_time, true
        | Timeout ->
          Timeout timeout, false
        | Invalid | Unknown _ | OutOfMemory | StepLimitExceeded ->
          Unknown pr.pr_time, true
        | Failure _ | HighFailure ->
          (if !canceled then NoResult else
           if !timedout then Timeout timeout else
             Failed), false
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
                ("result", `String (Format.asprintf "%a" pp_verdict result));
              ])
        ) !logchannel;
      begin match callback with
        | Some cb ->
          if precise then
            notify_pr prover limit pr cb
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

let crop ~timeout result =
  match result with
  | NoResult -> None
  | Failed | Unknown _ -> Some result
  | Timeout t -> if timeout <= t then Some result else None
  | Valid t ->
    if t <= timeout *. 1.25 then Some result else Some (Timeout timeout)

let definitive ~timeout result =
  match result with
  | NoResult -> false
  | Timeout t -> timeout <= t
  | Failed | Unknown _ | Valid _ -> true

let prove_prepared config ?name ?idename ?cancel ?callback prover task timeout =
  call_prover config ?name ?idename ?cancel ?callback
    ~prepared:(Prooftask task) ~prover ~timeout ()

let prepare prover task =
  Prepared (Driver.prepare_task prover.Prover.driver task)

let prepare = Timer.timed2 ~name:"Task preparation" prepare

let prove config ?name ?idename ?cancel ?callback prover task timeout =
  let entry = task,prover in
  let cached = Cache.get entry in
  let task = prepare prover task in
  match crop ~timeout cached with
  | Some cached ->
    incr hits ;
    Option.iter (notify config prover cached) callback ;
    Fibers.return cached
  | None ->
    incr miss ;
    Fibers.map
      (fun result -> Cache.set entry result ; result)
      (prove_prepared config ?name ?idename ?cancel ?callback
         prover task timeout)

let prepare prover goal =
  prepare prover (Session.goal_task goal)

let update prover goal result =
  let task = Session.goal_task goal in
  Cache.set (task,prover) result

let prove_cached prover goal timeout =
  let task = Session.goal_task goal in
  let cached = Cache.get (task,prover) in
  let cropped = crop ~timeout cached in
  incr (if cropped = None then miss else hits);
  cropped

let prove_buffer config ?cancel prover buffer timeout =
  call_prover config ?cancel ~prepared:(Buffered buffer) ~prover ~timeout ()

(* -------------------------------------------------------------------------- *)
(* --- Options                                                            --- *)
(* -------------------------------------------------------------------------- *)

let options = [
  ("-c", Arg.Clear Cache.cache, "force cache update");
  ("-j", Arg.Set_int jobs, "JOBS max parallel provers");
  ("--log-prover-results", Arg.String set_logfile,
   "FILE log the prover results in the given file");
]

let print_stats () =
  let h = !hits in
  let m = !miss in
  Format.printf "Cache %d/%d@." h (h+m)

(* -------------------------------------------------------------------------- *)
