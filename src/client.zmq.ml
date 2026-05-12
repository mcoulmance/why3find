(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Proof Client                                                       --- *)
(* -------------------------------------------------------------------------- *)

let host = ref ""
let port = ref 5555
let server = ref ""
let polling = ref 1.0
let trace = ref false

let options = [
  "--host",Arg.Set_string host,
  "HOST Proof Server host (default: none)" ;
  "--port",Arg.Set_int port,
  "PORT Proof Server port (default: 5555)" ;
  "--server",Arg.Set_string server,
  "URL Proof Server address (default: \"tcp://HOST:PORT\")" ;
  "--polling",Arg.Set_float polling,
  "TIME server polling interval (default 1.0s)";
  "--trace",Arg.Set trace,"Trace server protocol";
]

type goal = { prover : Prover.desc ; digest : string }

type task = {
  goal : goal ;
  prv : Prover.prover ;
  tsk : Runner.prooftask ;
  mutable timeout : float ;
  channel : Result.t Fibers.signal ;
}

type client = {
  wmain : Why3.Whyconf.main ;
  cache : Cache.t ;
  context : Zmq.Context.t ;
  socket : [`Dealer] Zmq.Socket.t ;
  profile : Calibration.profile ;
  pending : (goal,task) Hashtbl.t ;
  mutable terminated : bool ;
}

(* -------------------------------------------------------------------------- *)
(* --- Connection                                                         --- *)
(* -------------------------------------------------------------------------- *)

let clients : (string,client) Hashtbl.t = Hashtbl.create 0
let resolve () =
  if !server <> "" then Some !server else
  if !host <> "" then Some (Printf.sprintf "tcp://%s:%d" !host !port) else
    None

let connect (env : Project.env) =
  Option.map
    begin fun address ->
      try Hashtbl.find clients address with Not_found ->
        let wmain = Why3.Whyconf.get_main env.why3.config in
        let cache = env.cache in
        let context = Zmq.Context.create () in
        let socket = Zmq.Socket.(create context dealer) in
        let profile = Calibration.create () in
        let pending = Hashtbl.create 0 in
        Zmq.Socket.connect socket address ;
        Log.emit "Server %s" address ;
        let client = {
          wmain ; cache ; context ; socket ;
          profile ; pending ; terminated = false ;
        } in
        Hashtbl.add clients address client ;
        client
    end @@ resolve ()

(* -------------------------------------------------------------------------- *)
(* --- Socket                                                             --- *)
(* -------------------------------------------------------------------------- *)

let send client msg =
  if !trace then
    Log.emit "SEND -> %a" Utils.pp_args msg ;
  Zmq.Socket.send_all client.socket msg

let recv client fn =
  try
    let msg = Zmq.Socket.recv_all ~block:false client.socket in
    if !trace then
      Log.emit "<- RECV %a" Utils.pp_args msg ;
    fn msg
  with Unix.Unix_error(EAGAIN,_,_) -> ()

(* -------------------------------------------------------------------------- *)
(* --- Messages Sent                                                      --- *)
(* -------------------------------------------------------------------------- *)

let send_profile client prover (size,time) =
  let prover = Prover.desc_to_string prover.Prover.desc in
  send client
    ["PROFILE";prover;string_of_int size;string_of_float time]

let send_get client goal timeout =
  let prover = Prover.desc_to_string goal.prover in
  send client ["GET";prover;goal.digest;string_of_float timeout]

let send_upload client goal data =
  let prover = Prover.desc_to_string goal.prover in
  send client ["UPLOAD";prover;goal.digest;data]

(* -------------------------------------------------------------------------- *)
(* --- PROVE                                                              --- *)
(* -------------------------------------------------------------------------- *)

let get_task client prv tsk =
  let goal = { prover = prv.Prover.desc ; digest = Runner.digest tsk } in
  try Hashtbl.find client.pending goal with Not_found ->
    let channel = Fibers.signal () in
    let task = { goal ; prv ; tsk ; timeout = 0.0 ; channel } in
    Hashtbl.add client.pending goal task ; task

let request client profile prover task timeout =
  let open Fibers.Monad in
  let { wmain ; cache } = client in
  let htask = get_task client prover task in
  Fibers.background
    begin
      (* project profile *)
      let* project = Calibration.profile wmain cache profile prover in
      if Calibration.lock client.profile prover.desc then
        send_profile client prover project ;
      (* server profile *)
      let* server = Calibration.profile wmain cache client.profile prover in
      (* t-project / t-server *)
      let* gamma =
        let (np,tp) = project in
        let (ns,ts) = server in
        if np = ns then Fibers.return @@ tp /. ts else
          (* vp = tp / tr ; vs = tr / ts *)
          let+ vp = Calibration.velocity wmain cache profile prover
          and* vs = Calibration.velocity wmain cache client.profile prover
          in vs /. vp in
      (* server timeout *)
      let time = timeout /. gamma in
      if htask.timeout < time then
        begin
          htask.timeout <- time ;
          send_get client htask.goal time ;
        end ;
      Fibers.return ()
    end ;
  htask.channel

(* -------------------------------------------------------------------------- *)
(* --- CALIBRATION                                                        --- *)
(* -------------------------------------------------------------------------- *)

let do_profile client prover size time =
  Calibration.set client.profile prover size time

(* -------------------------------------------------------------------------- *)
(* --- RESULT                                                             --- *)
(* -------------------------------------------------------------------------- *)

let definitive ~timeout (result : Result.t) =
  match result with
  | NoResult -> false
  | Timeout t -> timeout <= t
  | Failed | Unknown _ | Valid _ -> true

let do_result client goal status time =
  let task = Hashtbl.find client.pending goal in
  let result =
    match status with
    | "NoResult" -> Result.NoResult
    | "Valid" -> Result.Valid time
    | "Unknown" -> Result.Unknown time
    | "Timeout" -> Result.Timeout time
    | _ -> Result.Failed
  in
  Fibers.background
    (Calibration.velocity client.wmain client.cache client.profile task.prv) ;
  if definitive ~timeout:task.timeout result then
    Hashtbl.remove client.pending task.goal ;
  Fibers.emit task.channel result

(* -------------------------------------------------------------------------- *)
(* --- DOWNLOAD                                                           --- *)
(* -------------------------------------------------------------------------- *)

let do_download client goal =
  let task = Hashtbl.find client.pending goal in
  send_upload client goal (Runner.data task.tsk)

(* -------------------------------------------------------------------------- *)
(* --- SERVER Handler                                                     --- *)
(* -------------------------------------------------------------------------- *)

let handler client msg =
  try match msg with
    | ["PROFILE";prover;size;time] ->
      let prover = Prover.desc_of_string prover in
      do_profile client prover (int_of_string size) (float_of_string time)
    | ["RESULT";prover;digest;status;time] ->
      let prover = Prover.desc_of_string prover in
      do_result client { prover ; digest } status (float_of_string time)
    | ["DOWNLOAD";prover;digest] ->
      let prover = Prover.desc_of_string prover in
      do_download client { prover ; digest }
    | _ -> ()
  with Not_found | Invalid_argument _ | Prover.InvalidProverDescription _ -> ()

(* -------------------------------------------------------------------------- *)
(* --- Running                                                            --- *)
(* -------------------------------------------------------------------------- *)

let yield () =
  Hashtbl.iter
    (fun addr client ->
       try recv client (handler client)
       with exn ->
         Log.emit "Client Error (%s:%s)" addr @@ Printexc.to_string exn
    ) clients

let terminate () =
  Hashtbl.iter
    (fun addr client ->
       try
         send client ["HANGUP"] ;
         Zmq.Socket.close client.socket ;
         Zmq.Context.terminate client.context ;
       with exn ->
         Log.emit "Client Killed (%s:%s)" addr @@ Printexc.to_string exn
    ) clients ;
  Hashtbl.clear clients

(* -------------------------------------------------------------------------- *)
