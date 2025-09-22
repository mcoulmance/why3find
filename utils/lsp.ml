(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Language Server Base Protocol                                      --- *)
(* -------------------------------------------------------------------------- *)

type response = (Json.t,Json.t) result
type callback = response -> unit

type message =
  | Notify of { meth: string; param: Json.t }
  | Request of { meth: string; param: Json.t; callback: callback }

type server = {
  mutable shutdown : bool ; (* has been shutdown *)
  mutable processing : bool ; (* processing a request *)
  queue : message Queue.t ; (* pending notifications *)
  callbacks : (string, callback) Hashtbl.t ;
  channel : Rpc.channel ;
}

(* -------------------------------------------------------------------------- *)
(* --- Decoding Json-RPC                                                  --- *)
(* -------------------------------------------------------------------------- *)

type handler = (server -> Json.t -> Json.t)
let protocol : (string, handler) Hashtbl.t = Hashtbl.create 0

let json_header = "jsonrpc", `String "2.0"
let json_rpc msg : Json.t = `Assoc ( json_header :: msg )

let send_response { channel } ~id ~result =
  if not @@ Json.is_empty id then
    Rpc.push channel @@ Json.to_string @@ json_rpc [
      "id", id ;
      "result", result
    ]

let send_error { channel } ~id ~code ~msg =
  if not @@ Json.is_empty id then
    Rpc.push channel @@ Json.to_string @@ json_rpc [
      "id", id ;
      "error", `Assoc [
        "code", `Int code ;
        "message", `String msg ;
      ]]

let send_notification { channel } meth params =
  Rpc.push channel @@ Json.to_string @@ json_rpc [
    "method", `String meth ;
    "params", params ;
  ]

let rqid = ref 0
let make_id () = incr rqid ; Printf.sprintf "#%d" !rqid

let send_request server meth params callback =
  let id = make_id () in
  Hashtbl.replace server.callbacks id callback ;
  Rpc.push server.channel @@ Json.to_string @@ json_rpc [
    "id", `String id ;
    "method", `String meth ;
    "params", params ;
  ]

let send_message ch = function
  | Notify { meth = m ; param = p } ->
    send_notification ch m p
  | Request { meth = m ; param = p ; callback = fn } ->
    send_request ch m p fn

let flush server =
  begin
    server.processing <- false ;
    Queue.iter (send_message server) server.queue ;
    Queue.clear server.queue ;
    Rpc.flush server.channel ;
  end

let jresponse rq =
  if Json.mfield "result" rq
  then Ok (Json.jfield "result" rq)
  else Error (Json.jfield "error" rq)

let process_request server ~id ~meth ~params =
  begin
    server.processing <- true ;
    match Hashtbl.find protocol meth server params with
    | result ->
      send_response server ~id ~result
    | exception Not_found ->
      if not @@ String.starts_with ~prefix:"$/" meth then
        Log.error "Unsupported method: %s" meth ;
      (* still error the request if there is an id *)
      send_error server ~id ~code:(-32601) ~msg:meth
    | exception Invalid_argument msg ->
      Log.error "Invalid parameters (%s:%s)" meth msg ;
      send_error server ~id ~code:(-32602) ~msg
    | exception Failure msg ->
      Log.error "Internal error (%s:%s)" meth msg ;
      send_error server ~id ~code:(-32603) ~msg
    | exception err ->
      let msg = Printexc.to_string err in
      Log.error "Internal error (%s:%s)" meth msg ;
      send_error server ~id ~code:(-32603) ~msg
  end

let process_response server ~id ~result =
  begin
    server.processing <- true ;
    match Hashtbl.find server.callbacks id with
    | exception Not_found -> ()
    | fn ->
      Hashtbl.remove server.callbacks id ; fn result
  end

let process server =
  Rpc.yield server.channel ;
  if server.processing then false
  else
    match Rpc.pull server.channel with
    | None -> false
    | Some data ->
      try
        let rq = Json.of_string data in
        begin
          if Json.mfield "method" rq then
            let id = Json.jfield "id" rq in
            let meth = Json.jfield "method" rq |> Json.jstring in
            let params = Json.jfield "params" rq in
            process_request server ~id ~meth ~params
          else
            let id = Json.jfield "id" rq |> Json.jstring in
            let result = jresponse rq in
            process_response server ~id ~result
        end ;
        flush server ; true
      with Invalid_argument msg ->
        Log.error "Invalid message (%s)" msg ;
        send_error server ~id:(`Int 0) ~code:(-32700) ~msg ;
        flush server ; true

let yield { channel } = Rpc.yield channel

(* -------------------------------------------------------------------------- *)
(* --- Decoding LSP Messages                                              --- *)
(* -------------------------------------------------------------------------- *)

let register ?(force=false) name fn =
  if not force && Hashtbl.mem protocol name then
    Log.error "Already registered method or notification: %s" name
  else
    Hashtbl.add protocol name fn

let ignore name = register name (fun _ _ -> `Null)

let notify server meth param =
  if server.processing then
    Queue.push (Notify { meth ; param }) server.queue
  else
    send_notification server meth param

let error meth = function Ok _ -> () | Error data ->
  Format.eprintf "[%s] ERROR: %a@." meth Json.pretty data

let send server meth ?(callback = error meth) param =
  if server.processing then
    Queue.push (Request { meth ; param ; callback }) server.queue
  else
    send_request server meth param callback

(* -------------------------------------------------------------------------- *)
(* --- Handling Transport I/O                                             --- *)
(* -------------------------------------------------------------------------- *)

let shutdown server = server.shutdown <- true

let running { shutdown ; processing ; channel } =
  not shutdown || processing || Rpc.processing channel

let establish transport = {
  shutdown = false ;
  processing = false ;
  queue = Queue.create () ;
  callbacks = Hashtbl.create 0 ;
  channel = Rpc.establish transport ;
}

(* -------------------------------------------------------------------------- *)
(* ---  Default LSP Behavior (refined in lspserver)                       --- *)
(* -------------------------------------------------------------------------- *)

let () = register "initialize"
    begin fun _channel _params -> `Assoc [ "capabilities", `Assoc [] ] end

let () = register "shutdown"
    begin fun server _data -> shutdown server ; `Null end

let () = register "exit"
    begin fun server _data -> shutdown server ; `Null end

let () = ignore "initialized"
let () = ignore "$/setTrace"

(* -------------------------------------------------------------------------- *)
(* --- Capabilities                                                       --- *)
(* -------------------------------------------------------------------------- *)

type capability = Capability of { id: string ; meth: string }

let jregistration id meth param : Json.t =
  Json.assoc [
    "id", `String id ;
    "method", `String meth ;
    "registerOptions", param ;
  ]

let register_capability server meth param =
  let id = make_id () in
  send server "client/registerCapability" @@ Json.assoc [
    "registrations", `List [ jregistration id meth param ]
  ] ;
  Capability { id ; meth }

let unregister_capability server = function Capability { id ; meth } ->
  send server "client/unregisterCapability" @@ Json.assoc [
    "unregisterations", `List [ jregistration id meth `Null ]
  ]

(* -------------------------------------------------------------------------- *)
