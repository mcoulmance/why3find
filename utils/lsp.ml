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
(* --- Socket Bytes Read & Write                                          --- *)
(* -------------------------------------------------------------------------- *)

type response = (Json.t,Json.t) result
type callback = response -> unit
type request = {
  meth: string;
  param: Json.t;
  callback: callback option;
}

type channel = {
  mutable shutdown : bool ; (* has been shutdown *)
  mutable processing : bool ; (* processing a request *)
  queue : request Queue.t ; (* pending notifications *)
  callbacks : (string, callback) Hashtbl.t ;
  sock : Unix.file_descr ; (* Socket *)
  rcv : bytes ; (* RCV bytes buffer, re-used for transport *)
  snd : bytes ; (* SND bytes buffer, re-used for transport *)
  brcv : Buffer.t ; (* RCV data buffer, accumulated *)
  bsnd : Buffer.t ; (* SND data buffer, accumulated *)
}

let read_bytes { sock ; rcv ; brcv } =
  (* rcv buffer is only used locally *)
  let s = Bytes.length rcv in
  let rec scan p =
    (* try to fill RCV buffer *)
    let n =
      try Unix.read sock rcv p (s-p)
      with Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> 0
    in
    let p = p + n in
    if n > 0 && p < s then scan p else p
  in
  let n = scan 0 in
  if n > 0 then Buffer.add_subbytes brcv rcv 0 n

let send_bytes { sock ; snd ; bsnd } =
  (* snd buffer is only used locally *)
  let n = Buffer.length bsnd in
  if n > 0 then
    let s = Bytes.length snd in
    let rec send p =
      (* try to flush BSND buffer *)
      let w = min (n-p) s in
      Buffer.blit bsnd p snd 0 w ;
      let r =
        try Unix.single_write sock snd 0 w
        with Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> 0
      in
      let p = p + r in
      if r > 0 && p < n then send p else p
    in
    let p = send 0 in
    if p > 0 then
      let tail = Buffer.sub bsnd p (n-p) in
      Buffer.reset bsnd ;
      Buffer.add_string bsnd tail

let yield chan =
  send_bytes chan ; read_bytes chan

(* -------------------------------------------------------------------------- *)
(* --- Decoding Base Protocole                                            --- *)
(* -------------------------------------------------------------------------- *)

let ahead = "\r\n\r\n"
let content_length = Str.regexp "Content-Length: \\([0-9]+\\)\r\n"

let rec lookahead buf s p =
  if s = 4 then p else
    let s' = if ahead.[s] = Buffer.nth buf p then s+1 else 0 in
    (lookahead[@tailrec]) buf s' (p+1)

let pull { brcv } =
  try
    let h = lookahead brcv 0 0 in
    let head = Buffer.sub brcv 0 h in
    ignore @@ Str.search_forward content_length head 0 ;
    let d = int_of_string @@ Str.matched_group 1 head in
    let n = Buffer.length brcv in
    let data = Buffer.sub brcv h d in
    let tail = Buffer.sub brcv (h+d) (n-h-d) in
    Buffer.clear brcv ;
    Buffer.add_string brcv tail ;
    Some data
  with Not_found | Invalid_argument _ -> None

let push { bsnd } data =
  begin
    let n = String.length data in
    Printf.bprintf bsnd "Content-Length: %d\r\n\r\n%s" n data ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Decoding Json-RPC                                                  --- *)
(* -------------------------------------------------------------------------- *)

type handler = (channel -> Json.t -> Json.t)
let protocol : (string, handler) Hashtbl.t = Hashtbl.create 0

let json_header = "jsonrpc", `String "2.0"
let json_rpc msg : Json.t = `Assoc ( json_header :: msg )

let send_response channel ~id ~result =
  if not @@ Json.is_empty id then
    push channel @@ Json.to_string @@ json_rpc [ "id", id ; "result", result ]

let send_error channel ~id ~code ~msg =
  let err = `Assoc [
      "code", `Int code ;
      "message", `String msg ;
    ] in
  push channel @@ Json.to_string @@ json_rpc [ "id", id ; "error", err ]

let send_notification channel meth params =
  push channel @@ Json.to_string @@ json_rpc [
    "method", `String meth ;
    "params", params ;
  ]

let rqid = ref 0

let send_request channel meth params callback =
  let id = incr rqid ; Printf.sprintf "#%d" !rqid in
  Hashtbl.replace channel.callbacks id callback ;
  push channel @@ Json.to_string @@ json_rpc [
    "id", `String id ;
    "method", `String meth ;
    "params", params ;
  ]

let send channel rq =
  match rq.callback with
  | None -> send_notification channel rq.meth rq.param
  | Some fn -> send_request channel rq.meth rq.param fn

let flush channel =
  channel.processing <- false ;
  Queue.iter (send channel) channel.queue ;
  Queue.clear channel.queue ;
  send_bytes channel

let jresponse rq =
  if Json.mfield "result" rq
  then Ok (Json.jfield "result" rq)
  else Error (Json.jfield "error" rq)

let process_request channel ~id ~meth ~param =
  begin
    channel.processing <- true ;
    match Hashtbl.find protocol meth channel param with
    | result ->
      send_response channel ~id ~result
    | exception Not_found ->
      Log.error "Unsupported method: %s" meth ;
      send_error channel ~id ~code:(-32601) ~msg:meth
    | exception Invalid_argument msg ->
      Log.error "Invalid parameters (%s:%s)" meth msg ;
      send_error channel ~id ~code:(-32602) ~msg
    | exception Failure msg ->
      Log.error "Internal error (%s:%s)" meth msg ;
      send_error channel ~id ~code:(-32603) ~msg
    | exception err ->
      let msg = Printexc.to_string err in
      Log.error "Internal error (%s:%s)" meth msg ;
      send_error channel ~id ~code:(-32603) ~msg
  end

let process_response channel ~id ~result =
  begin
    channel.processing <- true ;
    match Hashtbl.find channel.callbacks id with
    | exception Not_found -> ()
    | fn ->
      Hashtbl.remove channel.callbacks id ; fn result
  end

let process channel =
  yield channel ;
  if not channel.processing then
    match pull channel with
    | None -> Unix.sleepf 0.01
    | Some data ->
      try
        let rq = Json.of_string data in
        begin
          if Json.mfield "method" rq then
            let id = Json.jfield "id" rq in
            let meth = Json.jfield "method" rq |> Json.jstring in
            let param = Json.jfield "param" rq in
            process_request channel ~id ~meth ~param
          else
            let id = Json.jfield "id" rq |> Json.jstring in
            let result = jresponse rq in
            process_response channel ~id ~result
        end ;
        flush channel
      with Invalid_argument msg ->
        Log.error "Invalid message (%s)" msg ;
        send_error channel ~id:(`Int 0) ~code:(-32700) ~msg ;
        flush channel

(* -------------------------------------------------------------------------- *)
(* --- Decoding LSP Messages                                              --- *)
(* -------------------------------------------------------------------------- *)

let register ?(force=false) name fn =
  if not force && Hashtbl.mem protocol name then
    Log.error "Already registered method or notification: %s" name
  else
    Hashtbl.add protocol name fn

let ignore name = register name (fun _ _ -> `Null)

let notify channel meth param =
  if channel.processing then
    Queue.push { meth ; param ; callback = None } channel.queue
  else
    send_notification channel meth param

let send channel meth param fn =
  if channel.processing then
    Queue.push { meth ; param ; callback = Some fn } channel.queue
  else
    send_request channel meth param fn

(* -------------------------------------------------------------------------- *)
(* --- Handling Transport I/O                                             --- *)
(* -------------------------------------------------------------------------- *)

type transport =
  | PIPE of string
  | PORT of int

let socket = function
  | PORT port ->
    Log.message "Socket port %d" port ;
    let fd = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
    Unix.setsockopt fd SO_REUSEADDR true ;
    let localhost = Unix.inet_addr_of_string "0.0.0.0" in
    Unix.bind fd (ADDR_INET(localhost,port)) ; fd
  | PIPE file ->
    Log.message "Socket pipe %s" file ;
    if Sys.file_exists file then Unix.unlink file ;
    let fd = Unix.socket PF_UNIX SOCK_STREAM 0 in
    Unix.bind fd (ADDR_UNIX file) ; fd

let set_socket_size sock opt s =
  begin
    let nbytes = s * 1024 in
    begin
      try Unix.setsockopt_int sock opt nbytes
      with Unix.Unix_error(err,_,_) ->
        let msg = Unix.error_message err in
        Log.warning "Invalid socket size (%d: %s)" nbytes msg ;
    end ;
    Unix.getsockopt_int sock opt
  end

let shutdown channel = channel.shutdown <- true

let running { shutdown ; processing ; brcv ; bsnd } =
  not shutdown
  || processing
  || Buffer.length brcv > 0
  || Buffer.length bsnd > 0

let establish transport =
  let exception Channel of channel in
  try
    Log.message "Why3 LSP Server" ;
    let fd = try socket transport with exn ->
      Log.error "Socket: %s" (Printexc.to_string exn) ;
      exit 1 in
    Unix.listen fd 1 ;
    while true do
      try
        Log.message "Waiting..." ;
        let sock,_addr = Unix.accept ~cloexec:true fd in
        Unix.set_nonblock sock ;
        let rcv = set_socket_size sock SO_RCVBUF 256 in
        let snd = set_socket_size sock SO_SNDBUF 256 in
        let channel = {
          sock ;
          shutdown = false ;
          processing = false ;
          queue = Queue.create () ;
          callbacks = Hashtbl.create 0 ;
          snd = Bytes.create snd ;
          rcv = Bytes.create rcv ;
          bsnd = Buffer.create snd ;
          brcv = Buffer.create rcv ;
        } in
        Log.message "Client connected." ;
        raise (Channel channel)
      with
      | Unix.Unix_error(EAGAIN,_,_) ->
        Unix.sleepf 0.1 ; (* 100ms *)
      | Unix.Unix_error(EPIPE,_,_) ->
        Log.message "Client disconnected." ;
        Unix.close fd ; exit 0
    done ;
    assert false
  with Channel channel -> channel

(* -------------------------------------------------------------------------- *)
(* ---  Default LSP Behavior                                              --- *)
(* -------------------------------------------------------------------------- *)

let () = register "initialize"
    begin fun _channel _params -> `Assoc [ "capabilities", `Assoc [] ] end

let () = register "shutdown"
    begin fun channel _data -> shutdown channel ; `Null end

let () = register "exit"
    begin fun channel _data -> shutdown channel ; `Null end

let () = ignore "initialized"
let () = ignore "$/setTrace"

(* -------------------------------------------------------------------------- *)
(* --- Launch Server                                                      --- *)
(* -------------------------------------------------------------------------- *)

let run transport =
  let channel = establish transport in
  while running channel do process channel done

(* -------------------------------------------------------------------------- *)
