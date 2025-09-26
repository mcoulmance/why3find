(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Debug Adapter Base Protocol                                        --- *)
(* -------------------------------------------------------------------------- *)

type adapter = {
  mutable shutdown : bool ; (* has been shutdown *)
  mutable handling_request : bool ; (* processing request *)
  mutable handling_callback : bool ; (* processing callback *)
  channel : Rpc.channel ;
  callback : (adapter -> unit) Queue.t ;
}

let establish transport =
  let channel = Rpc.establish transport in
  Log.message "why3find debug adapter started." ; {
    channel ;
    shutdown = false ;
    handling_request = false ;
    handling_callback = false ;
    callback = Queue.create () ;
  }

let shutdown server = server.shutdown <- true

let running { shutdown ; channel }=
  not shutdown || Rpc.processing channel

(* -------------------------------------------------------------------------- *)
(* --- Message Encoding                                                   --- *)
(* -------------------------------------------------------------------------- *)

let next = let k = ref 0 in fun () -> incr k ; !k

let emit { channel } event body =
  Rpc.push channel @@ Json.to_string @@ Json.assoc [
    "seq" , `Int (next ()) ;
    "type", `String "event" ;
    "event", `String event ;
    "body", body ;
  ]

let send_response { channel } ~seq command body =
  Rpc.push channel @@ Json.to_string @@ Json.assoc [
    "seq" , `Int (next ()) ;
    "type" , `String "response" ;
    "request_seq" , `Int seq ;
    "command" , `String command ;
    "success" , `Bool true ;
    "body" , body ;
  ]

let send_error { channel } ~seq command message =
  Rpc.push channel @@ Json.to_string @@ Json.assoc [
    "seq" , `Int (next ()) ;
    "type" , `String "response" ;
    "request_seq" , `Int seq ;
    "command" , `String command ;
    "success" , `Bool false ;
    "message" , `String message ;
  ]

(* -------------------------------------------------------------------------- *)
(* --- Message Processing                                                 --- *)
(* -------------------------------------------------------------------------- *)

type handler = adapter -> Json.t -> Json.t
let protocol : (string,handler) Hashtbl.t = Hashtbl.create 0

let register ?(force=false) name fn =
  if not force && Hashtbl.mem protocol name then
    Log.error "Already registered request: %s" name
  else
    Hashtbl.add protocol name fn

let process_request adapter ~seq command arguments =
  begin
    match Hashtbl.find protocol command adapter arguments with
    | body ->
      send_response adapter ~seq command body
    | exception Exit ->
      send_response adapter ~seq command `Null
    | exception Not_found ->
      Log.error "Unsupported command (%s)" command ;
      send_error adapter ~seq command "unsupported"
    | exception Invalid_argument msg ->
      Log.error "Invalid argument (%s:%s)" command msg ;
      send_error adapter ~seq command "invalid"
    | exception Failure msg ->
      Log.error "Internal error (%s:%s)" command msg ;
      send_error adapter ~seq command "failure"
    | exception err ->
      let msg = Printexc.to_string err in
      Log.error "Internal error (%s:%s)" command msg ;
      send_error adapter ~seq command "failure"
  end

let process adapter =
  Rpc.yield adapter.channel ;
  if adapter.handling_request then false
  else
  if not adapter.handling_callback &&
     not (Queue.is_empty adapter.callback)
  then
    begin
      try
        adapter.handling_callback <- true ;
        Queue.pop adapter.callback adapter ;
        adapter.handling_callback <- false ;
        true
      with exn ->
        adapter.handling_callback <- false ;
        raise exn
    end
  else
    match Rpc.pull adapter.channel with
    | None -> false
    | Some data ->
      try
        adapter.handling_request <- true ;
        let rq = Json.of_string data in
        let seq = Json.jfield "seq" rq |> Json.jint in
        let kind = Json.jfield "type" rq |> Json.jstring in
        if kind <> "request" then raise (Invalid_argument kind) ;
        let command = Json.jfield "command" rq |> Json.jstring in
        let arguments = Json.jfield "arguments" rq in
        process_request adapter ~seq command arguments ;
        adapter.handling_request <- false ;
        Rpc.flush adapter.channel ; true
      with Invalid_argument msg ->
        Log.error "Invalid message (%s)" msg ;
        adapter.handling_request <- false ;
        Rpc.flush adapter.channel ; true

let yield adapter = ignore @@ process adapter

let schedule adapter fn = Queue.add fn adapter.callback

(* -------------------------------------------------------------------------- *)
(* --- Client Capabilities                                                --- *)
(* -------------------------------------------------------------------------- *)

let pathFormat : [ `Path | `Uri ] ref = ref `Path
let colStart = ref 1
let lineStart = ref 1
let supportedProgress = ref false

let setPathFormat = function
  | `String "path" -> pathFormat := `Path
  | `String "uri" -> pathFormat := `Uri
  | fmt -> Log.error "Unknown path format (%a)" Json.pretty fmt

let setColStart = function `Bool false -> colStart := 0 | _ -> ()
let setLineStart = function `Bool false -> lineStart := 0 | _ -> ()
let set bref = function `Bool b -> bref := b | _ -> ()

let of_line ~start js = (Json.jint js) - start + !lineStart
let to_line ~start ln : Json.t = `Int (ln - !lineStart + start)

let of_column ~start js = (Json.jint js) - start + !colStart
let to_column ~start col : Json.t = `Int (col - !colStart + start)

let uri_prefix = "file://"

let of_path js =
  let path = Json.jstring js in
  match !pathFormat with
  | `Path -> path
  | `Uri ->
    if String.starts_with ~prefix:uri_prefix path then
      let n = String.length path in
      let p = String.length uri_prefix in
      String.sub path p (n-p)
    else
      raise (Invalid_argument "Dap.of_path")

let to_path path : Json.t =
  `String
    begin match !pathFormat with
      | `Path -> path
      | `Uri -> uri_prefix ^ path
    end

(* -------------------------------------------------------------------------- *)
(* --- Server Capabilities                                                --- *)
(* -------------------------------------------------------------------------- *)

let capabilities : (string, Json.t) Hashtbl.t = Hashtbl.create 0

let register_capability cap ?(value=`Bool true) () =
  Hashtbl.replace capabilities cap value

(* -------------------------------------------------------------------------- *)
(* --- Initialize Request                                                 --- *)
(* -------------------------------------------------------------------------- *)

let () = register "initialize"
    begin fun _adapter args ->
      let clientName = Json.jfield "clientName" args |> Json.jstring in
      Log.message "Connected to client (%s)" clientName ;
      Json.jfield "pathFormat" args |> setPathFormat ;
      Json.jfield "linesStartAt1" args |> setLineStart ;
      Json.jfield "columnsStartAt1" args |> setColStart ;
      Json.jfield "supportsProgressReporting" args |> set supportedProgress ;
      let pool = ref [] in
      Hashtbl.iter (fun c v -> pool := (c,v) :: !pool) capabilities ;
      Json.assoc !pool
    end

(* -------------------------------------------------------------------------- *)
(* --- Attach Request                                                     --- *)
(* -------------------------------------------------------------------------- *)

let on_attach fn = register "attach"
    begin fun adapter args ->
      let nodebug = Json.jfield "noDebug" args |> Json.jbool in
      Queue.push (fun s -> fn s ~nodebug) adapter.callback ; `Null
    end

(* -------------------------------------------------------------------------- *)
(* --- Launch Request                                                     --- *)
(* -------------------------------------------------------------------------- *)

let on_launch fn =
  register "launch"
    begin fun adapter args ->
      let nodebug = Json.jfield "noDebug" args |> Json.jbool in
      let restart = Json.jfield "__restart" args in
      Queue.push (fun s -> fn s ~nodebug ~restart) adapter.callback ; `Null
    end

(* -------------------------------------------------------------------------- *)
(* --- Exited & Terminated Events                                         --- *)
(* -------------------------------------------------------------------------- *)

let exited adapter code =
  emit adapter "exited" @@ Json.assoc [ "exitCode" , `Int code ]

let terminated adapter ?(restart=`Null) () =
  emit adapter "terminated" @@ Json.assoc [ "restart" , restart ]

(* -------------------------------------------------------------------------- *)
(* --- Terminate Request                                                  --- *)
(* -------------------------------------------------------------------------- *)

let on_terminate fn =
  register_capability "supportsTerminateRequest" () ;
  register "terminate"
    begin fun adapter args ->
      let restart = Json.jfield "restart" args |> Json.jbool in
      Queue.push (fun s -> fn s ~restart) adapter.callback ; `Null
    end

(* -------------------------------------------------------------------------- *)
(* --- Disconnect Request                                                 --- *)
(* -------------------------------------------------------------------------- *)

type disconnect = Disconnect of { restart : bool ; terminate : bool ; suspend : bool }
let disconnect = ref (fun _ _ -> ())
let on_disconnect ?(terminate=false) ?(suspend=false) fn =
  if terminate then register_capability "supportTerminateDebuggee" () ;
  if suspend then register_capability "supportSuspendDebuggee" () ;
  disconnect := fn

let () = register "disconnect"
    begin fun adapter args ->
      let restart = Json.jfield "restart" args |> Json.jbool in
      let terminate = Json.jfield "terminateDebuggee" args |> Json.jbool in
      let suspend = Json.jfield "suspendDebuggee" args |> Json.jbool in
      if not restart then adapter.shutdown <- true ;
      Queue.push
        (fun s -> !disconnect s @@ Disconnect { restart ; terminate ; suspend })
        adapter.callback ; `Null
    end

(* -------------------------------------------------------------------------- *)
