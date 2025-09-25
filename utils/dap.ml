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
  mutable processing : bool ; (* processing request *)
  channel : Rpc.channel ;
  callback : (unit -> unit) Queue.t ;
}

let establish transport =
  let channel = Rpc.establish transport in
  Log.message "why3find debug adapter started." ; {
    channel ;
    shutdown = false ;
    processing = false ;
    callback = Queue.create () ;
  }

let shutdown server = server.shutdown <- true

let running { shutdown ; channel }=
  not shutdown || Rpc.processing channel

let yield { channel } = Rpc.yield channel

let flush adapter =
  begin
    adapter.processing <- false ;
    Rpc.flush adapter.channel ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Message Encoding                                                   --- *)
(* -------------------------------------------------------------------------- *)

let next = let k = ref 0 in fun () -> incr k ; !k

let emit { channel } event body =
  Rpc.push channel @@ Json.to_string @@ `Assoc [
    "seq" , `Int (next ()) ;
    "type", `String "event" ;
    "event", `String event ;
    "body", body ;
  ]

let send_response { channel } ~seq command body =
  Rpc.push channel @@ Json.to_string @@ `Assoc [
    "seq" , `Int (next ()) ;
    "type" , `String "response" ;
    "request_seq" , `Int seq ;
    "command" , `String command ;
    "success" , `Bool true ;
    "body" , body ;
  ]

let send_error { channel } ~seq command message =
  Rpc.push channel @@ Json.to_string @@ `Assoc [
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
    adapter.processing <- true ;
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
  if adapter.processing then false
  else
  if not @@ Queue.is_empty adapter.callback then
    begin
      try
        adapter.processing <- true ;
        Queue.pop adapter.callback () ;
        adapter.processing <- false ;
        true
      with exn ->
        adapter.processing <- false ;
        raise exn
    end
  else
    match Rpc.pull adapter.channel with
    | None -> false
    | Some data ->
      try
        let rq = Json.of_string data in
        let seq = Json.jfield "seq" rq |> Json.jint in
        let kind = Json.jfield "type" rq |> Json.jstring in
        if kind <> "request" then raise (Invalid_argument kind) ;
        let command = Json.jfield "command" rq |> Json.jstring in
        let arguments = Json.jfield "arguments" rq in
        process_request adapter ~seq command arguments ;
        flush adapter ; true
      with Invalid_argument msg ->
        Log.error "Invalid message (%s)" msg ;
        flush adapter ; true

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

let capabilities : (string * Json.t) list ref = ref []

let register_capability cap ?(value=`Bool true) () =
  capabilities := (cap,value) :: !capabilities

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
      `Assoc (List.rev !capabilities)
    end

(* -------------------------------------------------------------------------- *)
(* --- Attach Request                                                     --- *)
(* -------------------------------------------------------------------------- *)

let attached = ref (fun ~debug -> ignore debug)
let on_attached fn = attached := fn

let () = register "attached"
    begin fun adapter args ->
      let debug = not (Json.jfield "noDebug" args |> Json.jbool) in
      Log.message "Debugger attached (%s)" (if debug then "debug" else "run") ;
      Queue.push (fun () -> !attached ~debug) adapter.callback ; `Null
    end

(* -------------------------------------------------------------------------- *)
(* --- Launch Request                                                     --- *)
(* -------------------------------------------------------------------------- *)

let launched = ref (fun ~debug ~restart -> ignore debug ; ignore restart)
let on_launched fn = launched := fn

let () = register "launch"
    begin fun adapter args ->
      let debug = Json.jfield "noDebug" args |> Json.jbool in
      let restart = Json.jfield "__restart" args in
      Log.message "Debugger launched (%s)" (if debug then "debug" else "run") ;
      Queue.push (fun () -> !launched ~debug ~restart) adapter.callback ; `Null
    end

(* -------------------------------------------------------------------------- *)
