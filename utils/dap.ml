(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let nop _ _ = ()
let trace = ref true

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
  if !trace then Format.eprintf "@[<hov 2>EMIT %S %a@]@." event Json.pretty body ;
  Rpc.push channel @@ Json.to_string @@ Json.assoc [
    "seq" , `Int (next ()) ;
    "type", `String "event" ;
    "event", `String event ;
    "body", body ;
  ]

let send_response { channel } ~seq command body =
  if !trace then Format.eprintf "@[<hov 2>RESPONSE %S %a@]@." command Json.pretty body ;
  Rpc.push channel @@ Json.to_string @@ Json.assoc [
    "seq" , `Int (next ()) ;
    "type" , `String "response" ;
    "request_seq" , `Int seq ;
    "command" , `String command ;
    "success" , `Bool true ;
    "body" , body ;
  ]

let send_error { channel } ~seq command message =
  if !trace then Format.eprintf "@[<hov 2>ERROR %S %S@]@." command message ;
  Rpc.push channel @@ Json.to_string @@ Json.assoc [
    "seq" , `Int (next ()) ;
    "type" , `String "response" ;
    "request_seq" , `Int seq ;
    "command" , `String command ;
    "success" , `Bool false ;
    "message" , `String message ;
  ]

(* -------------------------------------------------------------------------- *)
(* --- Output & Logging                                                   --- *)
(* -------------------------------------------------------------------------- *)

let ansi_styling = ref false

type category = [ `console | `important | `stdout | `stderr ]
let output adapter ?(category : category = `console) msg =
  let buffer = Buffer.create 80 in
  let fmt = Format.formatter_of_buffer buffer in
  if !ansi_styling then Utils.set_ansi_tags fmt ;
  Format.kfprintf
    begin
      fun fmt ->
        Format.pp_print_flush fmt () ;
        let output = Buffer.contents buffer in
        let category = match category with
          | `console -> "console"
          | `important -> "important"
          | `stdout -> "stdout"
          | `stderr -> "stderr" in
        emit adapter "output" @@ Json.assoc [
          "category", `String category ;
          "output", `String output ;
        ]
    end fmt msg

let protect fn adapter =
  try fn adapter with exn ->
    output adapter ~category:`important
      "Why3 DAP Error: %s" (Printexc.to_string exn) ;
    Rpc.flush adapter.channel

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
    if !trace then
      Format.eprintf "@[<hov 2>REQUEST %S %a@]@." command Json.pretty arguments ;
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
      adapter.handling_callback <- true ;
      protect (Queue.pop adapter.callback) adapter ;
      adapter.handling_callback <- false ;
      Rpc.flush adapter.channel ; true
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
let flush adapter = ignore @@ Rpc.flush adapter.channel

let schedule adapter fn = Queue.add fn adapter.callback

(* -------------------------------------------------------------------------- *)
(* --- Client Capabilities                                                --- *)
(* -------------------------------------------------------------------------- *)

let pathFormat : [ `Path | `Uri ] ref = ref `Path
let clientColStart = ref 1
let clientLineStart = ref 1
let supportedProgress = ref false

let setPathFormat = function
  | `String "path" -> pathFormat := `Path
  | `String "uri" -> pathFormat := `Uri
  | fmt -> Log.error "Unknown path format (%a)" Json.pretty fmt

let setColStartAt1 = function `Bool false -> clientColStart := 0 | _ -> clientColStart := 1
let setLineStartAt1 = function `Bool false -> clientLineStart := 0 | _ -> clientLineStart := 1
let setb r = function `Bool b -> r := b | _ -> ()
let none = (-1)
let of_line = function `Int n -> n - !clientLineStart | _ -> none
let to_line ln : Json.t = if ln < 0 then `Null else `Int (ln + !clientLineStart)
let of_column = function `Int n -> n - !clientColStart | _ -> none
let to_column col : Json.t = if col < 0 then `Null else `Int (col + !clientColStart)

let uri_prefix = "file://"

let of_source js =
  let path = Json.jfield "path" js |> Json.jstring in
  match !pathFormat with
  | `Path -> path
  | `Uri ->
    if String.starts_with ~prefix:uri_prefix path then
      let n = String.length path in
      let p = String.length uri_prefix in
      String.sub path p (n-p)
    else
      raise (Invalid_argument "Dap.of_path")

let to_source path : Json.t =
  if path = "" then `Null else
    let jpath =
      match !pathFormat with `Path -> path | `Uri -> uri_prefix ^ path in
    Json.assoc [ "path" , `String jpath ]

let js_int_opt n = if n <= 0 then `Null else `Int n
let js_bool_opt b = if b then `Bool true else `Null
let js_string_opt = function "" -> `Null | s -> `String s

(* -------------------------------------------------------------------------- *)
(* --- Server Capabilities                                                --- *)
(* -------------------------------------------------------------------------- *)

let capabilities : (string, Json.t) Hashtbl.t = Hashtbl.create 0

let register_capability cap ?(value=`Bool true) () =
  Hashtbl.replace capabilities cap value

let () = register_capability "supportsANSIStyling" ()

(* -------------------------------------------------------------------------- *)
(* --- Initialize Request                                                 --- *)
(* -------------------------------------------------------------------------- *)

let initialized adapter = emit adapter "initialized" `Null
let initcallback = ref initialized
let on_init fn = initcallback := fn

let () = register "initialize"
    begin fun adapter args ->
      let clientName = Json.jfield "clientName" args |> Json.jstring in
      Log.message "Connected to client (%s)" clientName ;
      Json.jfield "pathFormat" args |> setPathFormat ;
      Json.jfield "linesStartAt1" args |> setLineStartAt1 ;
      Json.jfield "columnsStartAt1" args |> setColStartAt1 ;
      Json.jfield "supportsANSIStyling" args |> setb ansi_styling ;
      Json.jfield "supportsProgressReporting" args |> setb supportedProgress ;
      let pool = ref [] in
      Hashtbl.iter (fun c v -> pool := (c,v) :: !pool) capabilities ;
      Queue.add !initcallback adapter.callback ;
      Json.assoc !pool
    end

(* -------------------------------------------------------------------------- *)
(* --- Configuration                                                      --- *)
(* -------------------------------------------------------------------------- *)

let on_configured fn =
  begin
    register_capability "supportsConfigurationDoneRequest" () ;
    register "configurationDone"
      (fun adapter _args -> Queue.add fn adapter.callback ; `Null) ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Launch & Attach Requests                                           --- *)
(* -------------------------------------------------------------------------- *)

let r_no_debug = ref false
let r_restarted = ref Json.null

let no_debug () = !r_no_debug
let restarted () = !r_restarted

let on_launch fn = register "launch"
    begin fun adapter args ->
      r_no_debug := Json.jfield "noDebug" args |> Json.jbool ;
      r_restarted := Json.jfield "__restart" args ;
      Queue.push fn adapter.callback ; `Null
    end

let on_attach fn = register "attach"
    begin fun adapter args ->
      r_no_debug := Json.jfield "noDebug" args |> Json.jbool ;
      r_restarted := `Null ;
      Queue.push fn adapter.callback ; `Null
    end

(* -------------------------------------------------------------------------- *)
(* --- Exited & Terminated Events                                         --- *)
(* -------------------------------------------------------------------------- *)

let exited adapter code =
  adapter.shutdown <- true ;
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
let disconnect = ref nop
let on_disconnect ?(terminate=false) ?(suspend=false) fn =
  if terminate then register_capability "supportTerminateDebuggee" () ;
  if suspend then register_capability "supportSuspendDebuggee" () ;
  disconnect := fn

let () = register "disconnect"
    begin fun adapter args ->
      let restart = Json.jfield "restart" args |> Json.jbool in
      let terminate = Json.jfield "terminateDebuggee" args |> Json.jbool in
      let suspend = Json.jfield "suspendDebuggee" args |> Json.jbool in
      Queue.push
        (fun s -> !disconnect s @@ Disconnect { restart ; terminate ; suspend })
        adapter.callback ; `Null
    end

(* -------------------------------------------------------------------------- *)
(* ---  Progress                                                          --- *)
(* -------------------------------------------------------------------------- *)

type progress = Idle | Progress of { id : string ; adapter : adapter }

let idle = Idle
let hprogress = Hashtbl.create 0
let kprogress = ref 0

let js_percent =
  function None -> Json.null | Some p -> Json.int (max 0 (min 100 p))

let progress adapter ?(id="") ~title ?cancel ?(message="") ?percent () : progress =
  if !supportedProgress then
    begin
      let k = incr kprogress ; !kprogress in
      let id = Format.sprintf "%s#%d" id k in
      emit adapter "progressStart" @@ Json.assoc [
        "progressId" , `String id ;
        "title" , `String title ;
        "cancellable" , js_bool_opt (cancel <> None) ;
        "message" , js_string_opt message ;
        "percentage", js_percent percent ;
      ] ;
      Option.iter (Hashtbl.add hprogress id) cancel ;
      Progress { id ; adapter }
    end
  else Idle

let update (progress : progress) ?(message="") ?percent () =
  match progress with
  | Idle -> ()
  | Progress { id ; adapter } ->
    if message <> "" && percent <> None then
      emit adapter "progressUpdate" @@ Json.assoc [
        "progressId" , `String id ;
        "message" , js_string_opt message ;
        "percentage", js_percent percent ;
      ]

let finished progress ?(message="") () =
  match progress with
  | Idle -> ()
  | Progress { id ; adapter } ->
    Hashtbl.remove hprogress id ;
    emit adapter "progressEnd" @@ Json.assoc [
      "progressId" , `String id ;
      "message" , js_string_opt message ;
    ]

let () = register "cancel"
    begin fun adapter args ->
      let id = Json.jfield "progressId" args |> Json.jstring in
      Option.iter
        (fun fn -> Queue.add fn adapter.callback)
        (Hashtbl.find_opt hprogress id) ;
      `Null
    end

(* -------------------------------------------------------------------------- *)
(* ---  Locations                                                         --- *)
(* -------------------------------------------------------------------------- *)

(* 0-started, <0 means none, "" means none *)
type location = Loc of {
    source : string ;
    line : int ;
    column : int ;
    end_line : int ;
    end_column : int ;
  }

let locrefs : (location, int) Hashtbl.t = Hashtbl.create 0
let locations : (int, location) Hashtbl.t = Hashtbl.create 0

let loc ?(source="") ?(line=none) ?(column=none) ?(end_line=none) ?(end_column=none) () =
  Loc { source ; line ; column ; end_line ; end_column }

let empty = loc ()

let locref (loc : location) : int =
  try Hashtbl.find locrefs loc with Not_found ->
    let r = Hashtbl.length locrefs in
    Hashtbl.add locrefs loc r ;
    Hashtbl.add locations r loc ; r

let js_locref = function None -> `Null | Some loc -> `Int (locref loc)

let () = register "locations"
    begin fun _adapter args ->
      let r = Json.jfield "locationReference" args |> Json.jint in
      let Loc l = Hashtbl.find locations r in
      Json.assoc [
        "source" , to_source l.source ;
        "line" , to_line l.line ;
        "column" , to_column l.column ;
        "endLine" , to_line l.end_line ;
        "endColumn" , to_column l.end_column ;
      ]
    end

(* -------------------------------------------------------------------------- *)
(* ---  Breakpoints                                                       --- *)
(* -------------------------------------------------------------------------- *)

type breakpoint = Break of Json.t [@@ unboxed]

let breakpoint
    ?(id=none) ?(verified=true) ?(message="") ?(source="")
    ?(line=none) ?(column=none) ?(end_line=none) ?(end_column=none)
    ?(reason="") ()
  = Break (Json.assoc [
      "id" , js_int_opt id ;
      "source" , to_source source ;
      "verified" , `Bool verified ;
      "message" , js_string_opt message ;
      "line" , to_line line ;
      "column" , to_column column ;
      "endLine" , to_line end_line ;
      "endColumn" , to_column end_column ;
      "reason", js_string_opt reason ;
    ])

type source_breakpoint =
    SourceBreakpoint of {
      source : string ;
      line : int ;
      column : int ;
      condition : string ;
      hit_condition : string ;
      log_message : string ;
    }

let of_source_breakpoint source js =
  let line = Json.jfield "line" js |> of_line in
  let column = Json.jfield "number" js |> of_column in
  let condition = Json.jfield "condition" js |> Json.jstring in
  let hit_condition = Json.jfield "hitCondition" js |> Json.jstring in
  let log_message = Json.jfield "logMessage" js |> Json.jstring in
  SourceBreakpoint { source ; line ; column ; condition ; hit_condition ; log_message }

let of_line_breakpoint source js =
  let line = of_line js in
  SourceBreakpoint {
    source ; line ; column = none ;
    condition = "" ; hit_condition = "" ; log_message = "" ;
  }

let verify_source_breakpoint = ref (fun _ -> failwith "unsupported breakpoints")

let on_breakpoint
    ?(condition=false)
    ?(hit_condition=false)
    ?(log_message=false)
    fn =
  begin
    if condition then register_capability "supportsConditionalBreakpoints" () ;
    if hit_condition then register_capability "supportsHitConditionalBreakpoints" () ;
    if log_message then register_capability "supportsLogPoints" () ;
    verify_source_breakpoint := fn ;
  end

let () = register "setBreakpoints"
    begin fun _adapter args ->
      let source = Json.jfield "source" args |> of_source in
      let breakpoints =
        List.map
          begin fun brk ->
            let Break js =
              try !verify_source_breakpoint brk with err ->
                breakpoint ~verified:false ~message:(Printexc.to_string err) ()
            in js
          end @@
        let breakpoints = Json.jfield "breakpoints" args |> Json.jlist in
        if breakpoints <> [] then
          List.map (of_source_breakpoint source) breakpoints
        else
          let lines = Json.jfield "lines" args |> Json.jlist in
          List.map (of_line_breakpoint source) lines
      in Json.assoc [ "breakpoints" , `List breakpoints ]
    end

let () = register "setExceptionBreakpoints" begin fun _ _ -> `Null end

(* -------------------------------------------------------------------------- *)
(* ---  Attributes                                                        --- *)
(* -------------------------------------------------------------------------- *)

type 'a attribute = {
  bythreads : (int,'a) Hashtbl.t ;
  temporary : (int,'a) Hashtbl.t ;
}
type value = (int -> unit)
let hooks = ref []
let varId n = 2*n
let frameId n = 2*n+1
let find_by_varId m id = Hashtbl.find m.temporary (varId id)
let find_by_frameId m id = Hashtbl.find m.temporary (frameId id)
let find_by_threadId m id = Hashtbl.find m.bythreads id
let store id values = List.iter (fun v -> v id) values
let value attr value id = Hashtbl.add attr.temporary id value
let attribute () =
  let temporary = Hashtbl.create 0 in
  let bythreads = Hashtbl.create 0 in
  hooks := (fun () -> Hashtbl.clear temporary) :: !hooks ;
  { temporary ; bythreads }

(* -------------------------------------------------------------------------- *)
(* ---  Variables                                                         --- *)
(* -------------------------------------------------------------------------- *)

type variable = Var of {
    name : string ;
    value : string ;
    vtype : string ;
    kind : string ;
    attributes : string list ;
    visiblity : string ;
    contents : variables ;
    decl : location option ;
    vdecl : location option ;
    values : value list ;
  }

and variables =
  | Vempty
  | Vars of {
      index : variable Darray.t ;
      named : variable Darray.t ;
    }

let vrefs = Hashtbl.create 0

let vref = function
  | Vempty -> 0,0,0
  | Vars { index ; named } ->
    let r = Hashtbl.length vrefs + 1 in
    Hashtbl.add vrefs r (index,named) ;
    r, Darray.length index, Darray.length named

let vempty = function None -> Darray.empty | Some vs -> vs

let variables index named =
  match index , named with
  | None , None -> Vempty
  | _ -> Vars { index = vempty index ; named = vempty named }

let variable ~name
    ?(value = "") ?(vtype = "") ?(kind = "") ?(attributes=[]) ?(visiblity="")
    ?index ?named ?decl ?vdecl ?(values=[]) () =
  let contents = variables index named in
  Var {
    name ; value ;
    vtype ; kind ; attributes ; visiblity ;
    contents ; decl ; vdecl ;
    values ;
  }

let js_variable (Var v) : Json.t =
  let vref,indexed,named = vref v.contents in
  store (varId vref) v.values ;
  Json.assoc [
    "name" , `String v.name ;
    "value" , `String v.value ;
    "type" , js_string_opt v.vtype ;
    "presentationHint" , Json.assoc ~nullempty:true [
      "kind" , js_string_opt v.kind ;
      "attributes" , Json.list_map ~nullempty:true js_string_opt v.attributes ;
      "visibility" , js_string_opt v.visiblity ;
    ] ;
    "variablesReference" , `Int vref ;
    "indexedVariables" , js_int_opt indexed ;
    "namedVariables" , js_int_opt named ;
    "declarationLocationReference" , js_locref v.decl ;
    "valueLocationReference" , js_locref v.vdecl ;
  ]

let () = register_capability "supportsVariablePaging" ()
let () = register "variables"
    begin fun _adapter args ->
      let vref = Json.jfield "variablesReference" args |> Json.jint in
      let filter = Json.jfield "filter" args |> Json.jstring in
      let start = Json.jfield "start" args |> Json.jint in
      let count = Json.jfield "count" args |> Json.jdefault max_int Json.jint in
      let varray =
        try
          let (index,named) = Hashtbl.find vrefs vref in
          match filter with
          | "named" -> named
          | "indexed" -> index
          | _ -> Darray.concat named index
        with Not_found -> Darray.empty in
      let last = min (start + count) (Darray.length varray) - 1 in
      let pool = ref [] in
      for k = last downto start do
        pool := (js_variable @@ Darray.get varray k) :: !pool
      done ;
      Json.assoc [ "variables" , `List !pool ] ;
    end

(* -------------------------------------------------------------------------- *)
(* ---  Scopes                                                            --- *)
(* -------------------------------------------------------------------------- *)

type scope = Scope of {
    name: string ;
    kind: string ;
    variables: variables ;
    expensive: bool ;
    range: location ;
  }

let scope ~name ?(kind="") ?index ?named ?(expensive=true) ?(range=empty) () =
  Scope { name ; kind; variables = variables index named ; expensive ; range }

let js_scope (Scope s) =
  let vref, indexed, named = vref s.variables in
  let Loc r = s.range in
  Json.assoc [
    "name" , `String s.name ;
    "presentationHint" , js_string_opt s.kind ;
    "variablesReference" , `Int vref ;
    "indexedVariables" , js_int_opt indexed ;
    "namedVariables" , js_int_opt named ;
    "expensive" , `Bool s.expensive ;
    "source" , to_source r.source ;
    "line" , to_line r.line ;
    "column" , to_column r.column ;
    "endLine" , to_column r.end_line ;
    "endColumn" , to_column r.end_column ;
  ]

let scopes : (int,scope list) Hashtbl.t = Hashtbl.create 0
let () = register "scopes"
    begin fun _adapter args ->
      let id = Json.jfield "frameId" args |> Json.jint in
      let ds = try Hashtbl.find scopes id with Not_found -> [] in
      Json.assoc [ "scopes" , Json.list_map js_scope ds ]
    end

(* -------------------------------------------------------------------------- *)
(* ---  Frames                                                            --- *)
(* -------------------------------------------------------------------------- *)

type frame =
  | Label of string
  | Frame of {
      name : string ;
      range : location ;
      restart : (adapter -> unit) option ;
      scopes : scope list ;
      values : value list ;
    }

let restartframes = Hashtbl.create 0
let restartableFrames = ref false
let setRestartableFrames ()  =
  begin
    restartableFrames := true ;
    register_capability "supportsRestartFrame" () ;
    register "restartFrame"
      begin fun adapter args ->
        let id = Json.jfield "frameId" args |> Json.jint in
        let callback = Hashtbl.find restartframes id in
        Queue.add callback adapter.callback ; `Null
      end
  end

let label ~name = Label name
let frame ~name ?(range=empty) ?restart ?(scopes=[]) ?(values=[]) () =
  if not (!restartableFrames || restart = None) then
    raise (Invalid_argument "Dap.restartableFrames") ;
  Frame { name ; range ; restart ; scopes ; values }

let js_frame (fr : frame) =
  let id = Hashtbl.length scopes in
  match fr with
  | Label name ->
    Hashtbl.add scopes id [] ;
    Json.assoc [
      "id" , `Int id ;
      "name" , `String name ;
      "presentationHint" , `String "label" ;
    ]
  | Frame fr ->
    store (frameId id) fr.values ;
    Hashtbl.add scopes id fr.scopes ;
    Option.iter (Hashtbl.add restartframes id) fr.restart ;
    let Loc r = fr.range in
    Json.assoc [
      "id" , `Int id ;
      "name" , `String fr.name ;
      "source" , to_source r.source ;
      "line" , to_line r.line ;
      "column" , to_column r.column ;
      "endLine" , to_column r.end_line ;
      "endColumn" , to_column r.end_column ;
      "canRestart" , `Bool (fr.restart <> None) ;
    ]

(* -------------------------------------------------------------------------- *)
(* ---  Threads                                                           --- *)
(* -------------------------------------------------------------------------- *)

type reason = [ `step | `pause | `entry | `except | `error ]

type thread = Thread of {
    id : int ;
    name : string ;
    adapter : adapter ;
    mutable frames : (thread -> frame Darray.t) ;
    mutable on_pause : (adapter -> unit) ;
    mutable on_next : (adapter -> unit) ;
    mutable on_step_in : (adapter -> unit) ;
    mutable on_step_out : (adapter -> unit) ;
    mutable on_continue : (adapter -> unit) ;
  }

let threadk = ref 0
let threads : (int,thread) Hashtbl.t = Hashtbl.create 0
let singleThreadSupport = ref false
let id (Thread th) = th.id
let find = Hashtbl.find threads
let living th = Hashtbl.mem threads (id th)
let get attr th = Hashtbl.find attr.bythreads (id th)
let set attr th v = Hashtbl.replace attr.bythreads (id th) v
let get_opt attr th = try Some (get attr th) with Not_found -> None
let remove attr th = Hashtbl.remove attr.bythreads (id th)

let pending () =
  let pool = ref [] in
  Hashtbl.iter (fun _ th -> pool := th :: !pool) threads ;
  List.sort (fun a b -> Int.compare (id a) (id b)) !pool

let reset_identifiers () =
  begin
    Hashtbl.clear vrefs ;
    Hashtbl.clear scopes ;
    Hashtbl.clear restartframes ;
    List.iter (fun fn -> fn ()) !hooks ;
  end

let updated ?(reason="updated") (Thread th) =
  emit th.adapter "thread" @@ Json.assoc [
    "reason", `String reason ;
    "threadId" , `Int th.id ;
  ]

let set_single_thread_support () =
  begin
    singleThreadSupport := true ;
    register_capability "supportsSingleThreadExecutionRequests" () ;
  end

let connect (Thread th)
    ?frames ?on_pause ?on_next ?on_step_in ?on_step_out ?on_continue () =
  begin
    let set_default fn =
      begin
        if th.on_next == ignore then th.on_next <- fn ;
        if th.on_step_in == ignore then th.on_step_in <- fn ;
        if th.on_step_out == ignore then th.on_step_out <- fn ;
        if th.on_continue == ignore then th.on_continue <- fn ;
      end in
    Option.iter (fun fn -> th.frames <- fn) frames ;
    Option.iter (fun fn -> th.on_pause <- fn) on_pause ;
    Option.iter (fun fn -> th.on_next <- fn) on_next ;
    Option.iter (fun fn -> th.on_step_in <- fn) on_step_in ;
    Option.iter (fun fn -> th.on_step_out <- fn) on_step_out ;
    Option.iter (fun fn -> th.on_continue <- fn) on_continue ;
    Option.iter set_default on_next ;
    Option.iter set_default on_continue ;
  end

let no_frames _th = Darray.empty

let thread adapter
    ~name ?(frames=no_frames) ?on_pause ?on_next ?on_step_in ?on_step_out ?on_continue () =
  let id = incr threadk ; !threadk in
  let th = Thread {
      id ; adapter ; name ;
      frames ;
      on_pause = ignore ;
      on_continue = ignore ;
      on_next = ignore ;
      on_step_in = ignore ;
      on_step_out = ignore ;
    } in
  Hashtbl.add threads id th ;
  updated ~reason:"started" th ;
  connect th ?on_pause ?on_next ?on_step_in ?on_step_out ?on_continue () ; th

let terminate thread =
  let Thread { id } = thread in
  Hashtbl.remove threads id ;
  updated ~reason:"exited" thread

let stopped ~(reason:reason) ?description ?text ?all ?preserveFocus ?frames thread =
  begin
    if all = Some false && not !singleThreadSupport then
      raise (Invalid_argument "no single thread support") ;
    let all = Option.value ~default:(not !singleThreadSupport) all in
    let Thread th = thread in
    Option.iter (fun frames -> th.frames <- frames) frames ;
    let reason = match reason with
      | `entry -> "entry"
      | `step -> "step"
      | `error -> "error"
      | `pause -> "pause"
      | `except -> "exception" in
    reset_identifiers () ;
    emit th.adapter "stopped" @@ Json.assoc [
      "reason" , `String reason ;
      "description" , Json.option_map Json.string description ;
      "text" , Json.option_map Json.string text ;
      "threadId" , `Int th.id ;
      "allThreadsStopped" , if all then `Bool true else `Null ;
      "preserveFocusHint" , Json.option_map Json.bool preserveFocus ;
    ]
  end

let () = register "pause"
    begin fun adapter args ->
      let threadId = Json.jfield "threadId" args |> Json.jint in
      Option.iter
        (function (Thread th) -> Queue.push th.on_pause adapter.callback)
        (Hashtbl.find_opt threads threadId) ;
      `Null
    end

let resume mode adapter args =
  let threadId = Json.jfield "threadId" args |> Json.jint in
  let single = Json.jfield "sinleThread" args |> Json.jbool in
  Option.iter
    (fun th -> Queue.push (mode th) adapter.callback)
    (Hashtbl.find_opt threads threadId) ;
  if not !singleThreadSupport || not single then `Null else
    Json.assoc [ "allThreadsContinued", `Bool (not single) ]

let () = register "next" @@ resume (function Thread th -> th.on_next)
let () = register "stepIn" @@ resume (function Thread th -> th.on_step_in)
let () = register "stepOut" @@ resume (function Thread th -> th.on_step_out)
let () = register "continue" @@ resume (function Thread th -> th.on_continue)

let () = register "threads"
    begin fun _adapter _args ->
      let pool = ref [] in
      Hashtbl.iter (fun _ th -> pool := th :: !pool) threads ;
      let byId (Thread a) (Thread b) = Int.compare a.id b.id in
      let jthread (Thread th) = Json.assoc [
          "id" , `Int th.id ;
          "name", `String th.name ;
        ] in
      Json.assoc [ "threads" , `List (List.map jthread @@ List.sort byId !pool)]
    end

let () = register "stackFrame"
    begin fun _adapter args ->
      let thId = Json.jfield "threadId" args |> Json.jint in
      let start = Json.jfield "startFrame" args |> Json.jint in
      let levels = Json.jfield "levels" args |> Json.jdefault max_int Json.jint in
      let Thread t as th = Hashtbl.find threads thId in
      let frames = t.frames th in
      let total = Darray.length frames in
      let last = min (start + levels) total - 1 in
      let pool = ref [] in
      for k = last downto start do
        pool := js_frame (Darray.get frames k) :: !pool ;
      done ;
      Json.assoc [
        "stackFrames", `List !pool ;
        "totalFrames", `Int total ;
      ]
    end

(* -------------------------------------------------------------------------- *)
