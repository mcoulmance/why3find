(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let register demons f = Queue.push f demons
let notify demons a b = Queue.iter (fun fn -> fn a b) demons
let notify_all demons a xs = Queue.iter (fun fn -> List.iter (fn a) xs) demons

(* -------------------------------------------------------------------------- *)
(* --- URI Utilities                                                      --- *)
(* -------------------------------------------------------------------------- *)

let file_uri = "file://"
let to_file ~uri =
  if String.starts_with ~prefix:file_uri uri then
    let p = String.length file_uri in
    let n = String.length uri in
    Some (String.sub uri p (n-p))
  else None

let to_uri ~filename =
  assert (not @@ Filename.is_relative filename) ;
  file_uri ^ filename

(* -------------------------------------------------------------------------- *)
(* --- LSP Initialize                                                     --- *)
(* -------------------------------------------------------------------------- *)

let definitionProvider = ref false
let typedefProvider = ref false
let implementationProvider = ref false
let hoverProvider = ref false
let codeLensProvider = ref false
let codeLensRefresh = ref false
let documentSymbolProvider = ref false
let foldingRangeProvider = ref false
let foldingRangeRefresh = ref false

let () = Lsp.register ~force:true "initialize"
    begin fun _channel params ->
      let workspace = Json.jpath "capabilities/workspace" params in
      codeLensRefresh :=
        Json.jbool @@ Json.jpath "codeLens/refreshSupport" workspace ;
      foldingRangeRefresh :=
        Json.jbool @@ Json.jpath "foldingRange/refreshSupport" workspace ;
      `Assoc [
        "capabilities", `Assoc [
          "textDocumentSync", `Assoc [
            "openClose", `Bool true ;
            "change", `Int 2 ;
            "save", `Bool true ;
          ] ;
          "definitionProvider", `Bool !definitionProvider ;
          "typeDefinitionProvider", `Bool !typedefProvider ;
          "implementationProvider", `Bool !implementationProvider ;
          "hoverProvider", `Bool !hoverProvider ;
          "codeLensProvider", `Bool !codeLensProvider ;
          "documentSymbolProvider", `Bool !documentSymbolProvider;
          "foldingRangeProvider", `Bool !foldingRangeProvider ;
          "executeCommandProvider", `Bool true ;
        ];
        "serverInfo", `Assoc [
          "name", `String "why3find" ;
          "version", `String Version.version ;
        ];
      ]
    end

(* -------------------------------------------------------------------------- *)
(* --- Documents                                                          --- *)
(* -------------------------------------------------------------------------- *)

type change = Text of string | Update of Range.range * string
type update = { before: Ropes.t ; change: change ; after: Ropes.t }

let jposition js =
  let line = Json.jfield "line" js |> Json.jint in
  let col = Json.jfield "character" js |> Json.jint in
  Range.at ~line ~col ()

let wposition Range.{ line ; col } : Json.t =
  `Assoc [ "line", `Int line ; "character", `Int col ]

let jrange js =
  let p = Json.jfield "start" js |> jposition in
  let q = Json.jfield "end" js |> jposition in p,q

let wrange (p,q) : Json.t =
  `Assoc [ "start", wposition p ; "end", wposition q ]

let jchange js =
  let text = Json.jfield "text" js |> Json.jstring in
  match Json.jfield "range" js |> Json.joption jrange with
  | None -> Text text
  | Some range -> Update(range,text)

type document = Doc of { uri: string ; lang: string ; text: Ropes.t }

type location = Loc of { uri: string ; range: Range.range}

let location ~uri ~range = Loc { uri ; range }

let wlocation (loc: location) : Json.t =
  match loc with Loc { uri ; range } ->
    `Assoc [
      "uri", `String uri ;
      "range", wrange range ;
    ]

let apply (Doc doc) update =
  let text =
    match update with
    | Text text -> Ropes.of_string text
    | Update(range,text) -> Ropes.update range text doc.text
  in Doc { doc with text }

(* Indexed by uri *)
let documents : (string,document) Hashtbl.t = Hashtbl.create 0

let document ~uri = Hashtbl.find documents uri

(* -------------------------------------------------------------------------- *)
(* ---  Position Translation                                              --- *)
(* -------------------------------------------------------------------------- *)

let backward_position (u : update) (p : Range.pos) =
  match u.change with
  | Text _ -> None
  | Update((start,stop),_) ->
    if p.line < start.line then Some p else
    if p.line = start.line then
      if p.col <= start.col then Some p else None
    else
    if p.line > stop.line then
      let delta = Ropes.lines u.after - Ropes.lines u.before in
      Some (Range.at ~line:(p.line - delta) ~col:p.col ())
    else
      None

let forward_position (p : Range.pos) (u : update) =
  match u.change with
  | Text _ -> None
  | Update((start,stop),_) ->
    if p.line < start.line then Some p else
    if p.line = start.line then
      if p.col <= start.col then Some p else None
    else
    if p.line > stop.line then
      let delta = Ropes.lines u.after - Ropes.lines u.before in
      Some (Range.at ~line:(p.line + delta) ~col:p.col ())
    else
      None

let backward_range (u: update) ((start,stop) : Range.range) =
  match
    backward_position u start,
    backward_position u stop
  with Some start , Some stop -> Some (start,stop) | _ -> None

let forward_range ((start,stop) : Range.range) (u: update) =
  match
    backward_position u start,
    backward_position u stop
  with Some start , Some stop -> Some (start,stop) | _ -> None

(* -------------------------------------------------------------------------- *)
(* --- Document Synchronization                                           --- *)
(* -------------------------------------------------------------------------- *)

let dopen = Queue.create ()
let dclose = Queue.create ()
let dsave = Queue.create ()
let dupdate = Queue.create ()

let on_open = register dopen
let on_close = register dclose
let on_save = register dsave
let on_update = register dupdate

let () = Lsp.register "textDocument/didOpen"
    begin fun channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      let lang = Json.jfield "languageId" doc |> Json.jstring in
      let text = Json.jfield "text" doc |> Json.jstring |> Ropes.of_string in
      let doc = Doc { uri ; lang ; text } in
      Hashtbl.replace documents uri doc ;
      notify dopen channel doc ;
      `Null
    end

let () = Lsp.register "textDocument/didClose"
    begin fun channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      begin
        try
          let doc = Hashtbl.find documents uri in
          Hashtbl.remove documents uri ;
          notify dclose channel doc ;
        with Not_found -> ()
      end ; `Null
    end

let () = Lsp.register "textDocument/didChange"
    begin fun channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      let changes = Json.jfield "contentChanges" data |> Json.jmap jchange in
      let doc = Hashtbl.find documents uri in
      let stack = ref [] in
      let push d u =
        let d' = apply d u in
        let Doc { text = before } = d in
        let Doc { text = after } = d' in
        let upd = { before ; change = u ; after } in
        stack := upd :: !stack ; d' in
      let newDoc = List.fold_left push doc changes in
      Hashtbl.replace documents uri newDoc ;
      let updates = List.rev !stack in
      Queue.iter (fun f -> List.iter (f channel ~uri) updates) dupdate ;
      `Null
    end

let () = Lsp.register "textDocument/didSave"
    begin fun channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      begin
        try
          let doc = Hashtbl.find documents uri in
          notify dsave channel doc ;
        with Not_found -> ()
      end ; `Null
    end

(* -------------------------------------------------------------------------- *)
(* ---  Goto Definition                                                   --- *)
(* -------------------------------------------------------------------------- *)

type defkind = [ `Def | `Typedef | `Implementation ]

let on_definition ?(kind:defkind = `Def) fn =
  let provider, meth =
    match kind with
    | `Def -> definitionProvider, "textDocument/definition"
    | `Typedef -> typedefProvider, "textDocument/typeDefinition"
    | `Implementation -> implementationProvider, "textDocument/implementation"
  in
  provider := true ;
  Lsp.register meth
    begin fun channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      let pos = Json.jfield "position" data |> jposition in
      try
        match fn channel ~uri pos with
        | [] -> `Null
        | [p] -> wlocation p
        | ps -> `List (List.map wlocation ps)
      with Exit | Not_found -> `Null
    end

(* -------------------------------------------------------------------------- *)
(* ---  Hover                                                             --- *)
(* -------------------------------------------------------------------------- *)

type markup = [ `Plaintext | `Markdown ]
type hover = Hover of {
    range : Range.range option ;
    kind : markup ;
    value : string list ;
  }

let wmarkup = function
  | `Plaintext -> "plaintext"
  | `Markdown -> "markdown"

let whover (Hover h) : Json.t = Json.assoc [
    "contents", Json.assoc [
      "kind", `String (wmarkup h.kind);
      "value", `String (String.concat "\n" h.value);
    ] ;
    "range", Json.option_map wrange h.range ;
  ]

let is_empty s = String.length @@ String.trim s = 0

let code ?(lang="") lines =
  if List.for_all is_empty lines then [] else ["```" ^ lang] @ lines @ ["```"]

let hover ?range ?(kind=`Plaintext) value = Hover { range ; kind ; value }

let on_hover fn =
  hoverProvider := true ;
  Lsp.register "textDocument/hover"
    begin fun channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      let pos = Json.jfield "position" data |> jposition in
      try whover @@ fn channel ~uri pos
      with Exit | Not_found -> `Null
    end

(* -------------------------------------------------------------------------- *)
(* ---  Commands                                                          --- *)
(* -------------------------------------------------------------------------- *)

type command = Cmd of {
    title: string ;
    command: string ;
    args: Json.t list ;
  }

let wcommand = function Cmd { title ; command ; args } ->
  Json.assoc [
    "title", `String title ;
    "command", `String command ;
    "arguments", Json.list args ;
  ]

(* -------------------------------------------------------------------------- *)
(* ---  Code Lenses                                                       --- *)
(* -------------------------------------------------------------------------- *)

type codelens =
  | Resolved of Range.pos * command
  | ToResolve of Range.pos * Json.t

let wrangepos p = wrange (p,p)

let wcodelens = function
  | Resolved(p,cmd) ->
    Json.assoc [ "range", wrangepos p ; "command", wcommand cmd ]
  | ToResolve(p,data) ->
    Json.assoc [ "range", wrangepos p ; "data", data ]

let resolved pos cmd = Resolved(pos,cmd)
let codelens pos data = ToResolve(pos,data)

let on_codelens ?resolve fn =
  codeLensProvider := true ;
  Lsp.register "textDocument/codeLens"
    begin fun channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      try Json.list_map wcodelens @@ fn channel ~uri
      with Exit | Not_found -> `Null
    end ;
  Option.iter
    begin fun fn ->
      Lsp.register "codeLens/resolve"
        begin fun _ param ->
          let pos =
            Json.jfield "range" param |>
            Json.jfield "start" |> jposition in
          let data = Json.jfield "data" param in
          wcodelens @@
          try Resolved(pos,fn pos data)
          with Exit | Not_found -> ToResolve(pos,data)
        end
    end resolve

let refresh_codelens channel =
  if !codeLensRefresh then
    Lsp.send channel "workspace/codeLens/refresh" `Null

(* -------------------------------------------------------------------------- *)
(* ---  Folding Ranges                                                    --- *)
(* -------------------------------------------------------------------------- *)

type folding = Folding of {
    range : Range.range ;
    kind : string option ;
    text : string option ;
  }

let folding ~range ?kind ?text () =
  Folding { range ; kind ; text }

let wfolding = function Folding { range=(start,stop) ; kind ; text } ->
  Json.assoc [
    "startLine", `Int start.line ;
    "startCharacter", `Int start.col ;
    "endLine", `Int stop.line ;
    "endCharacter", `Int stop.col ;
    "kind", Json.option_map Json.string kind ;
    "collapsedText", Json.option_map Json.string text ;
  ]

let on_folding fn =
  foldingRangeProvider := true ;
  Lsp.register "textDocument/foldingRange"
    begin fun channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      try Json.list_map wfolding @@ fn channel ~uri
      with Exit | Not_found -> `Null
    end

let refresh_folding channel =
  if !foldingRangeRefresh then
    Lsp.send channel "workspace/foldingRange/refresh" `Null

(* -------------------------------------------------------------------------- *)
(* ---  Document Symbols                                                  --- *)
(* -------------------------------------------------------------------------- *)

type kind = [
  | `File | `Module | `Namespace | `Package | `Class
  | `Method | `Property | `Field | `Constructor | `Enum | `Interface
  | `Function | `Variable | `Constant
  | `String | `Number | `Boolean | `Array | `Object | `Key
  | `Null | `EnumMember | `Struct
  | `Event | `Operator| `TypeParameter
]

let ikind (kind : kind) =
  match kind with
  | `File -> 1 | `Module -> 2 | `Namespace -> 3 | `Package -> 4 | `Class -> 5
  | `Method -> 6 | `Property -> 7 | `Field -> 8 | `Constructor -> 9
  | `Enum -> 10 | `Interface -> 11 | `Function -> 12 | `Variable -> 13
  | `Constant -> 14 | `String -> 15 | `Number -> 16 | `Boolean -> 17
  | `Array -> 18 | `Object -> 19 | `Key -> 20 | `Null -> 21 | `EnumMember -> 22
  | `Struct -> 23 | `Event -> 24 | `Operator -> 25 | `TypeParameter -> 26

type symbol = Symbol of {
    name : string ;
    kind : kind ;
    range : Range.range ;
    selection : Range.range ;
    children : symbol list ;
  }

let symbol ~name ~kind ~range ?(selection=range) ?(children=[]) () =
  Symbol { name ; kind ; range ; selection ; children }

let rec wsymbol = function Symbol s ->
  Json.assoc [
    "name", `String s.name ;
    "kind", `Int (ikind s.kind) ;
    "range", wrange s.range ;
    "selectionRange", wrange s.selection ;
    "children", Json.list_map wsymbol s.children ;
  ]

let on_document_symbol fn =
  documentSymbolProvider := true ;
  Lsp.register "textDocument/documentSymbol"
    begin fun channel data ->
      let doc = Json.jfield "textDocument" data in
      let uri = Json.jfield "uri" doc |> Json.jstring in
      try Json.list_map wsymbol @@ fn channel ~uri
      with Exit | Not_found -> `Null
    end

(* -------------------------------------------------------------------------- *)
(* ---  Diagnostics                                                       --- *)
(* -------------------------------------------------------------------------- *)

type severity = [ `Error | `Warning | `Information | `Hint ]

type related = Info of {
    location : location ;
    message : string ;
  }

type diagnostic = Diag of {
    range : Range.range ;
    severity : severity ;
    source : string option ;
    message : string ;
    related : related list ;
    data : Json.t ;
  }

let related ~location ~message = Info { location ; message }

let diagnostic
    ~range ?(severity=`Error) ?source ~message ?(related=[]) ?(data=`Null) () =
  Diag { range ; severity ; source ; message ; related ; data }

let wseverity (s : severity) : Json.t =
  match s with
  | `Error -> `Int 1
  | `Warning -> `Int 2
  | `Information -> `Int 3
  | `Hint -> `Int 4

let wrelated (Info i) =
  Json.assoc [
    "location", wlocation i.location ;
    "message", `String i.message ;
  ]

let wdiagnostic (Diag d) =
  Json.assoc [
    "range", wrange d.range ;
    "severity", wseverity d.severity ;
    "source", Json.option_map Json.string d.source ;
    "message", `String d.message ;
    "relatedInformation", Json.list_map wrelated d.related ;
    "data", d.data ;
  ]

let publish_diagnostic channel ~uri (ds : diagnostic list) =
  Lsp.notify channel "textDocument/publishDiagnostics" @@
  `Assoc [
    "uri", `String uri ;
    "diagnostics", `List (List.map wdiagnostic ds) ;
  ]

(* -------------------------------------------------------------------------- *)
(* ---  File Watchers                                                     --- *)
(* -------------------------------------------------------------------------- *)

type event = [ `Created | `Changed | `Deleted ]
type file_event = string * event

let devents = Queue.create ()
let on_file_event fn = register devents (fun ch (uri,evt) -> fn ch ~uri evt)

let wevent (js : Json.t) : event =
  match js with
  | `Int 1 -> `Created
  | `Int 2 -> `Changed
  | `Int 3 -> `Deleted
  | _ -> `Changed (* fallback *)

let wchange (js : Json.t) : file_event option =
  try
    let uri = Json.jfield "uri" js |> Json.jstring in
    let evt = Json.jfield "type" js |> wevent in
    Some (uri,evt)
  with Invalid_argument _ -> None

let () = Lsp.register "workspace/didChangeWatchedFiles"
    begin fun channel data ->
      let changes =
        Json.jfield "changes" data
        |> Json.jlist
        |> List.filter_map wchange
      in notify_all devents channel changes ;
      `Null
    end

let wevents (evts : event list) : Json.t =
  let m = List.fold_left
      (fun m evt ->
         match evt with
         | `Created -> m lor 1
         | `Changed -> m lor 2
         | `Deleted -> m lor 4
      ) 0 evts
  in `Int m

let wpattern uri pattern =
  match uri with
  | None -> `String pattern
  | Some base -> Json.assoc [
      "baseUri", `String base;
      "pattern",`String pattern;
    ]

let watcher events uri pattern = Json.assoc [
    "globPattern", wpattern uri pattern ;
    "kind", Json.option_map wevents events ;
  ]

let watchers events uri patterns : Json.t =
  `Assoc [ "watchers" , Json.list_map (watcher events uri) patterns ]

let watch channel ?events ?uri patterns =
  Lsp.register_capability channel
    "workspace/didChangeWatchedFiles" @@ watchers events uri patterns

(* -------------------------------------------------------------------------- *)
(* ---  Command Provider                                                  --- *)
(* -------------------------------------------------------------------------- *)

let commandProvider = Hashtbl.create 0

let () = Lsp.register "workspace/executeCommand"
    begin fun _channel data ->
      let command = Json.jfield "command" data |> Json.jstring in
      if command = "" then `Null else
        match Hashtbl.find_opt commandProvider command with
        | None ->
          invalid_arg @@ Printf.sprintf "Unsupported command %S" command
        | Some fn ->
          let args = Json.jfield "arguments" data |> Json.jlist in
          fn args
    end

let on_execute_command command handler =
  if Hashtbl.mem commandProvider command then
    invalid_arg @@ Printf.sprintf "Lspserver.on_execute_command (%s)" command ;
  Hashtbl.add commandProvider command handler

(* -------------------------------------------------------------------------- *)
