(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* ---  Observing Nodes                                                   --- *)
(* -------------------------------------------------------------------------- *)

let anchor = Dap.attribute ()

let pp_goal_location fmt goal =
  match Session.goal_loc goal with
  | None -> ()
  | Some loc ->
    let file,line,_,_,_ = Why3.Loc.get loc in
    Format.fprintf fmt "%s:%d: " file line

let dloc_of_position (p : Why3.Loc.position) : Dap.location =
  let file,l1,c1,l2,c2 = Why3.Loc.get p in
  Dap.loc ~file ~line:(l1-1) ~column:c1 ~end_line:(l2-1) ~end_column:c2 ()

let dloc_of_range ~file ((p,q) : Range.range) : Dap.location =
  Dap.loc ~file ~line:p.line ~column:p.col ~end_line:q.line ~end_column:q.col ()

let dloc_of_goal g = Option.map dloc_of_position @@ Session.goal_loc g

let dloc_of_node n =
  let goal = Hammer.goal n in
  let file = Session.goal_file goal in
  let context = Hammer.context n in
  let loc =
    List.find_map
      (function Vc.{ kind = Goal ; range } -> Some range | _ -> None)
      context.chunks in
  match loc with
  | Some range -> Some (dloc_of_range ~file range)
  | None ->
    if 0 <= context.g1 && context.g1 <= context.g2 && context.g2 < max_int then
      Some (Dap.loc ~file ~line:context.g1 ~end_line:context.g2 ())
    else
      dloc_of_goal goal

(* -------------------------------------------------------------------------- *)
(* ---  Proof Tree Scope                                                  --- *)
(* -------------------------------------------------------------------------- *)

let rec is_stuck n =
  match Hammer.state n with
  | Paused | Scheduled -> false
  | Failed -> true
  | Closed crc -> Crc.is_stuck crc
  | Stacked(_,children) -> List.for_all is_stuck children

let add buffer ~name ~value ?vdecl ?index () =
  Darray.add buffer @@
  Dap.variable ~name ~value ?vdecl ?index ()

let add_vc buffer ~name ~file (chunk : Vc.chunk) =
  let value = Format.asprintf "@[<hov 2>%a@]"
      Why3.Pretty.print_term chunk.term in
  let vdecl = dloc_of_range ~file chunk.Vc.range in
  add buffer ~name ~value ~vdecl ()

let node_status n =
  match Hammer.state n with
  | Failed -> "Failed"
  | Paused | Scheduled -> "Paused"
  | Closed crc ->
    if Crc.is_stuck crc then "Failed" else
      Format.asprintf "%a" Crc.pp_crc crc
  | Stacked(tactic,children) ->
    Format.asprintf "[%a] %a"
      Tactic.pretty tactic
      Crc.pp_crc_all (List.map Hammer.proof children)

let node_index n =
  match Hammer.state n with
  | Paused | Scheduled | Failed | Closed _ -> None
  | Stacked(_,children) ->
    if List.for_all is_stuck children then None else Some children

let rec vc_vars ~goal ?(status=goal) n =
  let buffer = Darray.create () in
  let vc = Hammer.context n in
  let file = vc.file in
  List.iter
    (fun chunk ->
       match chunk.Vc.kind with
       | Goal when goal -> add_vc buffer ~file ~name:"Goal" chunk
       | When when not goal -> add_vc buffer ~file ~name:"Hyp" chunk
       | WhenNot when not goal -> add_vc buffer ~file ~name:"Not" chunk
       | _ -> ()
    ) vc.chunks ;
  begin
    if status then
      let value = node_status n in
      let index = Option.map (Darray.delayed index_vars) (node_index n) in
      add buffer ~name:"Status" ~value ?index () ;
  end ;
  Darray.contents buffer

and index_vars children =
  Darray.list @@
  List.mapi
    (fun i n ->
       let expl = Session.goal_expl @@ Hammer.goal n in
       let status = node_status n in
       let value = Printf.sprintf "%s %s" expl status in
       Dap.variable
         ~name:(string_of_int i) ~value
         ~named:(Darray.delayed (vc_vars ~status:false ~goal:true) n)
         ()
    ) children

let vc_scope ~goal n =
  Dap.scope
    ~name:(if goal then "Proof" else "Context")
    ~expensive:true
    ~named:(Darray.delayed (vc_vars ~goal) n) ()

(* -------------------------------------------------------------------------- *)
(* ---  Callstack                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec reach stack n =
  match Hammer.state n with
  | Closed _ -> None
  | Paused | Scheduled | Failed -> Some (n::stack)
  | Stacked(_, children) ->
    if List.for_all is_stuck children then Some (n::stack) else
      List.find_map (reach (n::stack)) children

let frame n =
  let range = dloc_of_node n in
  let name =
    match Hammer.state n with
    | Stacked(tactic,_) -> Tactic.name tactic
    | _ -> Session.goal_expl @@ Hammer.goal n in
  Dap.frame ~name ?range ~scopes:[
    vc_scope ~goal:false n ;
    vc_scope ~goal:true n ;
  ] ~values:[Dap.value anchor n] ()

let callstack n =
  match reach [] n with
  | None -> [frame n]
  | Some stk -> List.map frame stk

(* -------------------------------------------------------------------------- *)
(* ---  Observing Thread                                                  --- *)
(* -------------------------------------------------------------------------- *)

let frames thread =
  Darray.list @@
  match Dap.get anchor thread with
  | None -> []
  | Some n -> callstack n

(* -------------------------------------------------------------------------- *)
(* ---  Current Node                                                      --- *)
(* -------------------------------------------------------------------------- *)

let rec current n =
  match Hammer.state n with
  | Closed _ -> None
  | Paused | Scheduled | Failed -> Some n
  | Stacked(_,children) -> List.find_map current children

let rec context n =
  match Hammer.state n with
  | Closed _ -> None
  | Paused | Scheduled | Failed -> None
  | Stacked(_,children) ->
    match List.find_map context children with
    | None -> Some children
    | Some _ as result -> result

(* -------------------------------------------------------------------------- *)
(* ---  BreakPoints                                                       --- *)
(* -------------------------------------------------------------------------- *)

type breakpoint = Brk of {
    id : int ; line : int ;
    mutable active : bool ;
  }

let breakpointk = ref 0
let breakpoints = Hashtbl.create 0
let clear_breakpoints () = breakpointk := 0 ; Hashtbl.clear breakpoints

let add_breakpoint ~thread ~line =
  let th = Dap.id thread in
  let id = incr breakpointk ; !breakpointk in
  let brk = Brk { id ; line ; active = true } in
  Hashtbl.add breakpoints th brk ; id

let has_breakpoint ~line thread =
  match Dap.get anchor thread with
  | None -> false
  | Some node ->
    let vc = Hammer.chunks node in
    vc.h1 <= line && line <= vc.h2

let hit_breakpoints thread =
  match Dap.get anchor thread with
  | None -> []
  | Some n ->
    match current n with
    | None -> []
    | Some n ->
      let vc = Hammer.context n in
      List.filter_map
        (function Brk b ->
           if b.active && b.line <= vc.g2 then
             (b.active <- false ; Some b.id)
           else None)
        (Hashtbl.find_all breakpoints (Dap.id thread))

let () = Dap.on_breakpoint ~clear:clear_breakpoints
    begin function Dap.SourceBreakpoint { line } ->
      let threads = Dap.threads () in
      match List.find_opt (has_breakpoint ~line) threads with
      | Some thread -> Dap.verified ~id:(add_breakpoint ~thread ~line) ()
      | None -> Dap.failed ~message:"Out of range" ()
    end

(* -------------------------------------------------------------------------- *)
(* ---  Proof Context                                                     --- *)
(* -------------------------------------------------------------------------- *)

let jchunk ~file (ch : Vc.chunk) =
  let jkind =
    match ch.kind with Goal -> "goal" | When -> "hpos" | WhenNot -> "hneg" in
  Json.assoc @@
  ("kind", `String jkind) ::
  (Dap.js_rangefields @@ dloc_of_range ~file ch.range)

let frame_node ~threadId ~frameId =
  match Dap.get_by_frameId anchor frameId with
  | Some _ as result -> result
  | None -> Option.bind (Dap.get_by_threadId anchor threadId) current

let () = Dap.register "why3/context"
    begin fun _adapter args ->
      let threadId = Json.jfield "threadId" args |> Json.jint in
      let frameId = Json.jfield "frameId" args |> Json.jint in
      match frame_node ~threadId ~frameId  with
      | None -> `Null
      | Some node ->
        let context = Hammer.chunks node in
        let file = context.file in
        Json.assoc [
          "source" , `String file ;
          "chunks" , Json.list_map (jchunk ~file) context.chunks ;
        ]
    end

let () = Dap.register "why3/task"
    begin fun _adapter args ->
      let threadId = Json.jfield "threadId" args |> Json.jint in
      let frameId = Json.jfield "frameId" args |> Json.jint in
      match frame_node ~threadId ~frameId  with
      | None -> `Null
      | Some node ->
        let task = Session.goal_task @@ Hammer.goal node in
        let state = Hammer.state node in
        let line = "------------------------------" in
        let rec pp_stack fmt = function
          | Hammer.Thread _ -> ()
          | Hammer.Node n ->
            Format.fprintf fmt
              "@\nfrom %a" Hammer.pp_state (Hammer.state n) ;
            pp_stack fmt (Hammer.parent n)
        in
        let text =
          Format.asprintf
            "%a%s Proof  %s@\n@\nproof : %a%a@\n@\n%s--------%s@."
            Why3.Pretty.print_sequent task
            line line Hammer.pp_state state
            pp_stack (Hammer.parent node)
            line line
        in `String text
    end

(* -------------------------------------------------------------------------- *)
(* ---  Running State                                                     --- *)
(* -------------------------------------------------------------------------- *)

type state =
  | Paused
  | Running of { adapter : Dap.adapter ; progress : Dap.progress }

let state = ref Paused
let stopping = ref None

let update = Timer.throttle
    begin fun h ->
      match !state with
      | Paused -> ()
      | Running { progress } ->
        let message =
          Format.asprintf "%a%t" Hammer.pp_progress h Runner.pp_goals in
        Dap.update progress ~message ()
    end

let started adapter =
  let progress = Dap.progress adapter ~title:"Why3" ~message:"Started" () in
  state := Running { adapter ; progress }

let terminated () =
  match !state with
  | Paused -> ()
  | Running { progress } ->
    state := Paused ;
    Dap.finished progress ~message:"Terminated" ()

let breaked thread =
  match hit_breakpoints thread with
  | [] -> None
  | bks -> Some (thread,bks)

let paused () =
  match !state with
  | Paused -> ()
  | Running { progress } ->
    state := Paused ;
    Dap.finished progress ~message:"paused" () ;
    let stop = !stopping in
    stopping := None ;
    match stop with
    | Some(thread,reason) when Dap.living thread ->
      let breakpoints = hit_breakpoints thread in
      Dap.stopped ~reason ~breakpoints thread
    | _ ->
      let threads = Dap.threads () in
      match List.find_map breaked threads with
      | Some (thread,breakpoints) ->
        Dap.stopped ~reason:`breakpoint ~breakpoints thread
      | None ->
        match Dap.threads () with
        | thread::_ -> Dap.stopped ~reason:`pause thread
        | _ -> terminated ()

let observe () =
  match Hammer.status () with
  | Proving h -> update h
  | Paused -> paused ()
  | Exited -> terminated ()

let on_pause thread =
  stopping := Some(thread,`pause) ;
  Hammer.pause ()

let on_continue thread =
  stopping := Some(thread,`pause) ;
  started (Dap.on thread) ;
  Hammer.resume ()

let on_next thread =
  match Dap.get anchor thread with
  | None -> on_continue thread
  | Some n ->
    match current n with
    | None-> on_continue thread
    | Some n ->
      stopping := Some(thread,`step) ;
      started (Dap.on thread) ;
      Hammer.step_out [n]

let on_step_in thread =
  match Dap.get anchor thread with
  | None -> on_continue thread
  | Some n ->
    match current n with
    | None -> on_continue thread
    | Some n ->
      stopping := Some(thread,`step) ;
      started (Dap.on thread) ;
      Hammer.step_in [n]

let on_step_out thread =
  match Dap.get anchor thread with
  | None -> on_continue thread
  | Some n ->
    match context n with
    | None -> on_step_in thread
    | Some children ->
      stopping := Some(thread,`step) ;
      started (Dap.on thread) ;
      Hammer.step_out children

let on_filter node =
  let th = Hammer.thread_id node in
  let line =
    List.fold_left
      (fun l (Brk b) -> if b.active then min l b.line else l)
      max_int (Hashtbl.find_all breakpoints th) in
  line = max_int || let vc = Hammer.context node in vc.g2 <= line

let on_failed node =
  begin
    let goal = Hammer.goal node in
    try
      let id = Hammer.thread_id node in
      let gname = Session.goal_expl goal in
      let thread = Dap.find id in
      let name = Dap.name thread in
      stopping := Some (thread,`error) ;
      Hammer.pause () ;
      Log.error "%a%s @{<red>failed@}" pp_goal_location goal name ;
      Option.iter
        begin fun loc ->
          Dap.emit (Dap.on thread) "why3/failed" @@
          Json.assoc (("goal", `String gname) :: Dap.js_sourcefields loc)
        end @@ dloc_of_node node
    with Not_found ->
      Log.error "%aGoal @{<red>failed@}" pp_goal_location goal
  end

(* -------------------------------------------------------------------------- *)
(* ---  Compilation                                                       --- *)
(* -------------------------------------------------------------------------- *)

let cc_proof adapter hammer theory goal hint =
  let thy = Session.theory theory in
  let tname = Session.thy_name thy in
  let gname = Session.goal_name goal in
  let name = Printf.sprintf "Goal %s.%s" tname gname in
  let frames = Darray.delayed frames in
  let thread = Dap.thread adapter
      ~name ~frames
      ~on_continue ~on_pause
      ~on_next ~on_step_in ~on_step_out
      () in
  let result = Fibers.var () in
  let callback crc =
    begin
      Dap.terminate thread ;
      Log.message "%s %a" name Crc.pretty crc ;
      Prove.merge_stats hint crc ;
      Fibers.set result crc ;
    end in
  let node = Hammer.thread hammer ~id:(Dap.id thread) ~callback goal hint in
  Dap.set anchor thread node ; result

let cc_theory adapter hammer buffer strategy gnames theory =
  let module M = Why3.Wstdlib.Mstr in
  let thy = Session.name theory in
  let goals = Session.split theory in
  let hints = M.find_def M.empty thy strategy in
  Proofs.init theory goals hints buffer ;
  let accept g =
    gnames = [] || List.mem thy gnames ||
    List.mem (thy ^ "." ^ (Session.goal_name g)) gnames in
  Fibers.all @@
  List.map
    (fun goal ->
       if not @@ accept goal then Fibers.return () else
         let gname = Session.goal_name goal in
         let hint = try M.find gname hints with Not_found -> Crc.stuck () in
         Fibers.map
           begin fun crc ->
             let complete = not @@ Hammer.interrupting () in
             let hint = Crc.update ~complete ~hint crc in
             Proofs.add theory goal hint buffer
           end @@
         cc_proof adapter hammer theory goal hint
    ) goals

let hline = "--------------------------------------------------------"

let cc_file adapter env gnames file =
  try
    Dap.output adapter "Compiling %s@." file ;
    let dir, lib = Wutil.filepath file in
    let theories, format = Wutil.load_theories env.Project.why3.env file in
    let session = Session.create ~dir ~file ~format theories in
    let local = Calibration.default env in
    let profile, strategy = Proofs.load ~local file in
    Prove.clear_stats () ;
    Runner.clear_stats () ;
    let buffer = Proofs.create ~file profile strategy in
    let hammer = Hammer.create ~profile env in
    let callback _ =
      begin
        Proofs.save buffer ;
        Log.printf "%s@\nLibrary %s:@\n%t%t%s@."
          hline (String.concat "." lib)
          Runner.print_stats
          (Prove.print_stats (Hammer.provers hammer) (Hammer.tactics hammer))
          hline ;
      end in
    Fibers.map callback @@ Fibers.all @@
    List.map
      (cc_theory adapter hammer buffer strategy gnames)
      (Session.theories session)
  with Why3.Loc.Located(loc,exn) ->
    let file,line,_,_,_ = Why3.Loc.get loc in
    Dap.output adapter ~category:`important
      "@{<red>Error:@} %s:%d:@\n%a@."
      file line Why3.Exn_printer.exn_printer exn ;
    Fibers.return ()

(* -------------------------------------------------------------------------- *)
(* ---  Debugger Initialization                                           --- *)
(* -------------------------------------------------------------------------- *)

let init gnames files =
  begin
    Dap.on_init
      begin fun adapter ->
        let echo category pp = Dap.output adapter ~category "%t" pp in
        Log.pp_stdout := echo `stdout ;
        Log.pp_stderr := echo `stderr ;
        breakpointk := 0 ;
        Hashtbl.clear breakpoints ;
        Hammer.pause () ;
        if files = [] then Log.warning "No files to debug." else
          match List.find_map Config.root_for files with
          | None ->
            Log.warning "No config file found." ;
            Dap.exited adapter 1
          | Some root ->
            let config = Config.load_config ~root in
            let env = Project.create ~root ~config () in
            let callback _ =
              Log.message "All proofs terminated." ;
              Dap.terminated adapter () in
            Fibers.background ~callback @@ Fibers.all @@
            List.map (cc_file adapter env gnames) files ;
            Dap.initialized adapter
      end ;
    Dap.on_launch
      begin fun adapter ->
        Dap.output adapter "Why3 Debugger Started.@." ;
        started adapter ;
        Hammer.resume () ;
      end ;
    Dap.on_terminate
      begin fun adapter ~restart ->
        ignore adapter ;
        ignore restart ;
        Hammer.interrupt () ;
      end ;
    Hammer.on_failed on_failed ;
    Hammer.on_filter on_filter ;
  end

let running adapter =
  Dap.running adapter || Hammer.running ()

let process adapter =
  let h = Hammer.process () in
  let d = Dap.process adapter in
  h || d

let run adapter =
  while running adapter do
    let busy = process adapter in
    observe () ;
    if not busy then Unix.sleepf 0.01 ;
  done ;
  Client.terminate ()

(* -------------------------------------------------------------------------- *)
