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

let pp_goal_location fmt goal =
  match Session.target goal with
  | None -> ()
  | Some loc ->
    let file,line,_,_,_ = Why3.Loc.get loc in
    Format.fprintf fmt "%s:%d: " file line

let dloc_of_position (p : Why3.Loc.position) : Dap.location =
  let file,l1,c1,l2,c2 = Why3.Loc.get p in
  Dap.loc ~file ~line:(l1-1) ~column:c1 ~end_line:(l2-1) ~end_column:c2 ()

let dloc_of_range ~file ((p,q) : Range.range) : Dap.location =
  Dap.loc ~file ~line:p.line ~column:p.col ~end_line:q.line ~end_column:q.col ()

let dloc_of_goal goal =
  let file = Session.file goal in
  let context = Session.context goal in
  let loc =
    List.find_map
      (function Vc.{ kind = Goal ; range } -> Some range | _ -> None)
      context.chunks in
  match loc with
  | Some range -> Some (dloc_of_range ~file range)
  | None ->
    let line,end_line = context.goal in
    if 0 <= line && line <= end_line && end_line < max_int then
      Some (Dap.loc ~file ~line ~end_line ())
    else
      Option.map dloc_of_position @@ Session.target goal

(* -------------------------------------------------------------------------- *)
(* ---  Cursor & Breakpoints Data                                          --- *)
(* -------------------------------------------------------------------------- *)

type cursor = NoCursor | Start | End | At of Session.goal

type breakpoint = Brk of {
    id : int ;
    file : string ;
    line : int ;
    mutable verified : bool ;
  }

let breakpointk : int ref = ref 0
let breakpoints : breakpoint list ref = ref []

let rootk : int ref = ref 0
let roots : Session.goal array ref = ref [| |]
let cursor : cursor ref = ref NoCursor

let clear () =
  begin
    breakpointk := 0 ;
    breakpoints := [] ;
    rootk := 0 ;
    roots := [| |] ;
    cursor := NoCursor ;
  end

(* -------------------------------------------------------------------------- *)
(* ---  BreakPoints                                                       --- *)
(* -------------------------------------------------------------------------- *)

let clear_breakpoints () =
  begin
    breakpointk := 0 ;
    breakpoints := [] ;
  end

let add_breakpoint ~file ~line ~verified =
  let id = incr breakpointk ; !breakpointk in
  let brk = Brk { id ; file ; line ; verified } in
  breakpoints := brk :: !breakpoints ; id

let () = Dap.on_breakpoint ~clear:clear_breakpoints
    begin function Dap.SourceBreakpoint { file ; line } ->
      if Array.length !roots = 0 then
        let id = add_breakpoint ~file ~line ~verified:false in
        Dap.pending ~id ~message:"Registered breakpoint" ()
      else
      if Array.exists
          (fun root ->
             let c = Session.context root in
             c.file = file &&
             let (p,q) = c.goal in p <= line && line <= q
          ) !roots
      then
        let id = add_breakpoint ~file ~line ~verified:true in
        Dap.verified ~id ~file ~line ()
      else
        Dap.failed ~message:"No related proof goals" ()
    end

let target g =
  match Session.target g with
  | None -> None
  | Some loc ->
    let file,p,_,q,_ = Why3.Loc.get loc in
    Some(file,p-1,q-1)

let hit (file,p,q) (Brk brk) =
  brk.file = file && p <= brk.line && brk.line <= q

let breakpoint_hit g =
  0 < !breakpointk &&
  match target g with
  | None -> false
  | Some rg -> List.exists (hit rg) !breakpoints

let breakpoint_hits g =
  if 0 = !breakpointk then [] else
    match target g with
    | None -> []
    | Some rg -> List.filter (hit rg) !breakpoints

(* -------------------------------------------------------------------------- *)
(* ---  Cursor Management                                                 --- *)
(* -------------------------------------------------------------------------- *)

let root_next () =
  if succ !rootk < Array.length !roots then
    (incr rootk ; Some !roots.(!rootk))
  else None

let root_prev () =
  if 0 < !rootk then (decr rootk ; Some !roots.(!rootk))
  else None

let step_next g =
  match Session.next g with Some _ as c -> c | None -> root_next ()

let step_prev g =
  match Session.prev g with Some _ as c -> c | None -> root_prev ()

(* Move to first incomplete or failed node, starting from g, included *)
let rec forward g =
  if breakpoint_hit g then Some g else
    match Session.proof g with
    | Some crc when Crc.is_complete crc -> Option.bind (step_next g) forward
    | _ -> match Session.children g with c::_ -> forward c | [] -> Some g

(* Move to last incomplete or failed node, starting from g, included *)
let rec backward g =
  if breakpoint_hit g then Some g else
    match Session.proof g with
    | Some crc when Crc.is_complete crc -> Option.bind (step_prev g) backward
    | _ -> match List.rev (Session.children g) with c::_ -> backward c | [] -> Some g

(* -------------------------------------------------------------------------- *)
(* ---  Task Frame                                                        --- *)
(* -------------------------------------------------------------------------- *)

type frame = {
  vars : Dap.variable Darray.buffer ;
  hyps : Dap.variable Darray.buffer ;
  goal : Dap.variable Darray.buffer ;
}

let populate (f : frame) goal =
  begin
    let task = Session.task goal in
    List.iter
      (fun (d : Why3.Decl.decl) ->
         match d.d_node with
         | Dtype _ | Ddata _ | Dlogic _ | Dind _ -> ()
         | Dparam ls ->
           let name = Format.asprintf "%a" Why3.Pretty.print_ls ls in
           let value = Format.asprintf "@[<hov 2>%t@]"
               begin fun fmt ->
                 List.iter
                   (Format.fprintf fmt "(%a) ->@ " Why3.Pretty.print_ty)
                   ls.ls_args ;
                 match ls.ls_value with
                 | None -> Format.fprintf fmt "Prop"
                 | Some tr -> Why3.Pretty.print_ty fmt tr
               end in
           Darray.add f.vars @@ Dap.variable ~name ~value ()
         | Dprop (kind,pr,term) ->
           let name = Format.asprintf "%a" Why3.Pretty.print_pr pr in
           let value = Format.asprintf "%a" Why3.Pretty.print_term term in
           let scope =
             match kind with Plemma | Paxiom -> f.hyps | Pgoal -> f.goal in
           Darray.add scope @@ Dap.variable ~name ~value ()
      ) @@ Why3.Task.(local_decls task @@ used_symbols @@ used_theories task) ;
    let status = Format.asprintf "%a" Session.pp_status goal in
    Darray.add f.goal @@ Dap.variable ~name:"Status" ~value:status () ;
  end

(* -------------------------------------------------------------------------- *)
(* ---  Stack Frames                                                      --- *)
(* -------------------------------------------------------------------------- *)

let goal_attr : Session.goal Dap.attribute = Dap.attribute ()

let rec goal_frames g =
  let name = Session.fullname g in
  let range = dloc_of_goal g in
  let values = [Dap.value goal_attr g] in
  let fr = {
    vars = Darray.create () ;
    hyps = Darray.create () ;
    goal = Darray.create () ;
  } in
  let compute = lazy (populate fr g) in
  let scope buffer = Lazy.force compute ; Darray.contents buffer in
  Dap.frame ~name ?range ~values ~scopes:[
    Dap.scope ~name:"Variables" ~named:(Darray.delayed scope fr.vars) () ;
    Dap.scope ~name:"Context" ~named:(Darray.delayed scope fr.hyps) () ;
    Dap.scope ~name:"Goal" ~named:(Darray.delayed scope fr.goal) () ;
  ] () ::
  match Session.parent g with None -> [] | Some g -> goal_frames g

let frames _thread : Dap.frame Darray.t =
  match !cursor with
  | NoCursor | Start | End -> Darray.empty
  | At g -> Darray.list @@ goal_frames g

(* -------------------------------------------------------------------------- *)
(* ---  Proof Context                                                     --- *)
(* -------------------------------------------------------------------------- *)

let jchunk ~file (ch : Vc.chunk) =
  let jkind =
    match ch.kind with Goal -> "goal" | When -> "hpos" | WhenNot -> "hneg" in
  Json.assoc @@
  ("kind", `String jkind) ::
  (Dap.js_rangefields @@ dloc_of_range ~file ch.range)

let () = Dap.register "why3/context"
    begin fun _adapter args ->
      let frameId = Json.jfield "frameId" args |> Json.jint in
      match Dap.get_by_frameId goal_attr frameId with
      | None -> `Null
      | Some goal ->
        let context = Session.context goal in
        let file = context.file in
        Json.assoc [
          "source" , `String file ;
          "chunks" , Json.list_map (jchunk ~file) context.chunks ;
        ]
    end

let () = Dap.register "why3/task"
    begin fun _adapter args ->
      let frameId = Json.jfield "frameId" args |> Json.jint in
      match Dap.get_by_frameId goal_attr frameId with
      | None -> `Null
      | Some goal ->
        let task = Session.task goal in
        let line = String.make 31 '-' in
        let text =
          Format.asprintf
            "%a%s Proof %s@\n@\n%t@\n@\n%s-------%s@."
            Why3.Pretty.print_sequent task
            line line
            begin fun fmt ->
              match Session.tactic goal with
              | Some tactic ->
                let children = Session.children goal in
                Format.fprintf fmt "(* %a %t *)@\n(* %a *)"
                  Tactic.pretty tactic
                  begin fun fmt ->
                    let n = List.length children in
                    if n = 0 then Format.fprintf fmt "(resolved)" else
                      let p = List.fold_left
                          (fun v g ->
                             match Session.proof g with
                             | Some crc when Crc.is_complete crc -> succ v
                             | _ -> v
                          ) 0 children in
                      if p < n
                      then Format.fprintf fmt "(%d/%d)" p n
                      else Format.fprintf fmt "(%d)" n
                  end
                  Crc.pp_hints
                  (List.map (fun g ->
                       match Session.proof g with
                       | None -> Crc.stuck
                       | Some crc -> crc
                     ) children)
              | None ->
                match Session.proof goal with
                | None ->
                  Format.fprintf fmt "(* Pending *)"
                | Some crc when Crc.is_stuck crc ->
                  Format.fprintf fmt "(* Failed *)"
                | Some crc ->
                  Format.fprintf fmt "(* %a *)" Crc.pp_hint crc
            end
            line line
        in `String text
    end

(* -------------------------------------------------------------------------- *)
(* ---  Running State                                                     --- *)
(* -------------------------------------------------------------------------- *)

type session =
  | Terminated
  | Session of {
      adapter : Dap.adapter ;
      progress : Dap.progress ;
      thread : Dap.thread ;
    }

let session = ref Terminated

let update = Timer.throttle
    begin fun () ->
      match !session with
      | Terminated -> ()
      | Session { progress } ->
        let pp_progress fmt =
          Hammer.pp_progress fmt @@ Hammer.progress () ; Runner.pp_goals fmt
        in Dap.update progress ~message:(Format.asprintf "%t" pp_progress) ()
    end

let stopped thread =
  match !cursor with
  | NoCursor | Start | End -> Dap.stopped ~reason:`pause thread
  | At goal ->
    let hits = breakpoint_hits goal in
    if hits <> [] then
      let breakpoints = List.map (fun (Brk b) -> b.id) hits in
      Dap.stopped ~reason:`breakpoint ~breakpoints thread
    else
      let reason =
        match Session.proof goal with
        | None -> `pause
        | Some crc -> if Crc.is_stuck crc then `error else `pause in
      Dap.stopped ~reason thread

let paused () =
  match !session with
  | Terminated -> ()
  | Session { progress ; thread } ->
    begin
      Dap.update progress ~message:"Paused" () ;
      stopped thread ;
    end

let complete () =
  match !session with
  | Terminated -> ()
  | Session { adapter ; progress ; thread } ->
    Dap.finished progress ~message:"Terminated" () ;
    if !cursor = NoCursor then
      begin
        clear () ; Dap.terminated adapter ()
      end
    else stopped thread

(* -------------------------------------------------------------------------- *)
(* ---  Stepping                                                          --- *)
(* -------------------------------------------------------------------------- *)

let hammer = ref Hammer.Proving
let resume ?goal () =
  hammer := Hammer.Proving ;
  let filter =
    Option.map
      (fun g n -> Session.is_parent g (Hammer.goal n))
      goal
  in Hammer.resume ?filter ()
let setup_cursor () =
  if !cursor = NoCursor then
    cursor := if Array.length !roots > 0 then Start else End

let move_end g = cursor := match g with None -> End | Some g -> At g
let move_start g = cursor := match g with None -> Start | Some g -> At g
let advance () =
  match !cursor with
  | NoCursor | End -> ()
  | At g -> move_end @@ forward g
  | Start -> rootk := 0 ; move_end @@ forward !roots.(0)

let breakpoint () =
  match !cursor with
  | Start | End | NoCursor -> ()
  | At g ->
    match Session.proof g with
    | None -> ()
    | Some crc -> if Crc.is_stuck crc then Hammer.pause ()

let () = Dap.on_pause
    begin fun _thread ->
      setup_cursor () ;
      Hammer.pause () ;
    end

let () = Dap.on_next
    begin fun _thread ->
      match !cursor with
      | NoCursor | End -> paused ()
      | Start -> rootk := 0 ; cursor := At !roots.(0) ; paused ()
      | At g ->
        match Session.proof g with
        | Some _ -> move_end @@ step_next g ; paused ()
        | None -> resume ~goal:g ()
    end

let () = Dap.on_step_in
    begin fun _thread ->
      match !cursor with
      | NoCursor | End -> paused ()
      | Start -> rootk := 0 ; cursor := At !roots.(0) ; paused ()
      | At g ->
        match Session.children g with
        | g::_ -> cursor := At g ; paused ()
        | [] ->
          match Session.proof g with
          | Some _ -> move_end @@ step_next g ; paused ()
          | None -> resume ~goal:g ()
    end

let () = Dap.on_step_out
    begin fun _thread ->
      match !cursor with
      | NoCursor | Start | End -> paused ()
      | At g ->
        let p = Option.value ~default:g (Session.parent g) in
        match Session.proof p with
        | Some _ -> cursor := At p ; paused ()
        | None -> resume ~goal:p ()
    end

let () = Dap.on_step_back
    begin fun _thread ->
      match !cursor with
      | NoCursor | Start -> paused ()
      | End ->
        let n = Array.length !roots in
        if n > 0 then
          begin
            rootk := n-1 ;
            cursor := At !roots.(n-1) ;
          end ;
        paused ()
      | At g -> move_start @@ step_prev g ; paused ()
    end

let () = Dap.on_continue
    begin fun _thread ->
      match !cursor with
      | NoCursor | End -> resume ()
      | Start ->
        rootk := 0 ; move_end @@ forward !roots.(0) ; paused ()
      | At g ->
        match Session.proof g with
        | None -> resume ()
        | Some _ -> move_end @@ forward g ; paused ()
    end

let () = Dap.on_reverse_continue
    begin fun _thread ->
      match !cursor with
      | NoCursor | Start -> paused ()
      | End ->
        let n = Array.length !roots in
        if n > 0 then
          begin
            rootk := n-1 ;
            move_start @@ backward !roots.(n-1) ;
          end ;
        paused ()
      | At g ->
        match Session.proof g with
        | None -> paused ()
        | Some _ -> move_start (backward g) ; paused ()
    end

(* -------------------------------------------------------------------------- *)
(* ---  Node Observers                                                    --- *)
(* -------------------------------------------------------------------------- *)

let on_failed node =
  begin
    let goal = Hammer.goal node in
    setup_cursor () ;
    advance () ;
    breakpoint () ;
    try
      let gname = Session.fullname goal in
      Log.error "%a%s @{<red>failed@}" pp_goal_location goal gname ;
      match !session with
      | Terminated -> ()
      | Session { adapter } ->
        Option.iter
          begin fun loc ->
            Dap.emit adapter "why3/failed" @@
            Json.assoc (("goal", `String gname) :: Dap.js_sourcefields loc)
          end @@ dloc_of_goal @@ Hammer.goal node
    with Not_found ->
      Log.error "%aGoal @{<red>failed@}" pp_goal_location goal
  end

let on_enter node =
  let g = Hammer.goal node in
  match !cursor with
  | At p when Session.is_parent p g -> cursor := At g
  | _ -> ()

let on_leave node =
  let g = Hammer.goal node in
  match !cursor with
  | At g0 when g0 == g -> advance () ; breakpoint () ;
  | _ -> ()

(* -------------------------------------------------------------------------- *)
(* ---  Compilation                                                       --- *)
(* -------------------------------------------------------------------------- *)

let cc_proof hammer theory goal hint =
  let tname = Session.thy_name theory in
  let gname = Session.name goal in
  let name = Printf.sprintf "Goal %s.%s" tname gname in
  let result = Fibers.var () in
  let callback crc =
    begin
      Log.message "%s %a" name Crc.pretty crc ;
      Prove.merge_stats hint crc ;
      Fibers.set result crc ;
    end in
  let _node = Hammer.prove hammer ~callback goal hint in
  roots := Array.append !roots [| goal |] ; result

let cc_theory hammer buffer strategy gnames theory =
  let module M = Why3.Wstdlib.Mstr in
  let thy = Session.thy_name theory in
  let goals = Session.split theory in
  let hints = M.find_def M.empty thy strategy in
  Proofs.init theory goals hints buffer ;
  let accept g =
    gnames = [] || List.mem thy gnames ||
    List.mem (thy ^ "." ^ Session.name g) gnames in
  Fibers.all @@
  List.map
    (fun goal ->
       if not @@ accept goal then Fibers.return () else
         let gname = Session.name goal in
         let hint = M.find_def Crc.stuck gname hints in
         Fibers.map
           begin fun crc ->
             let complete = Hammer.completing () in
             let hint = Crc.update ~complete ~hint crc in
             Proofs.add theory goal hint buffer
           end @@
         cc_proof hammer theory goal hint
    ) goals

let hline = "--------------------------------------------------------"

let cc_file adapter env gnames file =
  try
    Dap.output adapter "Compiling %s@." file ;
    let dir, lib = Wutil.filepath file in
    let theories, format = Wutil.load_theories env.Project.why3.env file in
    let session = Session.create ~dir ~file ~format theories in
    let jprofile, strategy = Proofs.load file in
    let default = Calibration.default env in
    let profile = Calibration.of_json ~default jprofile in
    Prove.clear_stats () ;
    Runner.clear_stats () ;
    let buffer = Proofs.create ~file jprofile strategy in
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
      (cc_theory hammer buffer strategy gnames)
      (Session.theories session)
  with Why3.Loc.Located(loc,exn) ->
    let file,line,_,_,_ = Why3.Loc.get loc in
    Dap.output adapter ~category:`important
      "@{<red>Error:@} %s:%d:@\n%a@."
      file line Why3.Exn_printer.exn_printer exn ;
    Fibers.return ()

let files = ref []
let goals = ref []

(* -------------------------------------------------------------------------- *)
(* ---  DAP Life Cycle                                                    --- *)
(* -------------------------------------------------------------------------- *)

let () = Dap.on_init
    begin fun adapter ->
      let echo category pp = Dap.output adapter ~category "%t" pp in
      Log.pp_stdout := echo `stdout ;
      Log.pp_stderr := echo `stderr ;
      clear () ;
      Hammer.pause () ;
      hammer := Hammer.status () ;
      Fibers.on Hammer.enter on_enter ;
      Fibers.on Hammer.leave on_leave ;
      Fibers.on Hammer.failed on_failed ;
      Dap.initialized adapter ;
    end

let () = Dap.on_launch
    begin fun adapter ->
      Dap.output adapter "Why3 Debugger Started.@." ;
      let files = !files in
      if files = [] then
        begin
          Log.warning "No files to debug." ;
          Dap.exited adapter 1
        end
      else
        begin
          match List.find_map Config.root_for files with
          | None ->
            Log.warning "No config file found." ;
            Dap.exited adapter 1
          | Some root ->
            clear () ;
            let config = Config.load_config ~root in
            let env = Project.create ~root ~config () in
            Fibers.background @@ Fibers.all @@
            List.map (cc_file adapter env !goals) files ;
            let progress = Dap.progress adapter ~title:"Why3" () in
            let thread = Dap.thread adapter ~name:"Why3find" frames in
            session := Session { adapter ; progress ; thread } ;
            hammer := Hammer.status () ;
            Hammer.resume () ;
        end
    end

let () = Dap.on_terminate
    begin fun adapter ~restart ->
      ignore adapter ;
      ignore restart ;
      clear () ;
      Hammer.stop () ;
    end

(* -------------------------------------------------------------------------- *)
(* ---  Debugger Processing                                               --- *)
(* -------------------------------------------------------------------------- *)

let running adapter =
  !cursor <> NoCursor || Dap.running adapter || Hammer.pending ()

let process adapter =
  let h = Hammer.process () in
  let d = Dap.process adapter in
  h || d

let run adapter =
  while running adapter do
    let busy = process adapter in
    let status = Hammer.status () in
    if status <> !hammer then
      begin
        hammer := status ;
        match status with
        | Paused -> paused ()
        | Stopped | Complete -> complete ()
        | Proving | Pausing | Stopping -> update ()
      end ;
    if not busy then Unix.sleepf 0.01 ;
  done ;
  Client.terminate ()

(* -------------------------------------------------------------------------- *)
