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

let dloc_of_goal goal =
  let file = Session.goal_file goal in
  let context = Session.goal_context goal in
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
      Option.map dloc_of_position @@ Session.goal_loc goal

(* -------------------------------------------------------------------------- *)
(* ---  BreakPoints                                                       --- *)
(* -------------------------------------------------------------------------- *)

type breakpoint = Brk of {
    id : int ;
    file : string ;
    line : int ;
    mutable active : bool ;
  }

let breakpointk = ref 0
let breakpoints : (string,breakpoint) Hashtbl.t = Hashtbl.create 0
let clear_breakpoints () = breakpointk := 0 ; Hashtbl.clear breakpoints

let add_breakpoint ~file ~line =
  let id = incr breakpointk ; !breakpointk in
  let brk = Brk { id ; file ; line ; active = true } in
  Hashtbl.add breakpoints file brk ; id

let () = Dap.on_breakpoint ~clear:clear_breakpoints
    begin function Dap.SourceBreakpoint { file ; line } ->
      let id = add_breakpoint ~file ~line in
      Dap.pending ~id ~message:"Registered breakpoint" ()
    end

(* -------------------------------------------------------------------------- *)
(* ---  Roots                                                             --- *)
(* -------------------------------------------------------------------------- *)

let roots : Session.goal Fibers.Queue.t = Fibers.Queue.create ()
let stops : Dap.reason Session.hmap = Session.hmap 0
let cursor : (Session.goal * Dap.reason) option ref = ref None

let forward () =
  let pending g = (Session.goal_proof g <> None) in
  let stopped g r = cursor := Some (g,r) ; raise Exit in
  try
    Fibers.Queue.filter
      (fun root ->
         Session.iter
           (fun g ->
              Option.iter (stopped g) @@ Session.find_opt stops g ;
              if pending g then stopped g `pause ;
           ) root ;
         true
      ) roots
  with Exit -> ()

(* -------------------------------------------------------------------------- *)
(* ---  Stack Frames                                                      --- *)
(* -------------------------------------------------------------------------- *)

let goal_attr : Session.goal Dap.attribute = Dap.attribute ()

let rec goal_frames g =
  let name = Session.goal_idename g in
  let range = dloc_of_goal g in
  let values = [Dap.value goal_attr g] in
  Dap.frame ~name ?range ~values () ::
  match Session.parent g with None -> [] | Some g -> goal_frames g

let frames () : Dap.frame Darray.t =
  match !cursor with
  | None -> Darray.empty
  | Some (g,_) -> Darray.list @@ goal_frames g

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
        let context = Session.goal_context goal in
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
        let task = Session.goal_task goal in
        let line = String.make 31 '-' in
        let text =
          Format.asprintf
            "%a%s Proof %s@\n@\n%t@\n@\n%s-------%s@."
            Why3.Pretty.print_sequent task
            line line
            begin fun fmt ->
              match Session.children goal with
              | Some (tactic, children) ->
                Format.fprintf fmt "(* %a %t *)@\n(* %a *)"
                  Tactic.pretty tactic
                  begin fun fmt ->
                    let n = List.length children in
                    if n = 0 then Format.fprintf fmt "(resolved)" else
                    let p = List.fold_left
                        (fun v g ->
                          match Session.goal_proof g with
                          | Some crc when Crc.is_complete crc -> succ v
                          | _ -> v
                        ) 0 children in
                    if p < n
                    then Format.fprintf fmt "(%d/%d)" p n
                    else Format.fprintf fmt "(%d)" n
                  end
                  Crc.pp_hints
                  (List.map (fun g ->
                       match Session.goal_proof g with
                       | None -> Crc.stuck
                       | Some crc -> crc
                     ) children)
              | None ->
                match Session.goal_proof goal with
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
        let h = Hammer.progress () in
        let message = Format.asprintf "%a%t" Hammer.pp_progress h Runner.pp_goals in
        Dap.update progress ~message ()
    end

let started adapter =
  begin
    let progress = Dap.progress adapter ~title:"Why3" ~message:"Started" () in
    let on_pause _thread = Hammer.pause () in
    let on_continue _thread = Hammer.resume () in
    let frames _thread = frames () in
    let thread = Dap.thread adapter ~name:"Why3find"
        ~frames ~on_continue ~on_pause () in
    session := Session { adapter ; progress ; thread } ;
  end

let paused () =
  match !session with
  | Terminated -> ()
  | Session { progress ; thread } ->
    begin
      if !cursor = None then forward () ;
      let reason = match !cursor with Some (_,r) -> r | None -> `pause in
      Dap.stopped ~reason thread ;
      Dap.update progress ~message:"Paused" () ;
    end

let terminated () =
  match !session with
  | Terminated -> ()
  | Session { progress } ->
    session := Terminated ;
    Dap.finished progress ~message:"Terminated" ()

(* -------------------------------------------------------------------------- *)
(* ---  Node Observers                                                    --- *)
(* -------------------------------------------------------------------------- *)

let on_failed node =
  begin
    let goal = Hammer.goal node in
    try
      Hammer.pause () ;
      cursor := Some (goal,`error) ;
      let gname = Session.goal_fullname goal in
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

(* -------------------------------------------------------------------------- *)
(* ---  Compilation                                                       --- *)
(* -------------------------------------------------------------------------- *)

let cc_proof hammer theory goal hint =
  let tname = Session.name theory in
  let gname = Session.goal_name goal in
  let name = Printf.sprintf "Goal %s.%s" tname gname in
  let result = Fibers.var () in
  let callback crc =
    begin
      Log.message "%s %a" name Crc.pretty crc ;
      Prove.merge_stats hint crc ;
      Fibers.set result crc ;
    end in
  let _node = Hammer.prove hammer ~callback goal hint in
  Fibers.Queue.push roots goal ; result

let cc_theory hammer buffer strategy gnames theory =
  let module M = Why3.Wstdlib.Mstr in
  let thy = Session.name theory in
  let goals = Session.split theory in
  let hints = M.find_def M.empty thy strategy in
  Proofs.init theory goals hints buffer ;
  let accept g =
    gnames = [] || List.mem thy gnames ||
    List.mem (thy ^ "." ^ Session.goal_name g) gnames in
  Fibers.all @@
  List.map
    (fun goal ->
       if not @@ accept goal then Fibers.return () else
         let gname = Session.goal_name goal in
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

let compile adapter =
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
        let config = Config.load_config ~root in
        let env = Project.create ~root ~config () in
        let callback _ =
          Log.message "Debugger Terminated." ;
          Dap.terminated adapter () in
        Fibers.background ~callback @@ Fibers.all @@
        List.map (cc_file adapter env !goals) files ;
        Hammer.resume ()
    end

(* -------------------------------------------------------------------------- *)
(* ---  DAP Life Cycle                                                    --- *)
(* -------------------------------------------------------------------------- *)

let () = Dap.on_init
    begin fun adapter ->
      let echo category pp = Dap.output adapter ~category "%t" pp in
      Log.pp_stdout := echo `stdout ;
      Log.pp_stderr := echo `stderr ;
      breakpointk := 0 ;
      Hashtbl.clear breakpoints ;
      Fibers.Queue.clear roots ;
      Session.clear stops ;
      Hammer.pause () ;
      Fibers.on Hammer.failed on_failed ;
      Dap.initialized adapter ;
    end

let () = Dap.on_launch
    begin fun adapter ->
      Dap.output adapter "Why3 Debugger Started.@." ;
      started adapter ;
      compile adapter ;
    end

let () = Dap.on_terminate
    begin fun adapter ~restart ->
      ignore adapter ;
      ignore restart ;
      Hammer.stop () ;
    end

(* -------------------------------------------------------------------------- *)
(* ---  Debugger Processing                                               --- *)
(* -------------------------------------------------------------------------- *)

let running adapter =
  Dap.running adapter || Hammer.pending ()

let process adapter =
  let h = Hammer.process () in
  let d = Dap.process adapter in
  h || d

let run adapter =
  let last = ref @@ Hammer.status () in
  while running adapter do
    let busy = process adapter in
    let status = Hammer.status () in
    if status <> !last then
      begin
        last := status ;
        match status with
        | Paused -> paused ()
        | Stopped | Complete -> terminated ()
        | Proving | Pausing | Stopping -> update ()
      end ;
    if not busy then Unix.sleepf 0.01 ;
  done ;
  Client.terminate ()

(* -------------------------------------------------------------------------- *)
