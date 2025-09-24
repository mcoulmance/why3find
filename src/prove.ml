(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Proof Manager                                                      --- *)
(* -------------------------------------------------------------------------- *)

module M = Why3.Wstdlib.Mstr
module Th = Why3.Theory
open Fibers.Monad

(* -------------------------------------------------------------------------- *)
(* --- Proof Statistics                                                   --- *)
(* -------------------------------------------------------------------------- *)

let fixed = ref 0
let broken = ref 0
let updated = ref 0
let minimized = ref 0
let unchanged = ref 0
let nproved = ref 0
let nstuck = ref 0

let adepth = ref 0
let bdepth = ref 0
let atime = ref 0.0
let btime = ref 0.0

let (+=) r n = r := !r + n
let (^=) r t = r := max !r t

let provers : (string,Stats.stats) Hashtbl.t = Hashtbl.create 0
let tactics : (string,int ref) Hashtbl.t = Hashtbl.create 0

let rec full_stats (p : Crc.crc) =
  match p.status with
  | Stuck -> ()
  | Prover(p,t) ->
    let id = Prover.desc_to_string p in
    let stat =
      try Hashtbl.find provers id with Not_found ->
        let s = Stats.create () in
        Hashtbl.add provers id s ; s
    in Stats.add stat t
  | Tactic { id ; children } ->
    let stat =
      try Hashtbl.find tactics id with Not_found ->
        let s = ref 0 in
        Hashtbl.add tactics id s ; s
    in incr stat ; List.iter full_stats children

let rec node_stats (a : Crc.crc) (b : Crc.crc) =
  begin
    match a.status, b.status with

    | Stuck, Stuck ->
      incr unchanged ;
      incr nstuck ;

    | Stuck, _ ->
      (if Crc.is_complete b then fixed else updated) += Crc.size b ;
      nstuck += Crc.get_stuck b ;
      nproved += Crc.get_proved b ;

    | _, Stuck ->
      incr (if Crc.is_complete a then broken else updated) ;
      incr nstuck ;

    | Prover _, Prover _ ->
      incr (if a = b then unchanged else updated) ;
      incr nproved ;

    | Tactic _ , Prover _ ->
      incr (if Crc.is_complete a then minimized else fixed) ;
      incr nproved ;

    | Tactic { id = f0 ; children = cs0 } ,
      Tactic { id = f1 ; children = cs1 }
      when f0 = f1 && List.length cs0 = List.length cs1 ->
      List.iter2 node_stats cs0 cs1

    | _ ->
      let r =
        match Crc.is_complete a, Crc.is_complete b with
        | true, true | false, false -> updated
        | true, false -> broken
        | false, true -> fixed
      in
      r += Crc.size b ;
      nstuck += Crc.get_stuck b ;
      nproved += Crc.get_proved b ;

  end

let stats a b =
  begin
    node_stats a b ;
    full_stats b ;
    adepth ^= Crc.get_depth a ;
    bdepth ^= Crc.get_depth b ;
    atime ^= Crc.get_time a ;
    btime ^= Crc.get_time b ;
  end

let print_time fmt =
  begin
    Utils.pp_time fmt !btime ;
    let d = !btime -. !atime in
    if abs_float d > 0.01 then
      Format.fprintf fmt " (%a)" Utils.pp_delta d ;
  end

let print_depth fmt =
  if !bdepth > 0 || !adepth > 0 then
    begin
      Format.fprintf fmt ", depth: %d" !bdepth ;
      let d = !bdepth - !adepth in
      if d < 0 then Format.fprintf fmt " (%d)" d ;
      if d > 0 then Format.fprintf fmt " (+%d)" d ;
    end

let print_stats allprovers alltactics =
  begin
    Utils.flush () ;
    Format.printf "Proofs %t"
      (Crc.pp_result ~proved:!nproved ~stuck:!nstuck) ;
    if !fixed + !broken + !minimized + !updated = 0 then
      if !nproved >0 then
        Format.printf " (unchanged)@\n"
      else
        Format.printf " (none)@\n"
    else
      begin
        Format.print_newline () ;
        if !unchanged > 0 then
          Format.printf " - unchanged: %d@\n" !unchanged ;
        if !fixed > 0 then
          Format.printf " - fixed: @{<green>%d@}@\n" !fixed ;
        if !minimized > 0 then
          Format.printf " - minimized: @{<green>%d@}@\n" !minimized ;
        if !updated > 0 then
          Format.printf " - updated: @{<orange>%d@}@\n" !updated ;
        if !broken > 0 then
          Format.printf " - broken: @{<red>%d@}@\n" !broken ;
      end ;
    if !btime > 0.0 || !bdepth > 0 then
      Format.printf "Provers %t%t@\n" print_time print_depth ;
    List.iter
      (fun (prv : Prover.prover) ->
         try
           let id = Prover.fullname prv in
           let s = Hashtbl.find provers id in
           Format.printf " - %-20s (%4d) (%a - %a - %a)@\n"
             id (Stats.count s)
             Utils.pp_rtime (Stats.min s)
             Utils.pp_rtime (Stats.average s)
             Utils.pp_rtime (Stats.max s)
         with Not_found -> ()
      ) allprovers ;
    List.iter
      (fun (tac : Tactic.tactic) ->
         try
           let id = Tactic.name tac in
           let s = Hashtbl.find tactics id in
           Format.printf " - %-20s (%4d)@\n" id !s
         with Not_found -> ()
      ) alltactics ;
    Format.print_flush ()
  end

(* -------------------------------------------------------------------------- *)
(* --- Proof Driver                                                       --- *)
(* -------------------------------------------------------------------------- *)

let accept ~gnames thy goal =
  List.mem thy gnames || List.mem (thy ^ "." ^ (Session.goal_name goal)) gnames

let prove_goal ~file ~hammer ~results ~force buffer theory gname goal hint =
  let+ crc =
    Crc.merge hint @+
    Hammer.schedule hammer goal (if force then Crc.stuck () else hint)
  in
  Option.iter (Proofs.add theory goal crc) buffer;
  if Utils.tty then stats hint crc ;
  if not (Crc.is_complete crc) then
    begin
      Utils.flush () ;
      Option.iter
        (Format.printf "%a: proof failed@." Why3.Loc.pp_position)
        (Session.goal_loc goal) ;
      Format.printf "@[<v2>Goal @{<red>%s.%s@}: %a@]@."
        (Session.name theory) gname Crc.pretty crc ;
      Crc.iter
        (fun g (s : Crc.status) ->
           match s with
           | Prover _ | Tactic _ -> ()
           | Stuck ->
             match results with
             | `Context context when Utils.tty && context >= 0 ->
               Vc.highlight ~file ~context
                 (Session.theory theory)
                 (Session.goal_task g)
             | `Tasks -> Vc.dump ~file (Session.goal_task g)
             | _ -> Format.printf "  %a@." Session.pp_goal g
        ) crc ;
    end ;
  goal, crc

let prove_theory ~file ~hammer ~results ~force ~gnames buffer strategy theory =
  let thy = Session.name theory in
  let goals = Session.split theory in
  let hints = M.find_def M.empty thy strategy in
  Option.iter (Proofs.init theory goals hints) buffer ;
  let goals =
    if gnames = [] then goals else List.filter (accept ~gnames thy) goals in
  let+ proofs =
    Fibers.all @@ List.map
      (fun goal ->
         let gname = Session.goal_name goal in
         let hint = try M.find gname hints with Not_found -> Crc.stuck () in
         prove_goal ~file ~hammer ~results ~force buffer theory gname goal hint
      ) goals
  in theory, proofs

(* -------------------------------------------------------------------------- *)
(* --- Axioms Logger                                                      --- *)
(* -------------------------------------------------------------------------- *)

let stdlib = ref false
let externals = ref false
let builtins = ref false

let standardized = "Standard  ",ref 0
let dependencies = "Assumed   ",ref 0
let hypotheses   = "Hypothesis",ref 0
let parameters   = "Parameter ",ref 0
let procedures   = "Procedure ",ref 0
let externalized = "External  ",ref 0
let unsoundness  = "Unsound   ",ref 0

let print_axioms_stats () =
  let u = !(snd unsoundness) in
  let p = !(snd parameters) in
  let h = !(snd hypotheses) in
  let r = !(snd procedures) in
  let e = !(snd externalized) in
  let d = !(snd dependencies) in
  let s = !(snd standardized) in
  if u+p+h+r+e+d+s = 0 then
    Format.printf "Hypotheses: none@."
  else
    begin
      Format.printf "Hypotheses:@\n" ;
      if p > 0 then
        Format.printf " - parameter%a: @{<green>%d@}@\n" Utils.pp_s p p ;
      if h > 0 then
        Format.printf " - hypothes%a: @{<orange>%d@}@\n" Utils.pp_yies h h ;
      if r > 0 then
        Format.printf " - procedure%a: @{<orange>%d@}@\n" Utils.pp_s r r ;
      if e > 0 then
        Format.printf " - externalized: @{<orange>%d@}@\n" e ;
      if s > 0 then
        Format.printf " - standardized: @{<orange>%d@}@\n" s ;
      if u > 0 then
        Format.printf " - unsound%a: @{<red>%d@}@\n" Utils.pp_s u u ;
      if d > 0 then
        Format.printf " - dependenc%a: @{<red>%d@}@\n" Utils.pp_yies d d ;
      Format.print_flush ()
    end

let pp_print_red fmt = Format.fprintf fmt "@{<red>%s@}"

let report_parameter ~lib ~signature (prm : Axioms.parameter) =
  try
    let id = prm.name in
    let ident = Id.resolve ~lib id in
    let std = ident.id_pkg = `Stdlib in
    let builtin = prm.builtin <> [] in
    let extern = prm.extern <> None in
    if (!stdlib || not std) &&
       ((!builtins && builtin) || (!externals || not extern))
    then
      begin
        let pp, kind, param =
          match prm.kind with
          | Type -> Format.pp_print_string,"type ", parameters
          | Logic -> Format.pp_print_string,"logic", parameters
          | Param -> Format.pp_print_string,"param", parameters
          | Value -> Format.pp_print_string,"value", procedures
          | Axiom -> Format.pp_print_string,"axiom", hypotheses
          | Unsafe -> pp_print_red,"value", unsoundness
        in
        let action, count =
          if builtin || extern then externalized else
          if signature then param else
          if std then standardized else dependencies
        in incr count ;
        Format.printf "  %a %s %a" pp action kind Id.pp_title ident ;
        let categories = List.filter (fun c -> c <> "") [
            if std then "stdlib" else "" ;
            if builtin then "builtin" else "" ;
            if extern then "extern" else "" ;
          ] in
        if categories <> [] then
          Format.printf " (%s)" (String.concat ", " categories) ;
        Format.printf "@\n" ;
      end
  with Not_found -> ()

let report_signature henv ~lib (th : Th.theory) =
  match henv with
  | None -> ()
  | Some henv ->
    List.iter
      (report_parameter ~lib ~signature:true)
      (Axioms.parameters @@ Axioms.signature henv th)

let report_hypotheses henv ~lib (ths : Th.theory list) =
  match henv with
  | None -> ()
  | Some henv ->
    let once = ref true in
    Axioms.iter henv
      (fun prm ->
         if !once then (Format.printf "Dependencies:@\n" ; once := false) ;
         report_parameter ~lib ~signature:false prm)
      ths

(* -------------------------------------------------------------------------- *)
(* --- Proof Logger                                                       --- *)
(* -------------------------------------------------------------------------- *)

type results = [ `Default | `Modules | `Theories | `Goals | `Proofs | `Tasks | `Context of int ]

let report ~(results:results) henv ~lib proofs =
  begin
    let stuck = ref 0 in
    let proved = ref 0 in
    let failed = ref false in
    List.iter
      (fun (_,goals) ->
         List.iter
           (fun (_,crc) ->
              stuck := !stuck + Crc.get_stuck crc ;
              proved := !proved + Crc.get_proved crc ;
              if not @@ Crc.is_complete crc then failed := true ;
           ) goals
      ) proofs ;
    let ths = List.map (fun (th,_) -> Session.theory th) proofs in
    begin
      let path = String.concat "." lib in
      match results with
      | `Modules | `Default ->
        Format.printf "Library %s: %t@."
          path (Crc.pp_result ~stuck:!stuck ~proved:!proved) ;
        List.iter (report_signature henv ~lib) ths
      | `Theories | `Goals | `Proofs | `Tasks | `Context _ ->
        List.iter
          (fun (th,goals) ->
             let thy = Session.theory th in
             let tn = Session.name th in
             let (s,p) = List.fold_left
                 (fun (s,p) (_,c) -> s + Crc.get_stuck c, p + Crc.get_proved c)
                 (0,0) goals in
             Format.printf "Theory %s.%s: %t@." path tn
               (Crc.pp_result ~stuck:s ~proved:p) ;
             begin
               match results with
               | `Default | `Modules | `Theories | `Tasks | `Context _ -> ()
               | `Goals ->
                 List.iter
                   (fun (g,c) ->
                      Format.printf "  Goal %s: %a@."
                        (Session.goal_name g) Crc.pretty c
                   ) goals
               | `Proofs ->
                 List.iter
                   (fun (g,c) ->
                      Format.printf "  @[<hv 2>Goal %s %a@ %a@]@."
                        (Session.goal_name g)
                        Utils.pp_mark (Crc.is_complete c) Crc.dump c
                   ) goals
             end ;
             report_signature henv ~lib thy
          ) proofs
    end ;
    report_hypotheses henv ~lib ths ;
    !failed
  end

(* -------------------------------------------------------------------------- *)
(* --- Single File Processing                                             --- *)
(* -------------------------------------------------------------------------- *)

type mode = [ `Force | `Update | `Minimize | `Replay ]

let process ~env ~level ~mode ~session ~results ~provers ~tactics ~axioms ~unsuccess ~gnames file =
  Fibers.background @@
  begin
    let save = session in
    let exts = Wutil.exts () in
    if not @@ List.exists (fun suffix -> String.ends_with ~suffix file) exts
    then
      begin
        Log.error "invalid file name: %S" file ;
        exit 2
      end ;
    let dir, lib = Wutil.filepath file in
    let theories, format = Wutil.load_theories env.Project.why3.env file in
    let session = Session.create ~dir ~file ~format theories in
    let local = Calibration.default env in
    let profile, strategy = Proofs.load ~local file in
    let buffer = (* the proof strategy must be kept to stabilize crc files *)
      if (mode = `Replay)
      then None
      else Some (Proofs.create file profile strategy) in
    let force = (mode = `Force) in
    let hammer =
      Hammer.create
        ~level
        ~replay:(mode = `Replay)
        ~minimize:(mode = `Minimize)
        ~provers ~tactics ~profile
        env in
    let* proofs =
      Fibers.all @@ List.map
        (prove_theory ~file ~hammer ~results ~force ~gnames buffer strategy)
        (Session.theories session)
    in
    if save then Session.save session ;
    Option.iter Proofs.save buffer ;
    let henv =
      if axioms then
        Some (Axioms.init env)
      else None in
    Utils.flush () ;
    let failed = report ~results henv ~lib proofs in
    if failed then unsuccess := file :: !unsuccess ;
    Fibers.return ()
  end

(* -------------------------------------------------------------------------- *)
(* --- Prove Command                                                      --- *)
(* -------------------------------------------------------------------------- *)

type outcome = {
  provers : Prover.prover list ;
  tactics : Tactic.tactic list ;
  time : int ;
  mem : int ;
  unfixed : string list ;
}

let prove_files ~(env:Project.env)
    ~mode ~level ~session ~results ~axioms ~details
    ~files ~gnames =
  begin
    let unsuccess = ref [] in
    let results : results = match results with
      | `Default -> if List.length files > 1 then `Modules else `Theories
      | _ -> results in
    let provers = Prover.select env.why3 ~patterns:env.config.provers in
    let tactics = Tactic.select env.why3.env env.config.tactics in
    List.iter
      (process ~env ~level ~mode ~session ~results
         ~provers ~tactics ~axioms ~unsuccess ~gnames)
      files ;
    Hammer.run () ;
    Utils.flush () ;
    if Utils.tty || details then
      begin
        Runner.print_stats () ;
        print_stats
          (if details then provers else [])
          (if details then tactics else []) ;
      end ;
    if axioms then print_axioms_stats () ;
    {
      provers ; tactics ;
      time = max 1 (int_of_float (0.5 +. env.config.time)) ;
      mem = Runner.memlimit (Why3.Whyconf.get_main env.why3.config) ;
      unfixed = List.rev !unsuccess ;
    }
  end

(* -------------------------------------------------------------------------- *)
