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
    adepth ^= Crc.get_depth a ;
    bdepth ^= Crc.get_depth b ;
    atime ^= Crc.get_time a ;
    btime ^= Crc.get_time b ;
  end

let print_stats () =
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
    Format.printf "Provers %a%t@." Utils.pp_time !btime
      (fun fmt ->
         let d = !btime -. !atime in
         if abs_float d > 0.01 then
           Format.fprintf fmt " (%a)" Utils.pp_delta d ;
         if !bdepth > 0 || !adepth > 0 then
           begin
             Format.fprintf fmt " (depth: %d" !bdepth ;
             let d = !bdepth - !adepth in
             if d < 0 then Format.fprintf fmt ", %d)" d else
             if d > 0 then Format.fprintf fmt ", +%d)" d else
               Format.fprintf fmt ")"
           end
      ) ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Proof Driver                                                       --- *)
(* -------------------------------------------------------------------------- *)

let accept ~goals thy task =
  List.mem thy goals || List.mem (thy ^ "." ^ (Session.goal_name task)) goals

let prove_theory ~goals ~file ~context mode profile buffer strategy theory =
  let thy = Session.name theory in
  let tasks = Session.split theory in
  let hints = M.find_def M.empty thy strategy in
  Option.iter (Proofs.init theory tasks hints) buffer ;
  let tasks =
    if goals = [] then tasks else List.filter (accept ~goals thy) tasks in
  let+ proofs =
    Fibers.all @@ List.map
      (fun task ->
         let goal = Session.goal_name task in
         let hint = M.find_def (Crc.stuck ()) goal hints in
         let replay = (mode = `Replay) in
         let+ crc =
           Crc.merge hint @+
           Hammer.schedule profile ~replay ~depth:0 ~idename:goal task
             (if mode = `Force then Crc.stuck () else hint)
         in
         Option.iter (Proofs.add theory task crc) buffer;
         if Utils.tty then stats hint crc ;
         if not (Crc.is_complete crc) then
           begin
             Utils.flush () ;
             Option.iter
               (Format.printf "%a: proof failed@." Why3.Loc.pp_position)
               (Session.goal_loc task) ;
             Format.printf "@[<v2>Goal @{<red>%s@}: %a@]@." goal Crc.pretty crc ;
             Crc.iter
               (fun g (s : Crc.status) ->
                  match s with Prover _ | Tactic _ -> () | Stuck ->
                    if Utils.tty && context >= 0 then
                      Vc.dump ~file ~context (Session.goal_task g)
                    else
                      Format.printf "  %a@." Session.pp_goal g
               ) crc ;
           end ;
         goal, crc
      ) tasks
  in
  theory, proofs

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

type results = [ `Modules | `Theories | `Goals | `Proofs | `Context of int ]

let report_results (log : results) henv ~lib proofs =
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
      match log with
      | `Modules ->
        Format.printf "Library %s: %t@."
          path (Crc.pp_result ~stuck:!stuck ~proved:!proved) ;
        List.iter (report_signature henv ~lib) ths
      | `Theories | `Goals | `Proofs | `Context _ ->
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
               match log with
               | `Modules | `Theories | `Context _ -> ()
               | `Goals ->
                 List.iter
                   (fun (g,c) ->
                      Format.printf "  Goal %s: %a@." g Crc.pretty c
                   ) goals
               | `Proofs ->
                 List.iter
                   (fun (g,c) ->
                      Format.printf "  @[<hv 2>Goal %s %a@ %a@]@." g
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
type log = [ `Default | `Context of int | results ]

let process ~env ~mode ~session ~(log:results) ~axioms ~unsuccess ~goals file =
  Fibers.background @@
  begin
    let exts = Wutil.exts () in
    if not @@ List.exists (fun suffix -> String.ends_with ~suffix file) exts
    then
      begin
        Log.error "invalid file name: %S" file ;
        exit 2
      end ;
    let dir, lib = Wutil.filepath file in
    let theories, format = Wutil.load_theories env.Wenv.wenv.env file in
    let session = Session.create ~session ~dir ~file ~format theories in
    let local = Calibration.default env in
    let profile, strategy = Proofs.load ~local file in
    let buffer = (* the proof strategy must be kept to stabilize crc files *)
      if (mode = `Replay)
      then None
      else Some (Proofs.create file profile strategy) in
    let context = match log with `Context n -> n | _ -> (-1) in
    let* proofs =
      Fibers.all @@ List.map
        (prove_theory ~goals ~file ~context mode profile buffer strategy)
        (Session.theories session)
    in
    Session.save session ;
    Option.iter (Proofs.save ~force:true) buffer ;
    let henv =
      if axioms then
        Some (Axioms.init env)
      else None in
    Utils.flush () ;
    let failed = report_results log henv ~lib proofs in
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

let prove_files ~(env:Config.env) ~mode ~hammer ~session ~log ~axioms ~files
    ~goals =
  begin
    let time = env.config.time in
    let maxdepth = env.config.depth in
    let patterns = env.config.provers in
    let tactics = Tactic.select env.wenv.env env.config.tactics in
    let provers = Prover.select env.wenv ~patterns in
    let unsuccess = ref [] in
    let minimize = (mode = `Minimize) in
    let log : results = match log with
      | `Default -> if List.length files > 1 then `Modules else `Theories
      | #results as l -> l in
    List.iter (process ~env ~mode ~session ~log ~axioms ~unsuccess ~goals) files ;
    Hammer.run {
      env ;
      client = Client.connect env ; level = hammer ;
      time ; maxdepth ; patterns ; provers ; tactics ; minimize ;
    } ;
    if Utils.tty then
      begin
        Utils.flush () ;
        Runner.print_stats () ;
        print_stats () ;
        if axioms then print_axioms_stats () ;
      end ;
    {
      provers ; tactics ;
      time = max 1 (int_of_float (0.5 +. time)) ;
      mem = Runner.memlimit (Why3.Whyconf.get_main env.wenv.config) ;
      unfixed = List.rev !unsuccess ;
    }
  end

(* -------------------------------------------------------------------------- *)
