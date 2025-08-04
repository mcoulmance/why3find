(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module M = Why3.Wstdlib.Mstr
module Th = Why3.Theory

type proofs = Crc.crc M.t M.t (* Theory -> Task -> CRC *)

(* -------------------------------------------------------------------------- *)
(* --- Proof Files                                                        --- *)
(* -------------------------------------------------------------------------- *)

(* Json to Map converter *)
let jmap cc js =
  let m = ref M.empty in
  Json.jiter (fun fd js -> m := M.add fd (cc js) !m) js ; !m

(* Map to Json converter *)
let map ?keepnull cc m =
  Json.assoc ?keepnull @@ List.rev @@ M.fold (fun a e js -> (a, cc e) :: js) m []

let jproofs : proofs -> Json.t = map (map  ~keepnull:true Crc.to_json)

let filename file =
  Filename.concat (Filename.chop_extension file) "proof.json"

let load file : Json.t * proofs =
  let file = filename file in
  let js = if Sys.file_exists file then Json.of_file file else `Null in
  let profile = if js = `Null then `List [] else Json.jfield "profile" js in
  let strategy = jmap (jmap Crc.of_json) @@ Json.jfield "proofs" js in
  profile , strategy

type buffer = {
  file : string;
  profile : Json.t ;
  mutable proofs : proofs ;
  theories_seen : (string, unit) Hashtbl.t ;
}

let dump =
  Timer.throttle @@
  Timer.timed ~name:"Proofs dump"
    begin function { file; profile; proofs } ->
      let file = filename file in
      Utils.mkdirs (Filename.dirname file) ;
      Json.to_file ~pretty_floats:true file @@ `Assoc [
        "profile", profile ;
        "proofs", jproofs proofs ;
      ]
    end

let create ~file profile proofs =
  { file; profile ; proofs ; theories_seen = Hashtbl.create 8 }

let add theory goal crc buffer =
  let theory = Session.name theory in
  let goal = Session.goal_name goal in
  let tasks = M.add goal crc @@ M.find_def M.empty theory buffer.proofs in
  buffer.proofs <- M.add theory tasks buffer.proofs ; dump buffer

let init theory goals hints buffer =
  let theory = Session.name theory in
  Hashtbl.add buffer.theories_seen theory ();
  let results =
    if M.is_empty hints then M.empty else
      List.fold_left
        (fun rs goal ->
           let a = Session.goal_name goal in
           match M.find_opt a hints with
           | None -> rs
           | Some r -> M.add a r rs)
        M.empty goals
  in buffer.proofs <- M.add theory results buffer.proofs ; dump buffer

let save buf =
  let proofs =
    M.filter (fun th _ -> Hashtbl.mem buf.theories_seen th) buf.proofs in
  dump ~force:true { buf with proofs }
