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

module M = Why3.Wstdlib.Mstr
module Th = Why3.Theory

type profile = Calibration.profile
type proofs = Crc.crc M.t M.t (* Theory -> Task -> CRC *)

(* -------------------------------------------------------------------------- *)
(* --- Proof Files                                                        --- *)
(* -------------------------------------------------------------------------- *)

(* Json to Map converter *)
let jmap cc js =
  let m = ref M.empty in
  Json.jiter (fun fd js -> m := M.add fd (cc js) !m) js ; !m

(* Map to Json converter *)
let map cc m = Json.assoc @@ M.fold (fun a e js -> (a, cc e) :: js) m []

let jproofs : proofs -> Json.t = map (map Crc.to_json)

let filename file =
  Filename.concat (Filename.chop_extension file) "proof.json"

let load ?local file : profile * proofs =
  let file = filename file in
  let js = if Sys.file_exists file then Json.of_file file else `Null in
  let profile = Calibration.of_json ?default:local @@ Json.jfield "profile" js in
  let strategy = jmap (jmap Crc.of_json) @@ Json.jfield "proofs" js in
  profile , strategy

type buffer = {
  file : string;
  profile : Calibration.profile ;
  mutable proofs : proofs ;
}

let save_proofs { file; profile; proofs } =
  let file = filename file in
  Utils.mkdirs (Filename.dirname file) ;
  Json.to_file file @@ `Assoc [
    "profile", Calibration.to_json profile ;
    "proofs", jproofs proofs ;
  ]

let save_proofs = Timer.timed ~name:"Proofs dump" save_proofs

let create file profile proofs = { file; profile; proofs }

let dump dumper =
  save_proofs dumper

let add theory goal crc dumper =
  let theory = Session.name theory in
  let tasks = M.add goal crc @@ M.find_def M.empty theory dumper.proofs in
  let proofs = M.add theory tasks dumper.proofs in
  dumper.proofs <- proofs ; dump dumper
