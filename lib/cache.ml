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
(* --- Cache                                                              --- *)
(* -------------------------------------------------------------------------- *)

let version = "v4"

(*
   Layout:

   CDIR/                    root cache directory    [~cdir]
   CDIR/version             cache version footprint
   CDIR/prover@ver/HH/HEX   serialized result

   cache version footprint is 'vx@why3.xxx'
*)

let reformat ~cdir =
  let fver = Filename.concat cdir "version" in
  let footprint = Printf.sprintf "%s@why3.%s\n" version Why3.Config.version in
  let local =
    if Sys.file_exists fver then Utils.readfile ~file:fver
    else "" in
  if local <> footprint then
    begin
      if Sys.file_exists cdir then
        begin
          Log.emit "Reformat cache (%s)" (String.trim footprint);
          Utils.rmpath cdir ;
        end ;
      Utils.mkdirs cdir ;
      Utils.writefile ~file:fver @@ footprint
    end

module Hash = Hashtbl.Make
    (struct
      type t = Why3.Task.task * Prover.prover
      let hash (t,p) = Hashtbl.hash (Why3.Task.task_hash t , p.Prover.digest)
      let equal (t1,p1) (t2,p2) = (p1 == p2) && (Why3.Task.task_equal t1 t2)
    end)

let basedigest =
  Timer.timed ~name:"Task checksum"
  @@ fun task -> Why3.Termcode.(task_checksum task |> string_of_checksum)

let digest ?prover task =
  Digest.to_hex @@ Digest.string @@
  let h = basedigest task in
  match prover with None -> h | Some p -> h ^ p.Prover.digest

type t = {
  cdir : string ;
  usecache : bool ;
  prepare : unit Lazy.t ;
  results : Result.t Hash.t ;
}

let create =
  let table = Hashtbl.create 16 in
  fun ~cdir ?(usecache=true) () ->
    try Hashtbl.find table (Utils.absolute cdir, usecache) with
    | Not_found ->
      let prepare = lazy (reformat ~cdir) in
      let results = Hash.create 0 in
      let res = { cdir ; prepare ; usecache ; results } in
      Hashtbl.add table (Utils.absolute cdir, usecache) res;
      res

let file cache task prover =
  Lazy.force cache.prepare ;
  let hx = digest ~prover task in
  let h2 = String.sub hx 0 2 in
  let (//) = Filename.concat in (* left associative: a//b//c = (a//b)//c  *)
  cache.cdir // Prover.(desc_to_string prover.desc) // h2 // hx

let read cache task prover =
  let f = file cache task prover in
  if Sys.file_exists f then
    try Json.of_file f |> Result.of_json with _ -> NoResult
  else NoResult

let write cache task prover result =
  let f = file cache task prover in
  Utils.mkdirs (Filename.dirname f) ;
  Json.to_file f (Result.to_json result)

let get cache task prover =
  let entry = task,prover in
  try Hash.find cache.results entry with Not_found ->
    let result = if cache.usecache then read cache task prover else NoResult in
    Hash.add cache.results entry result ; result

let set cache task prover result =
  let entry = task,prover in
  let current = try Hash.find cache.results entry with Not_found -> NoResult in
  let consolidated = Result.merge result current in
  if consolidated <> current then
    match consolidated with
    | NoResult -> ()
    | Failed -> Hash.replace cache.results entry Failed
    | Valid _ | Unknown _ | Timeout _ ->
      Hash.replace cache.results entry consolidated ;
      write cache task prover consolidated

(* -------------------------------------------------------------------------- *)
