(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Prover Stats                                                       --- *)
(* -------------------------------------------------------------------------- *)

type pstats = {
  mutable failed : int ;
  mutable proved : int ;
  mutable time : float ;
  mutable depth : int ;
  tactics : (string,int) Hashtbl.t ;
  provers : (string,Stats.stats) Hashtbl.t ;
}

let create () = {
  failed = 0 ;
  proved = 0 ;
  time = 0.0 ;
  depth = 0 ;
  tactics = Hashtbl.create 0 ;
  provers = Hashtbl.create 0 ;
}

let rec add_crc ps (crc : Crc.crc) =
  match crc with
  | Stuck -> ps.failed <- succ ps.failed
  | Prover (p,_,t, _) ->
    ps.time <- t +. ps.time;
    ps.proved <- succ ps.proved ;
    let s =
      try Hashtbl.find ps.provers p with Not_found ->
        let s = Stats.create () in
        Hashtbl.add ps.provers p s ; s
    in Stats.add s t
  | Tactic { id ; children ; depth } ->
    ps.depth <- max ps.depth depth ;
    let n = try Hashtbl.find ps.tactics id with Not_found -> 0 in
    Hashtbl.replace ps.tactics id (succ n) ;
    List.iter (add_crc ps) children

let add_strategy ps proofs =
  let module M = Why3.Wstdlib.Mstr in
  M.iter (fun _ cs -> M.iter (fun _ c -> add_crc ps c) cs) proofs

let failed ps = ps.failed
let proved ps = ps.proved
let total ps = ps.failed + ps.proved
let time ps = ps.time
let depth ps = ps.depth

let iter f hs =
  let rs = ref [] in
  Hashtbl.iter (fun p s -> rs := (p,s) :: !rs) hs ;
  List.iter (fun (p,s) -> f p s) @@
  List.sort (fun (p,_) (q,_) -> String.compare p q) !rs

let iter_tactics f ps = iter f ps.tactics
let iter_provers f ps = iter f ps.provers

(* -------------------------------------------------------------------------- *)
(* --- JSON Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

let jsloc ~file = function
  | None -> `Null
  | Some loc ->
    let f,l1,c1,l2,c2 = Why3.Loc.get loc in
    if f <> file then `Null else
      Json.assoc [
        "line", `Int l1 ;
        "column", `Int c1 ;
        "end_line", if l2 = l1 then `Null else `Int l2 ;
        "end_column", if c2 = c1 then `Null else `Int c2 ;
      ]

let jurl_name ~url = function
  | None -> `Null
  | Some name -> `String (Printf.sprintf "%s#%s" url name)

let jurl_loc ~file ~url ?name = function
  | None -> jurl_name ~url name
  | Some loc ->
    let f,line,_,_,_ = Why3.Loc.get loc in
    if f <> file
    then jurl_name ~url name
    else `String (Printf.sprintf "%s#_L%d" url line)

(* -------------------------------------------------------------------------- *)
(* --- Proof Statistics                                                   --- *)
(* -------------------------------------------------------------------------- *)

type jstats = {
  time : float ;
  stuck : int ;
  proved : int ;
  json : Json.t ;
}

let empty = { time = 0.0 ; stuck = 0 ; proved = 0 ; json = `Null }

let is_empty { proved ; json } = (proved = 0) || (json = `Null)

let jmap f xs =
  let time = ref 0.0 in
  let stuck = ref 0 in
  let proved = ref 0 in
  let js = List.map
      (fun x ->
         let s = f x in
         time := !time +. s.time ;
         stuck := !stuck + s.stuck ;
         proved := !proved + s.proved ;
         s.json
      ) xs
  in {
    time = !time ;
    stuck = !stuck ;
    proved = !proved ;
    json = Json.list js ;
  }

let jnode (fields : (string * Json.t) list) =
  match List.assoc "children" fields with
  | `List cs as children when cs <> [] -> `Assoc [ "children" , children ]
  | _ | exception Not_found -> Json.assoc fields

(* -------------------------------------------------------------------------- *)
(* --- Compute Statistics                                                 --- *)
(* -------------------------------------------------------------------------- *)

let bind env ?goal id children =
  let (let*) v fn = match v with None -> [] | Some a -> fn a in
  let goals =
    let* goal = goal in
    let* tactic = Tactic.lookup env id in
    let* goals = Session.apply env tactic goal in
    goals in
  let rec pairwise k gs cs =
    match gs, cs with
    | _, [] -> []
    | [] , c::cs -> (k,None,c) :: pairwise (succ k) [] cs
    | g::gs , c::cs -> (k,Some g,c) :: pairwise (succ k) gs cs
  in pairwise 0 goals children

let rec stats_crc env ~file ~url ~path ~name ~onStuck (k,goal,crc) : Json.t =
  let loc = Option.bind goal Session.target in
  let path = Printf.sprintf "%s.%d" path (succ k) in
  let status = Format.asprintf "%a" Crc.pp_hint crc in
  let base = [
    "url" , jurl_loc ~file ~url ~name loc ;
    "path" , `String path ;
    "status" , `String status ;
    "location" , jsloc ~file loc ;
  ] in
  let stats =
    match crc with
    | Crc.Stuck ->
      Option.iter (fun f -> Option.iter f goal) onStuck ;
      [
        "time" , `Float 1.0 ; (* fake *)
        "stuck", `Int 1 ;
      ]
    | Crc.Prover(p,_,time, _) -> [
        "time" , `Float time ;
        "proved" , `Int 1 ;
        "prover" , `String p ;
      ]
    | Crc.Tactic { id ; children ; time ; stuck ; proved } -> [
        "time" , `Float time ;
        "stuck" , Json.optint stuck ;
        "proved" , Json.optint proved ;
        "tactic" , `String id ;
        "children" , Json.list_map
          (stats_crc env ~file ~url ~path ~name ~onStuck)
          (bind env ?goal id children) ;
      ]
  in jnode (base @ stats)

let stats_goal proofs ?subgoals ?lenses ?onStuck ~file ~path ~url (goal : Session.goal) =
  let name = Session.name goal in
  let crc = Why3.Wstdlib.Mstr.find_def Crc.stuck name proofs in
  let infix = Id.of_infix @@ name in
  let path = Printf.sprintf "%s.%s" path infix in
  let status = Format.asprintf "%a" Crc.pp_hint crc in
  let loc = Session.target goal in
  begin match lenses , loc with
    | None , _ | _ , None -> ()
    | Some lenses , Some loc ->
      let f,line,_,_,_ = Why3.Loc.get loc in
      if f = file then Hashtbl.add lenses line status
  end ;
  let base = [
    "goal" , `String name ;
    "path" , `String path ;
    "url" , jurl_loc ~file ~url ~name loc ;
    "status", `String status ;
    "location", jsloc ~file loc ;
  ] in
  match crc with
  | Crc.Stuck ->
    Option.iter (fun f -> f goal) onStuck ;
    {
      json = Json.assoc @@ base @ [
          "time", `Float 1.0 ; (* fake *)
          "stuck" , `Int 1 ;
        ] ;
      stuck = 1 ; proved = 0 ; time = 0.0 ;
    }
  | Crc.Prover(_,_,time, _) ->
    {
      json = Json.assoc @@ base @ [
          "time", `Float time ;
          "proved", `Int 1 ;
        ] ;
      stuck = 0 ; proved = 1 ; time ;
    }
  | Crc.Tactic { time ; stuck ; proved ; depth ; id ; children } ->
    let children =
      match subgoals with None -> `Null | Some env ->
        Json.list_map
          (stats_crc env ~url ~file ~path ~name ~onStuck)
          (bind env ~goal id children)
    in {
      json = jnode @@ base @ [
          "time" , `Float time ;
          "stuck" , Json.optint stuck ;
          "proved" , Json.optint proved ;
          "children" , children ;
          "depth" , Json.optint depth ;
        ] ;
      stuck ; proved ; time ;
    }

let stats_theory ?subgoals ?lenses ?onStuck ~file ~path
    (strategy : Proofs.strategy)
    (thy : Session.theory) =
  let name = Session.thy_name thy in
  let line =
    match (Session.thy thy).th_name.id_loc with
    | None -> 0
    | Some loc -> let _,l,_,_,_ = Why3.Loc.get loc in l in
  let proofs = Why3.Wstdlib.Mstr.(find_def empty name strategy) in
  let goals = Session.split thy in
  let url = Printf.sprintf "%s.%s.html" path name in
  let path = if subgoals = None then Printf.sprintf "%s.%s" path name else name in
  let jstats =
    jmap (stats_goal proofs ?subgoals ?lenses ?onStuck ~file ~path ~url) goals in
  let json = jnode [
      "path", `String path ;
      "file", `String file ;
      "line", `Int line ;
      "url", `String (Printf.sprintf "%s#_L%d" url line) ;
      "children", jstats.json ;
      "time", `Float jstats.time ;
      "stuck" , Json.optint jstats.stuck ;
      "proved" , Json.optint jstats.proved ;
    ]
  in { jstats with json }

let stats_session ~file ~path ?subgoals ?lenses ?onStuck strategy session =
  let theories = Session.theories session in
  let jstats = jmap
      (stats_theory ?subgoals ?lenses ?onStuck strategy ~file ~path) theories in
  let url = Printf.sprintf "%s.html" path in
  let json = jnode [
      "path", `String path ;
      "file", `String file ;
      "url", `String url ;
      "children", jstats.json ;
      "time", `Float jstats.time ;
      "stuck" , Json.optint jstats.stuck ;
      "proved" , Json.optint jstats.proved ;
    ]
  in { jstats with json }

let stats_file env ~root ~file =
  let theories, format = Wutil.load_theories env file in
  let path, _ = Wutil.filepath ~root ~file in
  let session = Session.create ~file ~format theories in
  let Proofs.{ strategy } = Proofs.load ~move:false ~root ~path ~file () in
  stats_session ~file ~path strategy session

let statistics ~root ~env ~files =
  let jstats = jmap (fun file -> stats_file env ~root ~file) files in
  let json =
    Json.assoc [
      "files", jstats.json ;
      "time", `Float jstats.time ;
      "stuck", Json.optint jstats.stuck ;
      "proved", Json.optint jstats.proved ;
    ] in
  { jstats with json }

(* -------------------------------------------------------------------------- *)
