(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Proof Certificates                                                 --- *)
(* -------------------------------------------------------------------------- *)

type crc =
  | Stuck
  | Prover of string * string option * float
  | Tactic of {
      id : string ;
      children : crc list ;
      stuck : int ;
      proved : int ;
      depth : int ;
      time : float ;
    }
let stuck = Stuck

let prover (p : Prover.prover) time =
  let name = Prover.desc_name p.desc in
  let vers = if p.relaxed then None else Some (Prover.desc_version p.desc) in
  Prover(name,vers,time)

let get_stuck = function
  | Stuck -> 1
  | Prover _ -> 0
  | Tactic { stuck } -> stuck

let get_proved = function
  | Stuck -> 0
  | Prover _ -> 1
  | Tactic { proved } -> proved

let get_time = function
  | Stuck -> 0.0
  | Prover(_,_,t) -> t
  | Tactic { time } -> time

let get_depth = function
  | Stuck | Prover _ -> 0
  | Tactic { depth } -> succ depth

let size = function
  | Stuck | Prover _ -> 1
  | Tactic { stuck ; proved } -> stuck + proved

let is_stuck = function
  | Stuck -> true
  | Prover _ | Tactic _ -> false

let is_complete = function
  | Stuck -> false
  | Prover _ -> true
  | Tactic { stuck } -> stuck = 0

let is_unknown = function
  | Stuck -> true
  | Prover _ -> false
  | Tactic { stuck ; proved } -> stuck > 0 && proved = 0

let get_time_all = List.fold_left (fun t c -> max t @@ get_time c) 0.0
let get_depth_all = List.fold_left (fun h c -> max h @@ get_depth c) 0
let get_stuck_all = List.fold_left (fun n c -> n + get_stuck c) 0
let get_proved_all = List.fold_left (fun n c -> n + get_proved c) 0

let make id children =
  let stuck = get_stuck_all children in
  let proved = get_proved_all children in
  let time = get_time_all children in
  let depth = get_depth_all children in
  if stuck > 0 && proved = 0 then Stuck else
    Tactic { id ; children ; stuck ; proved ; depth ; time }

let tactic tac children = make (Tactic.name tac) children

type verdict = [ `Valid of int | `Failed of int | `Partial of int * int ]

let nverdict ~stuck:s ~proved:p =
  if s = 0 then `Valid p else
  if p = 0 then `Failed s else
    `Partial(p,s+p)

let verdict crc = nverdict ~stuck:(get_stuck crc) ~proved:(get_proved crc)
let verdict_all cs = nverdict ~stuck:(get_stuck_all cs) ~proved:(get_proved_all cs)

let pp_checked fmt ~stuck:s ~proved:p =
  if s = 0 then
    if p > 0
    then Format.fprintf fmt "%t (%d)" Utils.pp_ok p
    else Format.fprintf fmt "%t (-)" Utils.pp_ok
  else
  if p = 0 then Utils.pp_ko fmt
  else Format.fprintf fmt "%t (%d/%d)" Utils.pp_ko p (s+p)

let pp_status fmt = function
  | Tactic { id } -> Format.pp_print_string fmt id
  | Prover(p,None,_) -> Format.pp_print_string fmt p
  | Prover(p,Some v,_) -> Format.fprintf fmt "%s@%s" p v
  | Stuck -> Format.pp_print_string fmt "stuck"

let pretty fmt crc =
  let s = get_stuck crc in
  let p = get_proved crc in
  pp_checked fmt ~stuck:s ~proved:p

(* -------------------------------------------------------------------------- *)
(* --- Detailed Printers                                                  --- *)
(* -------------------------------------------------------------------------- *)

module M = Map.Make(String)

let add k a =
  M.add k (try succ @@ M.find k a with Not_found -> 1) a

let rec provers m = function
  | Stuck -> ()
  | Prover (p, _, _) ->
    m := add p !m
  | Tactic { id ; children } ->
    m := add id !m ; List.iter (provers m) children

let pp_verdict fmt = function
  | `Valid 0 -> Format.pp_print_string fmt "Valid (no goal)"
  | `Valid _ -> Format.pp_print_string fmt "Valid"
  | `Failed 1 -> Format.fprintf fmt "Failed"
  | `Failed n -> Format.fprintf fmt "Failed (%d goals)" n
  | `Partial (s,n) -> Format.fprintf fmt "Partial (%d/%d)" s n

let pp_time fmt t =
  if t > 1e-3 then Format.fprintf fmt " (%a)" Utils.pp_time t

let pp_depth fmt d =
  if d > 0 then Format.fprintf fmt " (depth %d)" d

let pp_solver fmt p n =
  if n > 1
  then Format.fprintf fmt " (%s %d)" p n
  else Format.fprintf fmt " (%s)" p

let pp_provers fmt cs =
  let m = ref M.empty in
  List.iter (provers m) cs ;
  M.iter (pp_solver fmt) !m

let pp_proof fmt (crc : crc) =
  begin
    pp_verdict fmt @@ verdict crc ;
    pp_time fmt @@ get_time crc ;
    pp_provers fmt [crc] ;
    pp_depth fmt @@ get_depth crc ;
  end

let pp_proofs fmt (cs : crc list) =
  begin
    pp_verdict fmt @@ verdict_all cs ;
    pp_time fmt @@ get_time_all cs ;
    pp_provers fmt cs ;
    pp_depth fmt @@ get_depth_all cs ;
  end

let pp_hint fmt (crc : crc) =
  if is_stuck crc then
    Format.pp_print_string fmt "No proof"
  else pp_proof fmt crc

let pp_hints fmt = function
  | [] -> Format.pp_print_string fmt "No goal"
  | [c] -> pp_hint fmt c
  | cs ->
    if List.for_all is_stuck cs then
      Format.pp_print_string fmt "No proofs"
    else
      pp_proofs fmt cs

(* -------------------------------------------------------------------------- *)
(* --- JSON                                                               --- *)
(* -------------------------------------------------------------------------- *)

let rec to_json (a : crc) : Json.t =
  match a with
  | Stuck -> `Null
  | Prover(p,None,t) ->
    `Assoc [ "prover", `String p ; "time", `Float t]
  | Prover(p,Some v,t) ->
    let pv = Printf.sprintf "%s@%s" p v in
    `Assoc [ "prover", `String pv ; "time", `Float t]
  | Tactic { id ; children } ->
    `Assoc [
      "tactic", `String id;
      "children", `List (List.map to_json children);
    ]

let rec of_json (js : Json.t) : crc =
  try
    match js with
    | `Assoc fds when List.mem_assoc "prover" fds ->
      let p = List.assoc "prover" fds |> Json.jstring in
      let t = List.assoc "time" fds |> Json.jfloat in
      let p,v = Prover.pattern_of_string p in
      Prover(p,v,t)
    | `Assoc fds when List.mem_assoc "tactic" fds ->
      let tr = List.assoc "tactic" fds |> Json.jstring in
      let xs = List.assoc "children" fds |> Json.jlist in
      make tr (List.map of_json xs)
    | `Assoc fds when List.mem_assoc "transf" fds ->
      (* Deprecated *)
      let tr = List.assoc "transf" fds |> Json.jstring in
      let xs = List.assoc "children" fds |> Json.jlist in
      make tr (List.map of_json xs)
    | _ -> Stuck
  with _ -> Stuck

(* -------------------------------------------------------------------------- *)
(* --- Merging                                                            --- *)
(* -------------------------------------------------------------------------- *)

let equivalent ~period t0 t1 =
  Utils.scale ~period t0 = Utils.scale ~period t1

let update ?(complete=true) ?(period=1.0) ?(hint=Stuck) result =
  let rec merge h r =
    match h, r with
    | Prover(p0,v0,t0), Prover(p1,v1,t1)
      when p0 = p1 && v0 = v1 && equivalent ~period t0 t1 -> h
    | Tactic { id = f ; children = hs } ,
      Tactic { id = g ; children = rs }
      when f = g && List.length hs = List.length rs ->
      make f (List.map2 merge hs rs)
    | _ , Stuck when not complete -> h
    | _ -> r
  in merge hint result

(* -------------------------------------------------------------------------- *)
(* --- Dump                                                               --- *)
(* -------------------------------------------------------------------------- *)

let rec dump fmt = function
  | Stuck -> Format.fprintf fmt "@{<red>Unknown@}"
  | Prover(p,_,t) -> Format.fprintf fmt "%s (%a)" p Utils.pp_time t
  | Tactic { id ; children } ->
    Format.fprintf fmt "@[<hv 2>%s" id ;
    List.iter (dumpchild fmt) children ;
    Format.fprintf fmt "@]"

and dumpchild fmt c = Format.pp_print_space fmt () ; dump fmt c

(* -------------------------------------------------------------------------- *)
