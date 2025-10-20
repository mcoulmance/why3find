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

type status =
  | Stuck
  | Prover of Prover.desc * float
  | Tactic of {
      id : string ;
      children : crc list ;
      stuck : int ;
      proved : int ;
      depth : int ;
      time : float ;
    }

and crc = {
  goal : Session.goal option ; (* not saved in JSON *)
  status : status ;
}

let none = Stuck
let stuck ?goal () = { goal ; status = Stuck }
let prover ?goal p f = { goal ; status = Prover(p,f) }

let get_stuck { status } = match status with
  | Stuck -> 1
  | Prover _ -> 0
  | Tactic { stuck } -> stuck

let get_proved { status } = match status with
  | Stuck -> 0
  | Prover _ -> 1
  | Tactic { proved } -> proved

let get_time { status } = match status with
  | Stuck -> 0.0
  | Prover(_,t) -> t
  | Tactic { time } -> time

let get_depth { status } = match status with
  | Stuck | Prover _ -> 0
  | Tactic { depth } -> succ depth

let size { status } = match status with
  | Stuck | Prover _ -> 1
  | Tactic { stuck ; proved } -> stuck + proved

let is_stuck { status } = match status with
  | Stuck -> true
  | Prover _ | Tactic _ -> false

let is_complete { status } = match status with
  | Stuck -> false
  | Prover _ -> true
  | Tactic { stuck } -> stuck = 0

let is_unknown { status } = match status with
  | Stuck -> true
  | Prover _ -> false
  | Tactic { stuck ; proved } -> stuck > 0 && proved = 0

let get_time_all = List.fold_left (fun t c -> max t @@ get_time c) 0.0
let get_depth_all = List.fold_left (fun h c -> max h @@ get_depth c) 0
let get_stuck_all = List.fold_left (fun n c -> n + get_stuck c) 0
let get_proved_all = List.fold_left (fun n c -> n + get_proved c) 0

let make goal id children =
  let stuck = get_stuck_all children in
  let proved = get_proved_all children in
  let time = get_time_all children in
  let depth = get_depth_all children in
  if stuck > 0 && proved = 0 then { goal ; status = Stuck } else
    { goal; status = Tactic { id ; children ; stuck ; proved ; depth ; time } }

let tactic ?goal tac children = make goal (Tactic.name tac) children

type verdict = [ `Valid of int | `Failed of int | `Partial of int * int ]

let nverdict ~stuck:s ~proved:p =
  if s = 0 then `Valid p else
  if p = 0 then `Failed s else
    `Partial(p,s+p)

let verdict crc = nverdict ~stuck:(get_stuck crc) ~proved:(get_proved crc)
let verdict_all cs = nverdict ~stuck:(get_stuck_all cs) ~proved:(get_proved_all cs)

let pp_result fmt ~stuck:s ~proved:p =
  if s = 0 then
    if p > 0
    then Format.fprintf fmt "%t (%d)" Utils.pp_ok p
    else Format.fprintf fmt "%t (-)" Utils.pp_ok
  else
  if p = 0 then Utils.pp_ko fmt
  else Format.fprintf fmt "%t (%d/%d)" Utils.pp_ko p (s+p)

let pp_status fmt = function
  | Tactic { id } -> Format.pp_print_string fmt id
  | Prover(p,_) -> Format.pp_print_string fmt @@ Prover.desc_name p
  | Stuck -> Format.pp_print_string fmt "stuck"

let pretty fmt crc =
  let s = get_stuck crc in
  let p = get_proved crc in
  pp_result fmt ~stuck:s ~proved:p

(* -------------------------------------------------------------------------- *)
(* --- JSON                                                               --- *)
(* -------------------------------------------------------------------------- *)

let rec to_json (a : crc) : Json.t = match a.status with
  | Stuck -> `Null
  | Prover(p,t) ->
    `Assoc [ "prover", `String (Prover.desc_to_string p) ; "time", `Float t]
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
      prover (Prover.desc_of_string p) t
    | `Assoc fds when List.mem_assoc "tactic" fds ->
      let tr = List.assoc "tactic" fds |> Json.jstring in
      let xs = List.assoc "children" fds |> Json.jlist in
      make None tr (List.map of_json xs)
    | `Assoc fds when List.mem_assoc "transf" fds ->
      (* Deprecated *)
      let tr = List.assoc "transf" fds |> Json.jstring in
      let xs = List.assoc "children" fds |> Json.jlist in
      make None tr (List.map of_json xs)
    | _ -> stuck ()
  with _ -> stuck ()

(* -------------------------------------------------------------------------- *)
(* --- Merging                                                            --- *)
(* -------------------------------------------------------------------------- *)

let window t0 t1 =
  let w0 = max t0 0.01 in
  let w1 = max t1 0.01 in
  w0 *. 0.5 < w1 && w1 < w0 *. 2.0

let update ?(complete=true) ~hint result =
  let rec merge h r =
    let goal = match r.goal with None -> h.goal | g -> g in
    match h.status, r.status with
    | Prover(p0,t0), Prover(p1,t1) when p0 = p1 && window t0 t1 -> h
    | Tactic { id = f ; children = hs } ,
      Tactic { id = g ; children = rs }
      when f = g && List.length hs = List.length rs ->
      make goal f (List.map2 merge hs rs)
    | _ , Stuck when not complete -> h
    | _ -> r
  in merge hint result

(* -------------------------------------------------------------------------- *)
(* --- iter                                                               --- *)
(* -------------------------------------------------------------------------- *)

let rec iter (f : Session.goal -> status -> unit) c =
  Option.iter (fun g -> f g c.status) c.goal ;
  match c.status with
  | Stuck | Prover _ -> ()
  | Tactic { children } -> List.iter (iter f) children

(* -------------------------------------------------------------------------- *)
(* --- Dump                                                               --- *)
(* -------------------------------------------------------------------------- *)

let rec dump fmt { status } = match status with
  | Stuck -> Format.fprintf fmt "@{<red>Unknown@}"
  | Prover(p,t) -> Format.fprintf fmt "%s (%a)" (Prover.desc_name p) Utils.pp_time t
  | Tactic { id ; children } ->
    Format.fprintf fmt "@[<hv 2>%s" id ;
    List.iter (dumpchild fmt) children ;
    Format.fprintf fmt "@]"

and dumpchild fmt c = Format.pp_print_space fmt () ; dump fmt c

(* -------------------------------------------------------------------------- *)
