(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Prover Result                                                      --- *)
(* -------------------------------------------------------------------------- *)

type t =
  | NoResult | Failed | Unknown of float | Timeout of float | Valid of float * int

let merge a b =
  match a,b with
  | NoResult,c | c,NoResult -> c
  | Failed,c | c,Failed -> c
  | Valid (ta, sa) , Valid (tb, sb) -> Printf.printf "TOTOTITITATA"; Valid (min ta tb, min sa sb)
  | Valid _ , (Unknown _ | Timeout _) -> a
  | (Unknown _ | Timeout _) , Valid _ -> b
  | Unknown ta , Unknown tb -> Unknown (min ta tb)
  | Unknown _ , Timeout _ -> a
  | Timeout _ , Unknown _ -> b
  | Timeout ta , Timeout tb -> Timeout (max ta tb)

let map f = function
  | NoResult -> NoResult
  | Failed -> Failed
  | Valid (t,s) -> Valid (f t, s)
  | Unknown t -> Unknown (f t)
  | Timeout t -> Timeout (f t)

let pp_result fmt = function
  | NoResult -> Format.pp_print_string fmt "No Result"
  | Failed -> Format.pp_print_string fmt "Failed"
  | Unknown t -> Format.fprintf fmt "Unknown (%a)" Utils.pp_time t
  | Timeout t -> Format.fprintf fmt "Timeout (%a)" Utils.pp_time t
  | Valid (t, s) -> Format.fprintf fmt "Valid (%a, %d)" Utils.pp_time t s

let pp_verdict fmt = function
  | NoResult -> Format.pp_print_string fmt "No Result"
  | Failed -> Format.pp_print_string fmt "Failed"
  | Unknown _ -> Format.pp_print_string fmt "Unknown"
  | Timeout _ -> Format.pp_print_string fmt "Timeout"
  | Valid _ -> Format.pp_print_string fmt "Valid"

let of_json (js : Json.t) =
  let open Json in
  let status = jfield "status" js |> jstring in
  let time = jfield "time" js |> jfloat in
  let step = jfield "steps" js |> jint in
  match status with
  | "Failed" -> Failed
  | "Unknown" -> Unknown time
  | "Timeout" -> Timeout time
  | "Valid" -> Valid (time, step)
  | _ -> NoResult

let to_json (r : t) : Json.t =
  let status st = `Assoc [ "status", `String st ] in
  let timed st t s = `Assoc [ "status", `String st ; "time", `Float t; "steps", `Int s ] in
  match r with
  | NoResult -> status "NoResult"
  | Failed -> status "Failed"
  | Unknown t -> timed "Unknown" t 0 
  | Timeout t -> timed "Timeout" t 0
  | Valid (t, s) -> timed "Valid" t s

(* -------------------------------------------------------------------------- *)
(* --- Promotion                                                          --- *)
(* -------------------------------------------------------------------------- *)

let crop ~timeout result =
  match result with
  | NoResult -> None
  | Failed | Unknown _ -> Some result
  | Timeout t -> if timeout <= t then Some result else None
  | Valid (t, _) ->
    if t <= timeout then Some result else Some (Timeout timeout)
