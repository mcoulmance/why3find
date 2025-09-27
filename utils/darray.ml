(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Dynamic Arrays                                                     --- *)
(* -------------------------------------------------------------------------- *)

type 'a t = { length : int Lazy.t ; value : int -> 'a }
let get m = m.value
let length m = Lazy.force m.length
let iter f m = for i = 0 to length m - 1 do f (m.value i) done
let iteri f m = for i = 0 to length m - 1 do f i (m.value i) done

let empty = { length = lazy 0 ; value = fun _ -> raise Not_found }
let concat a b =
  match length a , length b with
  | 0 , 0 -> empty
  | _ , 0 -> a
  | 0 , _ -> b
  | na , nb ->
    let value k = if k < na then a.value k else b.value k in
    { length = lazy (na + nb) ; value }

let const ~length e = { length = lazy length ; value = fun _ -> e }
let option = function None -> empty | Some e -> const ~length:1 e
let array m = { length = lazy (Array.length m) ; value = Array.get m }
let list xs = array (Array.of_list xs)
let map f m = { length = m.length ; value = fun k -> f @@ m.value k }

type 'a buffer = (int,'a) Hashtbl.t
let create () : 'a buffer = Hashtbl.create 0
let addi buf e = let k = Hashtbl.length buf in Hashtbl.add buf k e ; k
let add buf e = ignore @@ addi buf e
let contents buf = let n = Hashtbl.length buf in
  { length = lazy n ; value = Hashtbl.find buf }

let deferred collection =
  let length = lazy (Lazy.force (Lazy.force collection).length) in
  let value k = (Lazy.force collection).value k in
  { length ; value }

let make ~length ~value = { length ; value }
let cache ~length ~value =
  let h = Hashtbl.create 0 in
  let value k =
    try Hashtbl.find h k with Not_found ->
      let v = value k in Hashtbl.add h k v ; v
  in { length ; value }
