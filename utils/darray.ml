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

type 'a t = { length : unit -> int ; value : int -> 'a }
let get m = m.value
let length m = m.length ()
let iter f m = for i = 0 to length m - 1 do f (m.value i) done
let iteri f m = for i = 0 to length m - 1 do f i (m.value i) done

let empty = { length = (fun () -> 0) ; value = fun _ -> raise Not_found }
let concat a b =
  match length a , length b with
  | 0 , 0 -> empty
  | _ , 0 -> a
  | 0 , _ -> b
  | na , nb ->
    let value k = if k < na then a.value k else b.value k in
    { length = (fun () -> na + nb) ; value }

let const ~length e = { length = (fun () -> length) ; value = fun _ -> e }
let option = function None -> empty | Some e -> const ~length:1 e
let array m = { length = (fun () -> Array.length m) ; value = Array.get m }
let list xs = array (Array.of_list xs)
let map f m = { length = m.length ; value = fun k -> f @@ m.value k }

type 'a buffer = (int,'a) Hashtbl.t
let create () : 'a buffer = Hashtbl.create 0
let addi buf e = let k = Hashtbl.length buf in Hashtbl.add buf k e ; k
let add buf e = ignore @@ addi buf e
let contents buf =
  { length = (fun () -> Hashtbl.length buf) ; value = Hashtbl.find buf }

let reverse d = {
  length = d.length ;
  value = fun k -> d.value (d.length () - k - 1) ;
}

let make ~length ~value = { length ; value }
let cache ~length ~value =
  let h = Hashtbl.create 0 in
  let value k =
    try Hashtbl.find h k with Not_found ->
      let v = value k in Hashtbl.add h k v ; v
  in { length ; value }

type 'a pager = {
  mutable last : 'a Seq.t ;
  mutable more : bool ;
  overhead : int ;
  buffer : (int,'a) Hashtbl.t ;
}

let rec feed (p : 'a pager) n =
  if n > 0 then
    match p.last () with
    | Nil -> p.more <- false
    | Cons(e,next) ->
      p.last <- next ;
      let k = Hashtbl.length p.buffer in
      Hashtbl.add p.buffer k e ;
      feed p (pred n)

let plength p () =
  Hashtbl.length p.buffer + if p.more then p.overhead else 0

let rec pvalue p k =
  try Hashtbl.find p.buffer k with Not_found when p.more ->
    feed p p.overhead ; pvalue p k

let sequence ?(overhead=20) (seq : 'a Seq.t) =
  let pager = {
    buffer = Hashtbl.create 0 ;
    last = seq ;
    more = true ;
    overhead ;
  } in { length = plength pager ; value = pvalue pager }

let delayed f a =
  let m = lazy (f a) in
  let length () = (Lazy.force m).length () in
  let value k = (Lazy.force m).value k in
  { length ; value }
