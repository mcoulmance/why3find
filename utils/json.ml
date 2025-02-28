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
(* --- JSON Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

type t = Yojson.Basic.t

let of_file f : t = Yojson.Basic.from_file f
let of_string s : t = (Yojson.Basic.from_string s :> t)

let float_needs_period s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '0'..'9' | '-' -> ()
      | _ -> raise Exit
    done;
    true
  with Exit ->
    false

let string_of_truncated_float f =
  let s = Printf.sprintf "%.3g" f in
  (* we don't want floats to get parsed back as ints *)
  if float_needs_period s then s ^ ".0" else s

let rec truncate_floats : Yojson.Basic.t -> Yojson.t = function
  | `Int i -> `Intlit (string_of_int i)
  | `Float f -> `Floatlit (string_of_truncated_float f)
  | `String _ as s -> `Stringlit (Yojson.Basic.to_string s)
  | `Assoc fs -> `Assoc (List.map (fun (s, js) -> (s, truncate_floats js)) fs)
  | `List js -> `List (List.map truncate_floats js)
  | (`Null | `Bool _ as js) -> js

let to_file ?(pretty_floats=false) f js =
  let out = open_out f in
  let js = if pretty_floats then truncate_floats js else (js :> Yojson.t) in
  Yojson.pretty_to_channel ~std:true out js ;
  output_char out '\n';
  close_out out

let to_string ?(pretty_floats=false) js =
  let js = if pretty_floats then truncate_floats js else (js :> Yojson.t) in
  Yojson.pretty_to_string ~std:true js

let pp_gen ?(pretty_floats=false) fmt js =
  Format.pp_print_string fmt @@ to_string ~pretty_floats js

let pretty = pp_gen ~pretty_floats:true
let print = pp_gen ~pretty_floats:false

let jbool = function
  | `Bool b -> b
  | _ -> false

let jint = function
  | `Int n -> n
  | `Float a -> int_of_float (a +. 0.5)
  | _ -> 0

let jfloat = function
  | `Float a -> a
  | `Int n -> float n
  | _ -> 0.0

let jstring = function
  | `String a -> a
  | _ -> ""

let jlist = function
  | `List xs -> xs
  | _ -> []

let jmap f js = jlist js |> List.map f

let jstringlist js = jlist js |> List.map jstring

let jmem fd = function
  | `Assoc fds -> List.mem_assoc fd fds
  | _ -> false

let mfield fd = function
  | `Assoc fds -> List.mem_assoc fd fds
  | _ -> false

let jfield fd = function
  | `Assoc fds -> (try List.assoc fd fds with Not_found -> `Null)
  | _ -> `Null

let jpath fds js =
  List.fold_left (fun js fd -> jfield fd js) js @@
  String.split_on_char '/' fds

let joption f = function
  | `Null -> None
  | js -> Some (f js)

let jdefault value pp = function
  | `Null -> value
  | js -> pp js

let jfield_exn fd = function
  | `Assoc fds -> List.assoc fd fds
  | _ -> raise Not_found

let jiter f = function
  | `Assoc fds -> List.iter (fun (fd,js) -> f fd js) fds
  | _ -> ()

let is_empty = function
  | `Null -> true
  | `List [] -> true
  | `Assoc [] -> true
  | _ -> false

let is_nonnull = function `Null -> false | _ -> true

let int n  = `Int n
let string s = `String s

let assoc ?(keepnull=false) fts =
  if keepnull then `Assoc fts
  else `Assoc (List.filter (fun (_,v) -> is_nonnull v) fts)

let list ?(keepnull=false) js =
  if keepnull then `List js
  else `List (List.filter is_nonnull js)

let option_map f = function None -> `Null | Some a -> f a

let list_map ?(keepnull=false) f xs = `List
    (List.filter_map
       (fun x -> let v = f x in if keepnull || is_nonnull v then Some v else None) xs)

(* -------------------------------------------------------------------------- *)
