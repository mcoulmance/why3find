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

type timer = {
  mutable start_date : float;
  stats_time : Stats.stats;
}

let create () =  {
  start_date = infinity;
  stats_time = Stats.create ();
}

let start ti =
  assert (ti.start_date = infinity);
  let t = Unix.gettimeofday () in
  ti.start_date <- t

let stop ti =
  assert (ti.start_date <> infinity);
  let t = Unix.gettimeofday () in
  Stats.add ti.stats_time (t -. ti.start_date);
  ti.start_date <- infinity

let stats ti =
  ti.stats_time

let count ti =
  assert (ti.start_date = infinity);
  Stats.count ti.stats_time

let min_time ti =
  assert (ti.start_date = infinity);
  Stats.min ti.stats_time

let max_time ti =
  assert (ti.start_date = infinity);
  Stats.max ti.stats_time

let average_time ti =
  assert (ti.start_date = infinity);
  Stats.average ti.stats_time

let total_time ti =
  assert (ti.start_date = infinity);
  Stats.sum ti.stats_time

let global_timers = ref []

let create_global name =
  let ti = create () in
  global_timers := (name, ti) :: !global_timers;
  ti

let timed ~name f =
  let ti = create_global name in
  fun x ->
    start ti;
    let r = f x in
    stop ti;
    r

let timed2 ~name f =
  let ti = create_global name in
  fun x y ->
    start ti;
    let r = f x y in
    stop ti;
    r

let timed3 ~name f =
  let ti = create_global name in
  fun x y z ->
    start ti;
    let r = f x y z in
    stop ti;
    r

let print_global fmt (name, ti) =
  Format.fprintf fmt "%s (min %f, max %f, avg %f, tot %f, count %d)"
    name
    (min_time ti)
    (max_time ti)
    (average_time ti)
    (total_time ti)
    (Stats.count (stats ti))

let print_timings fmt () =
  Format.fprintf fmt "@[<v>";
  Format.pp_print_list print_global fmt (List.rev !global_timers);
  Format.fprintf fmt "@]"
