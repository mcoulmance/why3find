(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
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
  Format.fprintf fmt "%-20s %10f (%4d) | %f - %f - %f |"
    name
    (total_time ti)
    (Stats.count (stats ti))
    (min_time ti)
    (average_time ti)
    (max_time ti)

let print_timings fmt =
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list print_global) (List.rev !global_timers)

let throttle ?(delay=0.1) fn =
  let stamp = ref (Unix.gettimeofday ()) in
  fun ?(force=false) x ->
    let time = Unix.gettimeofday () in
    if force || time > !stamp +. delay then
      (stamp := time ; fn x)
