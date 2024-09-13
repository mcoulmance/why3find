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

(** Time related features. *)

(** {2 Timers}

    A timer is created with {!create}, and can the be started with {!start}.
    A running timer can be stopped with {!stop} and the time of this {e run}
    (the time elapsed since last {!start}) is added to the timer. A new run can
    then be started with {!start}
*)

type timer
(** The type of timers. *)

val create : unit -> timer
(** Return a new timer. *)

val start : timer -> unit
(** Start the timer. *)

val stop : timer -> unit
(** Stop the timer. *)

val count : timer -> int
(** Return the number of runs. *)

val min_time : timer -> float
(** Return the time of the shortest run. *)

val max_time : timer -> float
(** Return the time of the longest run. *)

val average_time : timer -> float
(** Return the average time of a run. *)

val total_time : timer -> float
(** Return the total time (the sum of the time of every run). *)

val create_global : string -> timer
(** Create and register a timer to be printed by {!print_timings}. The string
    argument is the timer name. *)

val timed : name:string -> ('a -> 'b) -> ('a -> 'b)
(** Return a timed version of the argument function : a global timer of name
    [name] will time the function execution. *)

val timed2 : name:string -> ('a -> 'b -> 'c) -> ('a -> 'b -> 'c)
(** Same as {!timed} for binary functions. *)

val timed3 : name:string -> ('a -> 'b -> 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
(** Same as {!timed} for ternary functions. *)

val print_timings : Format.formatter -> unit
(** Print the global timers on the given formatter. *)

(** {2 Throttles} *)

val throttle : ?delay:float -> ('a -> unit) -> (?force:bool -> 'a -> unit)
(** [throttle ~delay:d f] make succesive calls to [f] to only happen every
    [~delay] seconds. The delayed function can be passed [~force:true] to
    apply [f] immediately. Defaults to [~delay:0.1] which is 100ms. *)
