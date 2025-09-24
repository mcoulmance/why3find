(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type stats = {
  mutable min : float;
  mutable max : float;
  mutable sum : float;
  mutable count : int;
}

let create () = {
  min = infinity;
  max = neg_infinity;
  sum = 0.;
  count = 0;
}

let add s x =
  s.count <- s.count + 1;
  s.sum <- s.sum +. x;
  s.min <- min s.min x;
  s.max <- max s.max x

let norm f = if Float.is_finite f then f else 0.0
let min s = norm s.min
let max s = norm s.max
let sum s = s.sum
let count s = s.count
let average s = if s.count = 0 then 0.0 else s.sum /. float_of_int s.count
