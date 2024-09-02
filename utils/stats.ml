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

let min s =
  s.min

let max s =
  s.max

let sum s =
  s.sum

let count s =
  s.count

let average s =
  if s.count = 0 then 0.0 else s.sum /. float_of_int s.count
