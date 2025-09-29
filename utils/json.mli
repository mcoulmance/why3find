(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- JSON Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

type t = Yojson.Basic.t

val of_file : string -> t
val of_string : string -> t

val to_file : ?pretty_floats:bool -> string -> t -> unit
(** [to_file ~pretty_floats path t] print the json value [t] in file [path].
    If pretty_floats is set to [true], the floats inside [t] are truncated to 3
    decimals. If it is set to [false], floats are printed with full precision.
    The default is [false]. *)

val to_string : ?pretty_floats:bool -> t -> string
(** [to_string ~pretty_floats t] convert the json value [t] to a string.
    If pretty_floats is set to [true], the floats inside [t] are truncated to 3
    decimals. If it is set to [false], floats are printed with full precision.
    The default is [false]. *)

val pp_gen : ?pretty_floats:bool -> Format.formatter -> t -> unit
(** [pp_gen ~pretty_floats fmt t] print the json value [t] in formatter [fmt].
    If pretty_floats is set to [true], the floats inside [t] are truncated to 3
    decimals. If it is set to [false], floats are printed with full precision.
    The default is [false]. *)

val pretty : Format.formatter -> t -> unit
(** [pretty fmt t] print the json value [t] in formatter [fmt], truncating
    floats inside [t] to 3 decimals. *)

val print :  Format.formatter -> t -> unit
(** [print fmt t] print the json value [t] in formatter [fmt]. Floats inside
    [t] are printed with full precision. *)

val is_empty : t -> bool

val jint : t -> int
val jbool : t -> bool
val jfloat : t -> float
val jstring : t -> string
val jstringlist : t -> string list
val jlist : t -> t list
val jmap : (t -> 'a) -> t -> 'a list
val mfield : string -> t -> bool
val jfield : string -> t -> t
val jfield_exn : string -> t -> t
val jpath : string -> t -> t (* '/'-separated list of fields *)
val joption : (t -> 'a) -> t -> 'a option
val jdefault : 'a -> (t -> 'a) -> t -> 'a
val jmem : string -> t -> bool
val jiter : (string -> t -> unit) -> t -> unit

val int : int -> t
val bool : bool -> t
val string : string -> t
val assoc : ?keepnull:bool -> ?nullempty:bool -> (string * t) list -> t
val list : ?keepnull:bool -> ?nullempty:bool -> t list -> t
val list_map : ?keepnull:bool -> ?nullempty:bool -> ('a -> t) -> 'a list -> t
val option_map : ('a -> t) -> 'a option -> t
