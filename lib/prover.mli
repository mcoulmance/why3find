(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Types representing provers and associated operations *)

(** {1 Prover descriptions} *)

type desc
(** The type of prover descriptions, identifying a unique prover version. *)

exception InvalidPattern of string
exception InvalidProverDescription of string
(** Raised on invalid pattern or prover description, see below. The argument
    is the erroneous string. *)

val desc_to_string : desc -> string
val desc_of_string : string -> desc
(** Converts prover descriptions to and from string. The syntax is the same as
    patterns below, but the version part is mandatory.
    @raise InvalidProverDescription when the string description is invalid. *)

val desc_name : desc -> string
val desc_version : desc -> string
val desc : name:string -> ?version:string -> unit -> string

val pp_desc : Format.formatter -> desc -> unit

val compare_desc : desc -> desc -> int
(** Prover descriptions comparison function. Different provers are ordered
    alphabetically by names, and different version of a same prover by
    {b reverse} version order. Thus, when sorting, the latest version come
    {b before} older ones. *)

module Mdesc : Why3.Extmap.S with type key = desc

(** {1 Provers} *)

type prover = private {
  desc : desc ;
  relaxed : bool ;
  config : Why3.Whyconf.config_prover ;
  driver : Why3.Driver.driver ;
  digest : string ;
}
(** The type of provers.

    [digest] is an hexadecimal digest of the prover and its config. For now,
    drivers are not completely taken into account, for instance modifying a
    driver which is [import]ed into the main driver won't change the digest. *)

val why3_desc : prover -> string
(** The prover descriptions in Why3 syntax. *)

val name : prover -> string
val version : prover -> string
val fullname : prover -> string
val infoname : prover -> string
val relaxed : prover -> prover

val pp_prover : Format.formatter -> prover -> unit
(** Prints prover fullname. *)

(** {1 Patterns} *)

(** Patterns identify provers. The syntax is [prover-name@prover-version].
    The [prover-name] match the prover name case-insensitively. (eg [alt-ergo]
    match the prover of Why3 name [Alt-Ergo])
    The version part, starting from the '@', is optional. If omitted, it
    generally means the last available version. *)

val pattern_name : string -> string
val pattern_version : string -> string option
val pattern_of_string : string -> string * string option
(** @raise InvalidPattern when the pattern is invalid *)

(** {1 Retrieving available provers} *)

val all : Config.why3env -> prover Mdesc.t
(** Returns the set of all available provers for the given Why3 environment. *)

val select : Config.why3env -> patterns:string list -> prover list
(** Selects the provers that match one of the [patterns].
    For each [pattern] in order, if the pattern is invalid or the prover is not
    in the map, a warning is emitted, otherwise one prover is appended to the
    return list, either the one specified or the latest available version in
    case of a version-less pattern. *)

val default : Config.why3env -> prover list
(** Returns the set of available "default" provers. *)

val find :
  prover list ->
  name:string -> ?version:string ->
  unit -> prover option
(** Find some corresponding prover in the list.
    Emits a warning if not found. *)
