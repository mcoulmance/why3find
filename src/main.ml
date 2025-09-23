(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Why3 find main entry point                                         --- *)
(* -------------------------------------------------------------------------- *)

let () = Log.set_log true
let () = Server.register ()
let () = Worker.register ()

let version () : unit =
  begin
    Format.printf "why3find v%s@." Version.version ;
    exit 0
  end

let help () : unit =
  begin
    Format.printf "why3find [-h|--help]@\n" ;
    Format.printf "why3find [-v|--version]@\n" ;
    Command.iter (fun cmd args ->
        if args = "" then
          Format.printf "why3find %s@\n" cmd
        else
          Format.printf "why3find %s %s@\n" cmd args
      ) ;
    Format.printf "why3find CMD [ARGS...]@\n" ;
    exit 0
  end

let main () =
  try
    Utils.flush () ;
    let n = Array.length Sys.argv in
    if n < 2 then help () else
      match Sys.argv.(1) with
      | "-v" | "-version" | "--version" -> version ()
      | "-h" | "-help" | "--help" | "help" -> help ()
      | cmd -> Command.process cmd (Array.sub Sys.argv 1 (n-1))
  with
  | Stdlib.Arg.Help msg ->
    Format.eprintf "%s@." msg ;
    exit 0
  | Stdlib.Arg.Bad msg ->
    Format.eprintf "why3find %s@." msg ;
    exit 1
  | Failure msg | Sys_error msg ->
    Format.eprintf "why3find: %s@." msg ;
    exit 1
  | Unix.Unix_error(err,_,arg) ->
    Format.eprintf "why3find: %s (%s)@."
      (Unix.error_message err) arg ;
    exit 1
  | Assert_failure(f,a,b) ->
    Format.eprintf "why3find: assertion failure (%s:%d:%d)@." f a b ;
    exit 2

let () = main ()

(* -------------------------------------------------------------------------- *)
