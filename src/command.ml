(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Command Line Parsing                                               --- *)
(* -------------------------------------------------------------------------- *)

let commands = ref []

let usage argv msg =
  Array.iter (function
      | "-h" | "-help" | "--help" -> raise @@ Stdlib.Arg.Help msg
      | _ -> ()
    ) argv

let allargs argv xs =
  let all =
    Array.exists (function "-a" | "--all" -> true | _ -> false) argv in
  match xs with
  | x::_ when not all -> [x]
  | _ -> xs

let iter f = List.iter (fun (cmd,(args,_)) -> f cmd args) (List.rev !commands)

let get argv k msg =
  if k < Array.length argv then argv.(k) else failwith msg

let get_opt argv k =
  if k < Array.length argv then Some argv.(k) else None

let generate_or_suggest file gen =
  match open_out_gen [Open_wronly; Open_creat; Open_excl] 0o666 file with
  | exception Sys_error _ ->
    Format.printf "#### Suggested %s:@." (Filename.basename file) ;
    gen stdout ;
    Format.printf "####@."
  | out ->
    Utils.output_and_close out gen ;
    Format.printf "Generated %s@." (Utils.absolute file)

let var = Str.regexp "%{\\([a-zA-Z]+\\)}"

let template ~subst ~inc ~out =
  let apply subst text =
    try List.assoc (Str.matched_group 1 text) subst
    with Not_found -> Str.matched_group 0 text
  in
  let rec walk () =
    match Stdlib.input_line inc with
    | exception End_of_file -> ()
    | line ->
      let line' = Str.global_substitute var (apply subst) line in
      Stdlib.output_string out line' ;
      Stdlib.output_string out "\n" ;
      walk ()
  in walk ()

let template ~subst ~src ~tgt =
  Utils.mkdirs (Filename.dirname tgt) ;
  let inc = open_in src in
  generate_or_suggest tgt (fun out -> template ~subst ~inc ~out) ;
  close_in inc

let contains ~pattern ~text =
  let regexp = Str.regexp_string pattern in
  match Str.search_forward regexp text 0 with
  | exception Not_found -> false
  | _ -> true

let parse argv opts arg umsg =
  Arg.parse_argv argv (opts @ Debug.options @ [ Utils.show_progress_opt ]) arg umsg

let print_timings = Debug.create_flag "print-timings"

let () =
  let tstart = Unix.gettimeofday () in
  let handler () =
    let times = Unix.times () in
    let tend = Unix.gettimeofday () in
    Utils.flush ();
    Debug.debug ~flag:print_timings
      "@[<v>%t@,%-20s %10f@,%-20s %10f@]"
      Timer.print_timings
      "CPU time" (times.tms_utime +. times.tms_stime)
      "Real time" (tend -. tstart) ;
  in
  at_exit (fun () -> try handler () with _ -> ())

(* -------------------------------------------------------------------------- *)
(* --- Wrapper Command                                                    --- *)
(* -------------------------------------------------------------------------- *)

let verbose = ref false

let exec
    ?(cmd="why3")
    ?(auto=false)
    ?(drivers)
    ?(prefix=[])
    ?(pkgs=[])
    ?(why3_warn_off=[])
    ?(skip=0)
    argv =
  let args = ref [] in
  let pkgs = ref pkgs in
  let drivers = ref drivers in
  let why3_debug_flags = ref [] in
  let why3_warn_off = ref why3_warn_off in
  let p = ref skip in
  while !p < Array.length argv do
    begin
      match argv.(!p) with
      | "--debug" ->
        incr p ;
        if !p < Array.length argv then
          Debug.parse_and_set_flags argv.(!p)
        else
          failwith "missing --debug argument"
      | "--why3-debug" ->
        incr p ;
        if !p < Array.length argv then
          why3_debug_flags := argv.(!p) :: !why3_debug_flags
        else
          failwith "missing --why3-debug argument"
      | "--why3-warn-off" ->
        incr p ;
        if !p < Array.length argv then
          why3_warn_off := argv.(!p) :: !why3_warn_off
        else
          failwith "missing --why3-debug argument"
      | "-p" | "--package" ->
        incr p ;
        if !p < Array.length argv then
          pkgs := argv.(!p) :: !pkgs
        else
          failwith "missing PKG name"
      | "--drivers" when auto && !drivers = None -> drivers := Some []
      | "-v" | "--verbose" -> verbose := true
      | arg -> args := arg :: !args
    end ; incr p
  done ;
  let pkgs = Meta.find_all (List.rev !pkgs) in
  let drv =
    match !drivers with
    | None -> []
    | Some ds ->
      List.map (Printf.sprintf "--driver=%s") ds @
      List.concat @@ List.map
        (fun (pkg : Meta.pkg) ->
           List.map
             (Printf.sprintf "--driver=%s/%s" pkg.path)
             pkg.drivers
        ) pkgs in
  let load =
    List.map
      (fun (pkg : Meta.pkg) -> Printf.sprintf "--library=%s" pkg.path)
      pkgs in
  let debug =
    List.map (Printf.sprintf "--debug=%s") !why3_debug_flags
    @ List.map (Printf.sprintf "--warn-off=%s") !why3_warn_off in
  let argv =
    Array.of_list @@ List.concat [
      cmd::prefix ; debug ; [ "-L" ; "." ] ; load ; drv ; List.rev !args
    ] in
  if !verbose then
    begin
      Format.printf "%s" cmd ;
      for i = 1 to Array.length argv - 1 do
        Format.printf " %s" argv.(i)
      done ;
      Format.printf "@." ;
    end ;
  Unix.execvp cmd argv

let process cmd argv : unit =
  match List.assoc cmd !commands with
  | exception Not_found ->
    usage argv
      "USAGE:\n\
       \n  why3find CMD [ARGS...]\n\n\
       DESCRIPTION:\n\
       \n  Execute command \"CMD\" with wrapped arguments.\n\n\
       OPTIONS:\n\
       \n  --p|--package PKG : pass --library=<path> for the package\
       \n  --drivers : pass also --driver=<DRV> options\
       \n" ;
    exec ~cmd ~auto:true ~skip:1 argv
  | _,process -> process argv

let register ~name ?(args="") process =
  if List.mem_assoc name !commands then
    Utils.failwith "Duplicate command '%s'" name ;
  commands := (name,(args,process)) :: !commands

let noargv = Utils.failwith "Don't know what to do with %S"

(* -------------------------------------------------------------------------- *)
(* --- WHERE                                                              --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"where"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find where [-a|--all]\n\n\
         DESCRIPTION:\n\
         \n  Prints installation site(s).\
         \n" ;
      List.iter
        (fun site -> Format.printf "%s@\n" site)
        (allargs argv Global.Sites.packages)
    end

(* -------------------------------------------------------------------------- *)
(* --- SHARED                                                             --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"shared"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find shared [-a|--all]\n\n\
         DESCRIPTION:\n\
         \n  Prints shared resources site(s).\
         \n" ;
      List.iter
        (fun site -> Format.printf "%s@\n" site)
        (allargs argv Global.Sites.resources)
    end

(* -------------------------------------------------------------------------- *)
(* --- INIT                                                               --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"init"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find init PKG [DIR]\n\n\
         DESCRIPTION:\n\
         \n  Create templates for dune-project and git-ignore for package PKG.\
         \n  Files are created in directory DIR (default ./PKG).\
         \n" ;
      let pkg = get argv 1 "missing PKG name" in
      let dir =
        match get_opt argv 2 with
        | None -> pkg
        | Some dir -> dir in
      let subst = ["pkg",pkg] in
      template
        ~subst
        ~src:(Meta.shared "gitignore.template")
        ~tgt:(Filename.concat dir ".gitignore") ;
      template
        ~subst
        ~src:(Meta.shared "dune-project.template")
        ~tgt:(Filename.concat dir "dune-project") ;
      let src = Filename.concat dir pkg in
      Utils.mkdirs src ;
      Format.printf "Generated %s@." (Utils.absolute src) ;
      Format.printf "Next steps:@." ;
      Format.printf "  - go into package directory %s@." dir ;
      Format.printf "  - use why3find config [OPTIONS] to configure@." ;
      Format.printf "  - populate with %s/**/*.mlw source files@." pkg ;
      Format.printf "  - why3find prove [files...] from anywhere@." ;
      Format.printf "  - why3find doc|install from root package directory@." ;
      Format.printf "@\nPackage %s initialized in directory %s@." pkg dir ;
    end

(* -------------------------------------------------------------------------- *)
(* --- LIST                                                               --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"list"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find list\n\n\
         DESCRIPTION:\n\
         \n  Prints all installed packages.\
         \n" ;
      List.iter
        (fun site ->
           if Sys.file_exists site && Sys.is_directory site then
             Utils.readdir
               (fun pkg -> Format.printf "%s/%s@\n" site pkg)
               site
        ) Global.Sites.packages
    end

(* -------------------------------------------------------------------------- *)
(* --- QUERY                                                              --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"query" ~args:"[PKG...]"
    begin fun argv ->
      let path = ref false in
      let load = ref false in
      let deps = ref false in
      let query = ref [] in
      parse argv
        [ "-p", Arg.Set path, "print package paths only"
        ; "-L", Arg.Set load, "prints -L <path> for all dependencies"
        ; "-r", Arg.Set deps, "recursive mode, query also dependencies" ]
        (fun pkg -> query := pkg :: !query)
        "USAGE:\n\
         \n  why3find query [PKG...]\n\n\
         DESCRIPTION:\n\
         \n  Query why3 package location.\n\n\
         OPTIONS:\n" ;
      let pkgs = List.rev !query in
      let pkgs =
        if !deps || !load
        then Meta.find_all pkgs
        else List.map Meta.find pkgs in
      if !path then
        List.iter
          (fun (p : Meta.pkg) -> Format.printf "%s@\n" p.path)
          pkgs
      else if !load then
        List.iter
          (fun (p : Meta.pkg) -> Format.printf "-L %s@\n" p.path)
          pkgs
      else
        let pp_opt name ps =
          if ps <> [] then
            begin
              Format.printf "  @[<hov 2>%s:" name ;
              List.iter (Format.printf "@ %s") ps ;
              Format.printf "@]@\n" ;
            end in
        let pp_yes name fg =
          if fg then Format.printf "  %s: yes@\n" name in
        List.iter
          (fun (p : Meta.pkg) ->
             Format.printf "Package %s:@\n" p.name ;
             Format.printf "  path: %s@\n" p.path ;
             pp_opt "depends" p.depends ;
             pp_opt "drivers" p.drivers ;
             pp_yes "extracted" p.extracted ;
          ) pkgs
    end

(* -------------------------------------------------------------------------- *)
(* --- CONFIGURATION                                                      --- *)
(* -------------------------------------------------------------------------- *)

let pp_list =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
    Format.pp_print_string

let rec configuration configs provers =
  match configs , provers with
  | [] , ps -> List.map Prover.infoname ps
  | cs , [] -> List.map (Printf.sprintf "(?%s)") cs
  | c::cs , p::ps ->
    let cn = Prover.pattern_name c in
    let pn = Prover.name p in
    if cn <> pn then Printf.sprintf "(?%s)" c :: configuration cs provers
    else
      let q = Prover.fullname p in
      if c = q then q :: configuration cs ps else
        Prover.infoname p :: configuration cs ps

let check_custom_drivers main p =
  let check_driver (extra_dir, n) =
    let path = Why3.Driver.resolve_driver_name main "drivers" ~extra_dir n in
    let prefix = Utils.absolute (Why3.Whyconf.datadir main) in
    if not (String.starts_with ~prefix (Utils.absolute path)) then
      begin
        Log.warning
          "Prover %a has a custom driver %s. Modifying this driver or one of \
           its dependencies might leave the cache unsound, remember to clean \
           it with 'rm -rf .why3find/%a'."
          Prover.pp_prover p
          path
          Prover.pp_prover p;
      end in
  check_driver p.config.driver;
  List.iter (fun (d, l) -> List.iter (fun n -> check_driver (Some d, n)) l)
    p.config.extra_drivers

let () = register ~name:"config" ~args:"[OPTIONS] PROVERS"
    begin fun argv ->
      let open Fibers.Monad in
      let list = ref true in
      let calibrate = ref false in
      let velocity = ref false in
      let reset = ref false in
      let default = ref false in
      let detect = ref false in
      let strict = ref false in
      let relax = ref false in
      let check = ref false in
      let retval = ref 0 in
      let list_provers = ref false in
      let list_tactics = ref false in
      let update () = strict := true; relax := true in
      parse argv
        begin
          Wenv.options () @
          [
            "--master", Arg.Set calibrate, "calibrate provers (master)";
            "--velocity", Arg.Set velocity, "evaluate prover velocity (local)";
            "--quiet", Arg.Clear list, "do not list final configuration";
            "--reset", Arg.Set reset, "configure from scratch";
            "--default", Arg.Set default, "import available provers";
            "--detect", Arg.Set detect, "detect and import available provers";
            "--list-provers", Arg.Set list_provers, "list available provers";
            "--list-tactics", Arg.Set list_tactics, "list available tactics";
            "--strict", Arg.Set strict, "commit prover versions";
            "--relax", Arg.Set relax, "relax prover versions";
            "--update", Arg.Unit update, "update provers to latest version";
            "--check", Arg.Set check,
            "check that proof certificates conform to configuration" ;
          ]
        end
        noargv
        "USAGE:\n\
         \n  why3find config [OPTIONS]\n\n\
         DESCRIPTION:\n\
         \n  Configuration of the local package.\
         \n  By default, report on the current configuration.\
         \n\n\
         OPTIONS:\n" ;
      (* --- Why3 Config --- *)
      if !detect then ignore @@ Sys.command "why3 config detect" ;
      let rawconfig = Wenv.load_raw_config () in
      let config =
        if !reset
        then Config.default
        else Config.config_of_json rawconfig in
      let config = Wenv.add_options config in
      let env = Wenv.init config in
      (* --- List Provers -------------- *)
      if !list_provers || !list_tactics then
        begin
          if !list_provers then
            begin
              Format.printf "Available Provers:@." ;
              let allprovers = Prover.all env.why3 in
              if Prover.Mdesc.is_empty allprovers then
                Format.printf "  (none, use --detect)@." ;
              Prover.Mdesc.iter (fun dp _ ->
                  Format.printf " - %a@." Prover.pp_desc dp
                ) allprovers ;
            end ;
          if !list_tactics then
            begin
              Format.printf "Available Tactics:@." ;
              Tactic.iter
                (fun tac pp ->
                   Format.printf "@[<hov 3> - %s: %a@]@." tac
                     Why3.Pp.formatted pp
                ) ;
            end ;
          exit 0
        end;
      (* --- Packages ---- *)
      let pkgs = config.packages in
      if !list && pkgs <> [] then
        begin
          Format.printf "Package Dependencies:@." ;
          List.iter (Format.printf " - %s@.") pkgs ;
        end ;
      (* --- Provers ----- *)
      let fast = config.fast in
      let time = config.time in
      if fast > time then
        Log.warning
          "Fast proof time (%a) is larger than mean time (%a).@\n\
           This may lead to performance loss."
          Utils.pp_time fast Utils.pp_time time ;
      let patterns, provers =
        if !default || !detect then
          let provers = Prover.default env.why3 in
          let patterns =
            if !relax then List.map Prover.name provers else
              List.map Prover.fullname provers
          in
          patterns, provers
        else
          let config = config.provers in
          let patterns =
            if !relax then List.map Prover.pattern_name config else config in
          let provers = Prover.select env.why3 ~patterns in
          let patterns =
            if !strict then List.map Prover.fullname provers else
            if !relax then List.map Prover.name provers else
              config in
          patterns, provers
      in
      let pnames = configuration patterns provers in
      let profile =
        if !calibrate then
          Fibers.await @@ begin
            let+ p = Calibration.calibrate_provers ~saved:true env provers in
            Some (Calibration.to_json p)
          end
        else None in
      if not !calibrate && !velocity then
        Calibration.velocity_provers env provers ;
      (* --- Transformations ----- *)
      let depth = config.depth in
      let tactics = config.tactics in
      let preprocess = config.preprocess in
      (* --- Drivers ----- *)
      let drivers = config.drivers in
      (* --- Disabled warnings ----- *)
      let warnoff = config.warnoff in
      (* --- Check ----- *)
      if !check then
        begin
          let rec check_crc crc =
            match crc.Crc.status with
            | Stuck -> ()
            | Prover (p, _) ->
              if Prover.check_and_get_prover env.why3 ~patterns p = None then
                retval := 1;
            | Tactic { children; _ } -> List.iter check_crc children in
          let check f =
            let iter = Why3.Wstdlib.Mstr.iter in
            iter (fun _ -> iter (fun _ -> check_crc)) @@ snd @@ Proofs.load f
          in
          Wenv.allfiles check "."
        end ;
      let wmain = Why3.Whyconf.get_main env.why3.config in
      List.iter (check_custom_drivers wmain) provers ;
      (* --- New Configuration --------- *)
      let newprofile = match profile with None -> config.profile | _ -> profile in
      let newconfig = Config.{
          fast ; time ; depth ;
          packages = pkgs ;
          provers = patterns ;
          profile = newprofile ;
          tactics ;
          drivers ;
          warnoff ;
          preprocess ;
        } in
      let modified = Config.config_to_json newconfig <> rawconfig in
      (* --- Printing Final Config -------------- *)
      if !list then
        begin
          let jobs = Runner.get_jobs wmain in
          Format.printf "Configuration:@." ;
          Format.printf " - runner: %d jobs" jobs ;
          if fast <> 0.2 then Format.printf ", %a" Utils.pp_time fast ;
          Format.printf ", %a@." Utils.pp_time time ;
          if pnames <> [] then
            Format.printf " - @[<hov 2>provers: %a@]@." pp_list pnames ;
        end ;
      if !list && tactics <> [] then
        Format.printf " - @[<hov 2>tactics: %a@ (depth %d)@]@."
          pp_list tactics depth;
      if !list && preprocess <> None then
        Format.printf " - @[<hov 2>preprocessing tactic: %s@]@."
          (Option.get preprocess);
      if !list && drivers <> [] then
        Format.printf " - @[<hov 2>drivers: %a@]@." pp_list drivers ;
      (* --- Updating -------------- *)
      if !reset || modified then Wenv.save newconfig;
      Wenv.update_why3_config env.why3.config ;
      exit (!retval)
    end

(* -------------------------------------------------------------------------- *)
(* --- PROVE                                                              --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"prove" ~args:"[OPTIONS] PATH..."
    begin fun argv ->
      let files = ref [] in
      let goals = ref [] in
      let ide = ref false in
      let session = ref false in
      let results = ref `Default in
      let mode = ref `Update in
      let hammer = ref 4 in
      let axioms = ref false in
      let details = ref false in
      let set m v () = m := v in
      let add r p = r := p :: !r in
      let context n = results := `Context n in
      let failed () = context 2 in
      parse argv
        begin
          Wenv.options () @
          [
            "-g", Arg.String (add goals), "<name> goals selection";
            "-f", Arg.Unit (set mode `Force), "force rebuild proofs";
            "-u", Arg.Unit (set mode `Update), "update proofs (default)";
            "-r", Arg.Unit (set mode `Replay), "replay proofs (no update)";
            "-m", Arg.Unit (set mode `Minimize), "minimize proofs (or update)";
            "-x", Arg.Unit failed, "show context on failed proof (tty only)";
            "-X", Arg.Unit (set results `Tasks), "show task on failed proof" ;
            "-i", Arg.Set ide, "run Why3 IDE on failed proof (implies -s)";
            "-s", Arg.Set session, "save why3 session";
            "-l", Arg.Set details, "performance details (cache, prover, tactics)";
            "-a", Arg.Set axioms, "report axioms and parameters";
            "-H", Arg.Set_int hammer, "[0-4] hammer strategy level (default 4)";
          ] @
          Client.options @ [
            "--modules",  Arg.Unit (set results `Modules), "list results by module";
            "--theories", Arg.Unit (set results `Theories), "list results by theory";
            "--goals", Arg.Unit (set results `Goals), "list results by goals";
            "--proofs", Arg.Unit (set results `Proofs), "list proofs by goals";
            "--context", Arg.Int context, "<n> show n-lines context on failed proof (tty only, default 2)";
            "--stdlib", Arg.Set Prove.stdlib, "report hypotheses from stdlib";
            "--extern", Arg.Set Prove.externals, "report also external symbols";
            "--builtin", Arg.Set Prove.builtins, "report also builtin symbols";
          ]
        end
        (add files)
        "USAGE:\n\
         \n  why3find prove [OPTIONS] PATH...\n\n\
         DESCRIPTION:\n\
         \n  Prove all why3 files and directories accessible from PATH.\n\n\
         OPTIONS:\n" ;
      let mode = !mode in
      let level = !hammer in
      let session = !session || !ide in
      let config = Wenv.load_config () in
      let env = Wenv.init config in
      let files = if !files = [] then ["."] else !files in
      let files = Wenv.argfiles @@ List.rev files in
      let gnames = List.rev !goals in
      let result = Prove.prove_files
          ~env ~mode ~level ~session
          ~results:!results ~axioms:!axioms ~details:!details
          ~files ~gnames in
      let n = List.length result.unfixed in
      if n > 0 then
        begin
          Log.error "%d unproved file%a" n Utils.pp_s n ;
          if not !ide then exit 1 ;
          let file = List.hd result.unfixed in
          let pkgs = env.config.packages in
          let why3_warn_off = env.config.warnoff in
          let Prove.{ provers ; time ; mem ; tactics } = result in
          let cfg = ".why3find/hammer.cfg" in
          Hammer.why3_ide_config ~tactics ~provers ~time ~mem cfg ;
          Format.printf "Fixing %s@." file ;
          exec ~prefix:["ide"] ~pkgs ~why3_warn_off [| "--extra-config" ; cfg ; file |]
        end
    end

(* -------------------------------------------------------------------------- *)
(* --- DOC                                                                --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"doc" ~args:"[OPTIONS] PATH..."
    begin fun argv ->
      let files = ref [] in
      let title = ref "" in
      let out = ref "" in
      let url = ref false in
      let package_url = ref None in
      parse argv
        begin
          Wenv.options ~packages:true ~drivers:true () @ [
            "-t", Arg.Set_string title, "TITLE document title (default none)" ;
            "-o", Arg.Set_string out,
            "DIR destination directory (default \"html\")" ;
            "-u", Arg.Set url, "output generated URI" ;
            "--url", Arg.String (fun s -> package_url := Some s) ,
            "URL prefix URL for external packages."
          ]
        end
        (fun f -> files := f :: !files)
        "USAGE:\n\
         \n  why3find doc [OPTIONS] PATH...\n\n\
         DESCRIPTION:\n\
         \n  Generate HTML documentation.\
         \n\
         \n  Includes all why3 sources and markdown pages\
         \n  accessible from PATH.\n\n\
         \n\
         OPTIONS:\n" ;
      let title = !title in
      let files = if !files = [] then ["."] else !files in
      let files = Wenv.argfiles ~exts:[".md";".mlw"] @@ List.rev files in
      let out = if !out = "" then "html" else Wenv.arg1 !out in
      let config = Wenv.load_config () in
      let env = Wenv.init config in
      Docgen.generate ~env ~out ~title ~files ~url:!url ~package_url:!package_url
    end

(* -------------------------------------------------------------------------- *)
(* --- PRINT                                                              --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"print" ~args:"[OPTIONS] PATH..."
    begin fun argv ->
      let files = ref [] in
      parse argv
        (Wenv.options ~packages:true ())
        (fun f -> files := f :: !files)
        "USAGE:\n\
         \n  why3find print [OPTIONS] PATH...\n\n\
         DESCRIPTION:\n\
         \n  Print Why3 theories and modules internal representation.\
         \n\
         OPTIONS:\n" ;
      let files = if !files = [] then ["."] else !files in
      let files = Wenv.argfiles ~exts:[".md";".mlw"] @@ List.rev files in
      let config = Wenv.load_config () in
      let env = Wenv.init config in
      List.iter
        begin fun file ->
          let lib = snd @@ Wutil.filepath file in
          Wutil.load_theories env.why3.env file |> fst |>
          List.iter
            begin fun (thy : Why3.Theory.theory) ->
              let fullpath = lib @ [thy.th_name.id_string] in
              Format.printf
                "(* -------------------------------------------- *)@\n\
                 (* --- %-36s --- *)@\n\
                 (* -------------------------------------------- *)@\n@."
                (String.concat "." fullpath) ;
              match Why3.Pmodule.restore_module thy with
              | pm ->
                Format.printf "%a@\n@." Wutil.pp_module pm
              | exception Not_found ->
                Format.printf "%a@\n@." Wutil.pp_theory thy
            end
        end files
    end

(* -------------------------------------------------------------------------- *)
(* --- EXTRACT                                                            --- *)
(* -------------------------------------------------------------------------- *)

let re_module = Str.regexp "\\([a-zA-Z0x-9_]+.\\)+[A-Z][a-zA-Z0-9_]*"

let () = register ~name:"extract" ~args:"[OPTIONS] MODULE..."
    begin fun argv ->
      let lib = ref false in
      let pkg = ref "" in
      let modules = ref [] in
      let libraries = ref [] in
      let out = ref "lib" in
      parse argv
        begin
          Wenv.options ~packages:true ~drivers:true () @ [
            "--lib", Arg.Set lib,
            "Generate PKG.lib library instead of PKG";
            "-l", Arg.String (fun d -> libraries := d :: !libraries),
            "PKG Additional OCaml library dependency";
            "-o", Arg.Set_string out,
            "destination directory (default \"lib\")" ;
            "-v", Arg.Set verbose,
            "print why3 extract command" ;
          ]
        end
        (fun m ->
           if not @@ Str.string_match re_module m 0 then
             Utils.failwith "%S is not a module name" m ;
           let mp = List.hd @@ String.split_on_char '.' m in
           if !pkg = "" then
             pkg := mp
           else
           if !pkg <> mp then
             Utils.failwith "%S is not in package %S" m !pkg ;
           modules := m :: !modules)
        "USAGE:\n\
         \n  why3find extract [OPTIONS] MODULE...\n\n\
         DESCRIPTION:\n\
         \n  Extract OCaml and generate Dune file.\
         \n\n\
         OPTIONS:\n" ;
      if !pkg = "" then failwith "Nothing to extract" ;
      let config = Wenv.load_config () in
      let pkgs = config.packages in
      let drivers = config.drivers in
      let libraries =
        List.rev !libraries @
        List.filter (fun pkg -> (Meta.find pkg).extracted) pkgs in
      Utils.rmpath !out ;
      Utils.mkdirs !out ;
      let dune = Filename.concat !out "dune" in
      Utils.formatfile ~file:dune begin fun fdune ->
        Format.fprintf fdune ";; Generated by why3find extract@\n(library" ;
        let name = if !lib then !pkg ^ "__lib" else !pkg in
        let public = if !lib then !pkg ^ ".lib" else !pkg in
        Format.fprintf fdune
          "@\n  (name %s)@\n  (public_name %s)@\n  (wrapped false)"
          name public ;
        if libraries <> [] then
          begin
            Format.fprintf fdune "@\n  (@[<hov 2>libraries" ;
            List.iter (Format.fprintf fdune "@ %s") @@ libraries ;
            Format.fprintf fdune "@])" ;
          end ;
        Format.fprintf fdune ")@\n"
      end ;
      Format.printf "Generated %s@." (Utils.absolute dune) ;
      let prefix = ["extract";"-D";"ocaml64";"-o";!out;"--modular"] in
      let argv = Array.of_list @@ List.rev !modules in
      exec ~prefix ~pkgs ~drivers argv
    end

(* -------------------------------------------------------------------------- *)
(* --- LSP                                                                --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"lsp" ~args:"[OPTIONS]"
    begin fun argv ->
      let transport = ref None in
      let set_port n = transport := Some (Rpc.PORT n) in
      let set_pipe s = transport := Some (Rpc.PIPE s) in
      Arg.parse_argv argv [
        "--pipe", Arg.String set_pipe, "<SOCKFILE> unix linux socket" ;
        "--port", Arg.Int set_port, "<PORT> use socket port" ;
        "--socket", Arg.Int set_port, "<PORT> use socket port (same as --port)" ;
      ] (Utils.failwith "Unexpected argument %S")
        "USAGE:\n\
         \n  why3find lsp [--pipe=<SOCKFILE>]\n\n\
         DESCRIPTION:\n\
         \n  Launch the Language Server Protocol on the specified socket.\
         \n\n\
         OPTIONS:\n" ;
      match !transport with
      | None -> Log.error "No transport protocol specified." ; exit 1
      | Some transport ->
        Lspcc.init () ;
        let server = Lsp.establish transport in
        while Lsp.running server || Fibers.pending () > 0 do
          Fibers.yield () ;
          if not @@ Lsp.process server then Unix.sleepf 0.01 ;
        done
    end

(* -------------------------------------------------------------------------- *)
(* --- DAP                                                                --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"dap" ~args:"[OPTIONS]"
    begin fun argv ->
      let transport = ref None in
      let set_port n = transport := Some (Rpc.PORT n) in
      let set_pipe s = transport := Some (Rpc.PIPE s) in
      let add_list xs p = xs := p :: !xs in
      let files = ref [] in
      let goals = ref [] in
      Arg.parse_argv argv [
        "--pipe", Arg.String set_pipe, "<SOCKFILE> unix linux socket" ;
        "--port", Arg.Int set_port, "<PORT> use socket port" ;
        "--socket", Arg.Int set_port, "<PORT> use socket port (same as --port)" ;
        "--goal" , Arg.String (add_list goals) , "<Thy|Thy.val> filter goals" ;
      ] (add_list files)
        "USAGE:\n\
         \n  why3find dap [--pipe=<SOCKFILE>]\n\n\
         DESCRIPTION:\n\
         \n  Launch the Debug Adapter Protocol on the specified socket.\
         \n\n\
         OPTIONS:\n" ;
      match !transport with
      | None -> Log.error "No transport protocol specified." ; exit 1
      | Some transport ->
        Debugger.init (List.rev !goals) (List.rev !files) ;
        Debugger.run @@ Dap.establish transport ;
    end

(* -------------------------------------------------------------------------- *)
(* --- INSTALL                                                            --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"install" ~args:"PKG PATH..."
    begin fun argv ->
      let args = ref [] in
      let dune = ref true in
      let lib = ref "lib" in
      let html = ref "html" in
      let nodoc () = html := "" in
      parse argv [
        "--dune", Arg.Set dune,"Generate dune installer (default)" ;
        "--global", Arg.Clear dune,"Install in global repository (why3find where)" ;
        "--lib", Arg.Set_string lib,"DIR extraction directory (why3find extract -o DIR)";
        "--doc", Arg.Set_string html,"DIR doc output directory (why3find doc -o DIR)";
        "--no-doc", Arg.Unit nodoc,"Do not install documentation";
      ] (fun f -> args := f :: !args)
        "USAGE:\n\
         \n  why3find install [OPTIONS] PKG PATH...\n\n\
         DESCRIPTION:\n\n\
         \n  Install the package PKG at the topmost installation site.\
         \n\
         \n  Package dependencies and configuration are taken from the\
         \n  local project, or from the command line:\
         \n\
         \n    PKG/**         all why3 source files\
         \n    PKG/**/*.mlw   specified why3 source files\
         \n    **/*.drv       OCaml extraction drivers\
         \n\
         \n  If no source file is given, all why3 source files\
         \n  in directory PKG will be installed.\
         \n\
         \n  Unless --no-doc is specified, documentation in './html'\
         \n  directory is also installed.\
         \n\n\
         OPTIONS:\n" ;
      let pkg,paths =
        match List.rev !args with
        | pkg::paths -> pkg, Wenv.argv paths
        | [] -> failwith "Missing PKG name" in
      let dune = !dune in
      let path = if dune then "." else Meta.path pkg in
      if not dune && Sys.file_exists path then
        Utils.rmpath path ;
      if not dune then Utils.mkdirs path ;
      let dunefiles = ref [] in
      let log ~kind src = Format.printf "install %-10s %s@." kind src in
      let install ~kind ?tgt src =
        let tgt = match tgt with Some p -> p | None -> src in
        log ~kind tgt ;
        if dune
        then dunefiles := (src,tgt) :: !dunefiles
        else Utils.copy ~src ~tgt:(Filename.concat path tgt)
      in
      let allsrc = ref true in
      let pkg_prefix = String.starts_with ~prefix:(Filename.concat pkg "") in
      let install_src ?(check=true) src =
        begin
          if check && not (pkg_prefix src) then
            Utils.failwith "Can not install %S from outside of %s directory"
              src pkg ;
          allsrc := false ;
          Wenv.allfiles (install ~kind:"(source)") src ;
          let proofs = Proofs.filename src in
          if Sys.file_exists proofs then
            install ~kind:"(proof)" proofs ;
        end in
      let config = Wenv.load_config () in
      ignore (Wenv.init config); (* So that all extensions get loaded *)
      let drivers = ref [] in
      let exts = Wutil.exts () in
      List.iter
        begin fun src ->
          if not @@ Sys.file_exists src then
            Utils.failwith "unknown file or directory %S" src ;
          if Sys.is_directory src then
            Wenv.allfiles install_src src
          else
            match Filename.extension src with
            | ".cfg" -> Log.warning "won't install configuration file %S" src
            | ".drv" -> drivers := src :: !drivers
            | ext when List.mem ext exts  -> install_src src
            | _ -> Utils.failwith "don't know what to do with %S" src
        end paths ;
      let depends = config.packages in
      let drivers = !drivers @ config.drivers in
      if !allsrc then Wenv.allfiles (install_src ~check:false) pkg ;
      List.iter (install ~kind:"(driver)") drivers ;
      let doc = !html in
      if doc <> "" then
        begin
          let rec install_doc src tgt =
            if Sys.file_exists src then
              if Sys.is_directory src then
                Utils.readdir (fun d ->
                    let src = Filename.concat src d in
                    let tgt = Filename.concat tgt d in
                    install_doc src tgt
                  ) src
              else
                install ~kind:"(html)" ~tgt src
          in install_doc doc "html" ;
        end ;
      let lib = !lib in
      let haslib = ref false in
      if Sys.file_exists lib && Sys.is_directory lib then
        begin
          Utils.readdir (fun d ->
              let src = Filename.concat lib d in
              if Filename.check_suffix src ".ml" then haslib := true ;
            ) lib
        end ;
      let extracted = !haslib in
      if dune && extracted then log ~kind:"(dune)" "extracted code" ;
      log ~kind:"(meta)" "META.json" ;
      Meta.install {
        name = pkg ; path ; depends ; drivers ; extracted ;
      } ;
      if dune then
        begin
          let meta = "META.json" in
          Format.printf "Generated %s@." (Utils.absolute meta);
          Utils.formatfile ~file:"dune.why3find"
            begin fun out ->
              Format.fprintf out ";; generated by why3find@\n" ;
              Format.fprintf out "(install@\n" ;
              Format.fprintf out "  (package %s)@\n" pkg ;
              Format.fprintf out "  (section (site (why3find packages)))@\n" ;
              Format.fprintf out "  (files@\n" ;
              List.iter
                (fun (src,tgt) ->
                   Format.fprintf out "    (%s as %s/%s)@\n" src pkg tgt)
                ((meta,meta) :: List.rev !dunefiles) ;
              Format.fprintf out "    ))@." ;
            end ;
          Format.printf "Generated %s@." (Utils.absolute "dune.why3find");
          let include_stanza = "(include dune.why3find)" in
          let already_included =
            try
              let dune = Utils.readfile ~file:"dune" in
              contains ~pattern:include_stanza ~text:dune
            with Sys_error _ -> false in
          if not already_included then
            generate_or_suggest "dune"
              (fun out -> output_string out (include_stanza ^ "\n"));
        end
      else
        begin
          Format.printf "Installed %s@." (Utils.absolute path);
          if extracted then (* Don't make it a real warning ? *)
            Format.eprintf "Warning: extracted code not installed (use dune)@." ;
        end
    end

(* -------------------------------------------------------------------------- *)
(* --- UNINSTALL                                                          --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"uninstall" ~args:"[PKG...]"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find uninstall [PKG...]\n\n\
         DESCRIPTION:\n\
         \n  Remove all specified packages from topmost installation site.\
         \n" ;
      for i = 1 to Array.length argv - 1 do
        let pkg = argv.(i) in
        let path = Meta.path pkg in
        if Sys.file_exists path then
          begin
            Format.printf "remove %s@." path ;
            Utils.rmpath path ;
          end
        else
          Log.warning "package %s not found" pkg
      done
    end

(* -------------------------------------------------------------------------- *)
(* --- COMPILE                                                            --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"compile" ~args:"[-p PKG] FILE"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find compile [OPTIONS] FILE\n\n\
         DESCRIPTION:\n\
         \n  Compile the given file(s) using why3 prove command.\n\n\
         OPTIONS:\n\
         \n  -v|--verbose print why3 command\
         \n  -p|--package PKG package dependency\
         \n";
      exec ~prefix:["prove";"--type-only"] ~skip:1 argv
    end

(* -------------------------------------------------------------------------- *)
(* --- IDE                                                                --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"ide" ~args:"[-p PKG] FILE"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find ide [OPTIONS] FILE\n\n\
         DESCRIPTION:\n\
         \n  Run why3 ide on the given file.\n\
         \n\
         OPTIONS:\n\
         \n  -v|--verbose print why3 command\
         \n  -p|--package PKG package dependency\
         \n";
      exec ~prefix:["ide"] ~skip:1 argv
    end

(* -------------------------------------------------------------------------- *)
(* --- REPLAY                                                             --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"replay" ~args:"[-p PKG] FILE"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find replay [OPTIONS] MODULE...\n\n\
         DESCRIPTION:\n\
         \n  Executes why3 replay with the specified arguments.\n\n\
         OPTIONS:\n\
         \n  -v|--verbose print why3 command\
         \n  -p|--package PKG package dependency\
         \n";
      exec ~prefix:["replay"] ~skip:1 argv
    end

(* -------------------------------------------------------------------------- *)
