open Ecma_sl
open Cmdliner

(* Common Options *)

module Common = struct
  let debug =
    let docs = Manpage.s_common_options in
    let docv = "LEVEL" in
    let doc =
      "Debug level used within the ECMA-SL application. Options include: (1) \
       'none' for hiding all ECMA-SL logs; (2) 'warn' [default] for showing \
       ECMA-SL warnings; and (3) 'full' to show all, including debug prints."
    in
    let levels = Arg.enum Debug_lvl.(args all) in
    Arg.(value & opt levels Warn & info [ "debug" ] ~docs ~doc ~docv)

  let colorless =
    let docs = Manpage.s_common_options in
    let doc =
      "Generate colorless output. This flag might be necessary for terminals \
       lacking 16-ANSI-color support."
    in
    Arg.(value & flag & info [ "colorless" ] ~docs ~doc)

  let set_options debug colorless =
    let open Debug_lvl in
    Log.Config.warns := value debug >= value Warn;
    Log.Config.debugs := value debug >= value Full;
    Font.Config.colored := not colorless

  let options = Term.(const set_options $ debug $ colorless)
end

(* File Options *)

module File = struct
  let parse_fpath str test_f =
    let file = Fpath.v str in
    match test_f file with
    | Ok true -> `Ok file
    | Ok false -> `Error (Fmt.asprintf "File '%s' not found!" str)
    | Error (`Msg err) -> `Error err

  let fpath = ((fun str -> `Ok (Fpath.v str)), Fpath.pp)
  let valid_fpath = ((fun str -> parse_fpath str Bos.OS.Path.exists), Fpath.pp)
  let non_dir_fpath = ((fun str -> parse_fpath str Bos.OS.File.exists), Fpath.pp)
  let dir_fpath = ((fun str -> parse_fpath str Bos.OS.Dir.exists), Fpath.pp)

  let input =
    let docv = "FILE" in
    let doc = "Name of the input file." in
    Arg.(required & pos 0 (some non_dir_fpath) None & info [] ~doc ~docv)

  let inputs =
    let docv = "FILE/DIR" in
    let doc = "Name of the input file or input directory." in
    Arg.(required & pos 0 (some valid_fpath) None & info [] ~doc ~docv)

  let output =
    let docv = "FILE" in
    let doc = "Name of the output file." in
    Arg.(value & opt (some fpath) None & info [ "o"; "output" ] ~doc ~docv)

  let harness =
    let docv = "FILE" in
    let doc = "Name of the harness file." in
    Arg.(value & opt (some non_dir_fpath) None & info [ "harness" ] ~doc ~docv)
end

(* Compile options *)

module Compile = struct
  let untyped =
    let doc =
      "Run the ECMA-SL compiler without performing static type checking. In \
       this mode, all type annotations are ignored."
    in
    Arg.(value & flag & info [ "untyped" ] ~doc)
end

(* Interpret options *)

module Interpret = struct
  let lang langs =
    let docv = "LANG" in
    let doc =
      "Language of the program to be interpreted. Options include: (1) 'auto' \
       [default] for inferring the language based on the file extension; (2) \
       'esl' for ECMA-SL (.esl) files; (3) 'cesl' for Core ECMA-SL (.cesl) \
       files; and (4) 'cesl-unattached' for executing Core ECMA-SL (.cesl) \
       without certain restrictions imposed by the ECMA-SL compiler, such as \
       predefined return value format."
    in
    let langs' = Arg.enum Lang.(args langs) in
    Arg.(value & opt langs' Auto & info [ "lang" ] ~doc ~docv)

  let verbose =
    let doc =
      "Show intermediate interpreter details, encompassing the evaluation \
       process of statements and expressions throughout the execution."
    in
    Arg.(value & flag & info [ "verbose" ] ~doc)

  let debugger =
    let doc =
      "Enable the ECMA-SL debugger. To open the debugger, insert a breakpoint \
       in the ECMA-SL (.esl) or Core ECMA-SL (.cesl) program. This is done by \
       preceding the statement with a '#' character."
    in
    Arg.(value & flag & info [ "db"; "debugger" ] ~doc)

  let main_func =
    let docv = "FUNC" in
    let doc =
      "Designated entry point function for the interpreter. Caution: modifying \
       this function can lead to unforeseen outcomes during interpretation, as \
       certain constraints enforced by the ECMA-SL compiler may be affected \
       (e.g., accesses to global variables)."
    in
    Arg.(value & opt string "main" & info [ "main" ] ~doc ~docv)

  let show_exitval =
    let doc =
      "Display the value returned by the top-level function, typically the \
       'main' function, at the end of the interpretation process."
    in
    Arg.(value & flag & info [ "show-exitval" ] ~doc)
end

(* Encode options *)

module Encode = struct
  let builder =
    let docv = "FUNC" in
    let doc =
      "Name of the function responsible for reconstructing the Abstract Syntax \
       Tree (AST) of the JavaScript program in the ECMA-SL memory."
    in
    Arg.(value & opt (some string) None & info [ "builder" ] ~doc ~docv)
end

(* Execute options *)

module Execute = struct
  let lang langs =
    let docv = "LANG" in
    let doc =
      "Language of the program to be executed. Options include: (1) 'auto' \
       [default] for inferring the language based on the file extension; (2) \
       'js' for JavaScript (.js) files; and (3) 'cesl' for Core ECMA-SL \
       (.cesl) files."
    in
    let langs' = Arg.enum Lang.(args langs) in
    Arg.(value & opt langs' Auto & info [ "lang" ] ~doc ~docv)

  let ecmaref =
    let docv = "ECMAREF" in
    let doc = "Version of the reference interpreter." in
    let ecmarefs = Arg.enum Ecmaref.(args all) in
    Arg.(value & opt ecmarefs Main & info [ "interp" ] ~doc ~docv)
end

(* Other options - TODO *)

let target_func =
  let doc = "The start function of the analysis." in
  Arg.(value & opt string "main" & info [ "target"; "t" ] ~doc)

let workspace_dir =
  let doc = "The workspace directory for the results of the analysis." in
  Arg.(
    value & opt File.fpath (Fpath.v "ecma-out") & info [ "workspace"; "w" ] ~doc )

let testsuit_dir =
  let doc = "Search $(docv) for concrete testsuites to validate." in
  Arg.(required & pos 1 (some File.fpath) None & info [] ~docv:"DIR" ~doc)
