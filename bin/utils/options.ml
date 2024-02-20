open Cmdliner

(* Common options *)

module Common = struct
  type t =
    { debug : Enums.DebugLvl.t
    ; colorless : bool
    }

  let debug =
    let open Enums.DebugLvl in
    let docv = "LEVEL" in
    let doc = "Debug level of the ECMA-SL application" in
    let levels = Arg.enum (args all) in
    Arg.(value & opt levels None & info [ "debug" ] ~doc ~docv)

  let colorless =
    let doc = "Generate colorless output." in
    Arg.(value & flag & info [ "colorless" ] ~doc)

  let options =
    let options' debug colorless = { debug; colorless } in
    Term.(const options' $ debug $ colorless)

  let set (copts : t) : unit =
    let open Ecma_sl in
    let open Enums.DebugLvl in
    Config.Common.warns := value copts.debug >= value Warn;
    Config.Common.debugs := value copts.debug >= value Full;
    Config.Common.colored := not copts.colorless
end

(* File options *)
module Fpath_ = struct
  let parse_fpath str is_something =
    let file = Fpath.v str in
    match is_something file with
    | Ok true -> `Ok file
    | Ok false -> `Error (Ecma_sl.Fmt.asprintf "File '%s' not found!" str)
    | Error (`Msg err) -> `Error err

  let non_dir_fpath =
    let parse str = parse_fpath str Bos.OS.File.exists in
    (parse, Fpath.pp)

  let fpath =
    let parse str = `Ok (Fpath.v str) in
    (parse, Fpath.pp)

  let input =
    let doc = "Name of the input file." in
    Arg.(required & pos 0 (some non_dir_fpath) None & info [] ~doc ~docv:"FILE")
end

module File = struct
  let input =
    let docv = "FILE" in
    let doc = "Name of the input file." in
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv)

  let inputs =
    let docv = "FILE/DIR" in
    let doc = "Name of the input file or input directory." in
    Arg.(required & pos 0 (some file) None & info [] ~doc ~docv)

  let output =
    let docv = "FILE" in
    let doc = "Name of the output file." in
    Arg.(value & opt (some string) None & info [ "o"; "output" ] ~doc ~docv)

  let harness =
    let docv = "FILE" in
    let doc = "Name of the harness file." in
    Arg.(value & opt (some non_dir_file) None & info [ "harness" ] ~doc ~docv)
end

(* Compile options *)

module Compile = struct
  let untyped =
    let doc =
      "Execute the ECMA-SL compiler without typechecking. In this mode, all \
       type annotations are disregarded."
    in
    Arg.(value & flag & info [ "untyped" ] ~doc)
end

(* Interpret options *)

module Interpret = struct
  let lang langs =
    let open Enums.Lang in
    let docv = "LANG" in
    let doc =
      "The language of the program to be interpreted. Options include: (1) \
       'auto' (default, inferring the language from the file extension); (2) \
       'esl' for ECMA-SL (.esl) files; (3) 'cesl' for Core ECMA-SL (.cesl) \
       files; and (4) 'cesl-unattached' for executing Core ECMA-SL (.cesl) \
       without certain restrictions imposed by the ECMA-SL compiler (e.g., \
       forced format of return values)."
    in
    let langs' = args langs in
    Arg.(value & opt (Arg.enum langs') Auto & info [ "lang" ] ~doc ~docv)

  let verbose =
    let doc =
      "Show intermediate interpreter details, encompassing the evaluation \
       process of statements and expressions throughout the execution."
    in
    Arg.(value & flag & info [ "verbose" ] ~doc)

  let verbose_at =
    let doc =
      "Include the source regions in verbose prints, revealing the location of \
       the statement/expression being evaluated during."
    in
    Arg.(value & flag & info [ "verbose-at" ] ~doc)

  let debugger =
    let doc =
      "Enable the Core ECMA-SL debugger. To utilize the debugger, insert a \
       breakpoint in the ECMA-SL (.esl) or Core ECMA-SL (.cesl) program, \
       indicated by a '#' preceding the statement."
    in
    Arg.(value & flag & info [ "db"; "debugger" ] ~doc)

  let main_func =
    let docv = "FUNC" in
    let doc =
      "The designated entry point function for the interpreter. Caution: \
       modifying this function can lead to unforeseen outcomes during \
       interpretation, as certain constraints enforced by the ECMA-SL compiler \
       (e.g., accesses to global variables) may be affected."
    in
    Arg.(value & opt string "main" & info [ "main" ] ~doc ~docv)

  let show_result =
    let doc =
      "Display the value returned by the top-level function, typically the \
       'main' function, at the end of the interpretation process."
    in
    Arg.(value & flag & info [ "show-result" ] ~doc)

  let hide_prints =
    let doc =
      "Ignore the prints within the ECMA-SL (.esl) or Core ECA-SL (.cesl) \
       program."
    in
    Arg.(value & flag & info [ "ignore-prints" ] ~doc)
end

(* Encode options *)

module Encode = struct
  let builder =
    let docv = "FUNC" in
    let doc =
      "The name of the function responsible for reconstructing the Abstract \
       Syntax Tree (AST) of the JavaScript program within the memory of \
       ECMA-SL."
    in
    Arg.(value & opt (some string) None & info [ "builder" ] ~doc ~docv)
end

(* Execute options *)

module Execute = struct
  let lang langs =
    let open Enums.Lang in
    let docv = "LANG" in
    let doc =
      "The language of the program to be executed. Options include: (1) 'auto' \
       (default, inferring the language from the file extension); (2) 'js' for \
       JavaScript (.js) files; and (3) 'cesl' for Core ECMA-SL (.cesl) files."
    in
    let langs' = Arg.enum (args langs) in
    Arg.(value & opt langs' Auto & info [ "lang" ] ~doc ~docv)

  let version =
    let open Enums.ECMARef in
    let docv = "ECMAREF" in
    let doc = "Version of the reference interpreter." in
    let versions = Arg.enum (args all) in
    Arg.(value & opt versions Main & info [ "interp" ] ~doc ~docv)
end

(* Other options - TODO *)

let target_func =
  let doc = "The start function of the analysis." in
  Arg.(value & opt string "main" & info [ "target"; "t" ] ~doc)

let workspace_dir =
  let doc = "The workspace directory for the results of the analysis." in
  Arg.(
    value
    & opt Fpath_.fpath (Fpath.v "ecma-out")
    & info [ "workspace"; "w" ] ~doc )

let testsuit_dir =
  let doc = "Search $(docv) for concrete testsuites to validate." in
  Arg.(required & pos 1 (some Fpath_.fpath) None & info [] ~docv:"DIR" ~doc)
