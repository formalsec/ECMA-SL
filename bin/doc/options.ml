open Cmdliner

(* Common options *)

module Common = struct
  type t =
    { debug : bool
    ; colorless : bool
    }

  let debug =
    let doc = "Run and generate debug output." in
    Arg.(value & flag & info [ "debug" ] ~doc)

  let colorless =
    let doc = "Generate colorless output." in
    Arg.(value & flag & info [ "colorless" ] ~doc)

  let options =
    let options' debug colorless = { debug; colorless } in
    Term.(const options' $ debug $ colorless)

  let set (copts : t) : unit =
    let open Ecma_sl in
    Config.Common.colored := not copts.colorless;
    Log.on_debug := copts.debug
end

(* File options *)

module File = struct
  let input =
    let docv = "FILE" in
    let doc = "Name of the input file." in
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv)

  let output =
    let docv = "FILE" in
    let doc = "Name of the output file." in
    Arg.(value & opt (some string) None & info [ "o"; "output" ] ~doc ~docv)
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
    let docv = "LANG" in
    let doc =
      "The language of the program to be interpreted. Options include: (1) \
       'auto' (default, inferring the language from the file extension); (2) \
       'esl' for ECMA-SL (.esl) files; (3) 'cesl' for Core ECMA-SL (.cesl) \
       files; and (4) 'cesl-unattached' for executing Core ECMA-SL (.cesl) \
       without certain restrictions imposed by the ECMA-SL compiler (e.g., \
       forced format of return values)."
    in
    let langs' = Lang.args langs in
    Arg.(value & opt (Arg.enum langs') Lang.Auto & info [ "lang" ] ~doc ~docv)

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
end

(* Other options - TODO *)

let execution_lang =
  let open Lang in
  let doc = "Language of the program to be executed." in
  let langs = Lang.args [ Auto; JS; ESL; CESL ] in
  Arg.(value & opt (Arg.enum langs) Auto & info [ "lang" ] ~doc)

let ecmaref_version =
  let open Ecmaref in
  let doc = "Version of the reference interpreter." in
  let ecmaref_version_enum = Arg.enum [ latest; toyecma; ecmaref5; ecmaref6 ] in
  Arg.(value & opt ecmaref_version_enum Latest & info [ "ecmaref" ] ~doc)

let ecmaref_builder =
  let open Ecmaref in
  let doc = "Building options for the reference interpreter." in
  let ecmaref_builder_enum = Arg.enum [ never; if_missing; always ] in
  Arg.(value & opt ecmaref_builder_enum Never & info [ "ecmaref-builder" ] ~doc)

let encode_esl_flag =
  let doc = "Encode the program in ECMA-SL (.esl)." in
  Arg.(value & flag & info [ "esl" ] ~doc)

let target_func =
  let doc = "The start function of the analysis." in
  Arg.(value & opt string "main" & info [ "target"; "t" ] ~doc)

let builder_func =
  let doc = "Name of the function that builds the AST." in
  Arg.(value & opt string "Build_AST" & info [ "builder" ] ~doc)

let workspace_dir =
  let doc = "The workspace directory for the results of the analysis." in
  Arg.(value & opt string "ecma-out" & info [ "workspace"; "w" ] ~doc)

let testsuit_dir =
  let doc = "Search $(docv) for concrete testsuites to validate." in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"DIR" ~doc)
