open Cmdliner
open Files

module CommonOpts = struct
  let debug =
    let docs = Manpage.s_common_options in
    let docv = "LEVEL" in
    let doc =
      "Debug level used within the ECMA-SL application. Options include: (1) \
       'none' for hiding all ECMA-SL logs; (2) 'warn' [default] for showing \
       ECMA-SL warnings; and (3) 'full' to show all, including debug prints."
    in
    let levels = Arg.enum Enums.DebugLvl.(args @@ all ()) in
    Arg.(value & opt levels Warn & info [ "debug" ] ~docs ~docv ~doc)

  let colorless =
    let docs = Manpage.s_common_options in
    let doc =
      "Generate colorless output. This flag might be necessary for terminals \
       lacking 16-ANSI-color support."
    in
    Arg.(value & flag & info [ "colorless" ] ~docs ~doc)
end

module FileOpts = struct
  let input =
    let docv = "FILE" in
    let doc = "Name of the input file." in
    Arg.(required & pos 0 (some non_dir_fpath) None & info [] ~docv ~doc)

  let inputs =
    let docv = "FILE/DIR" in
    let doc = "Name of the input file or input directory." in
    Arg.(required & pos 0 (some valid_fpath) None & info [] ~docv ~doc)

  let output =
    let docv = "FILE" in
    let doc = "Name of the output file." in
    Arg.(value & opt (some fpath) None & info [ "o"; "output" ] ~docv ~doc)
end

module CompileOpts = struct
  let untyped =
    let doc =
      "Run the ECMA-SL compiler without performing static type checking. In \
       this mode, all type annotations are ignored."
    in
    Arg.(value & flag & info [ "untyped" ] ~doc)
end

module CompileCmd = struct
  let sdocs = Manpage.s_common_options
  let doc = "Compiles an ECMA-SL program to Core ECMA-SL"

  let description =
    [| "Given an ECMA-SL (.esl) file, compiles the program to the Core ECMA-SL \
        (.cesl) language."
    |]

  let man = [ `S Manpage.s_description; `P (Array.get description 0) ]
  let man_xrefs = []

  let exits =
    List.append Cmd.Exit.defaults
      [ Cmd.Exit.info ~doc:"on application failure" 1
      ; Cmd.Exit.info ~doc:"on generic execution error" 2
      ; Cmd.Exit.info ~doc:"on compilation error" 3
      ]
end

module InterpretOpts = struct
  let lang =
    let docv = "LANG" in
    let doc =
      "Language of the program to be interpreted. Options include: (1) 'auto' \
       [default] for inferring the language based on the file extension; (2) \
       'esl' for ECMA-SL (.esl) files; (3) 'cesl' for Core ECMA-SL (.cesl) \
       files; and (4) 'cesl-unattached' for executing Core ECMA-SL (.cesl) \
       without certain restrictions imposed by the ECMA-SL compiler, such as \
       the predefined return value format: (success, value)."
    in
    let langs = Arg.enum Enums.Lang.(args Cmd_interpret.Options.langs) in
    Arg.(value & opt langs Auto & info [ "lang" ] ~docv ~doc)

  let tracer =
    let docv = "TRACER" in
    let doc =
      "Show the interpretation steps, including the evaluation of statements \
       and expressions. Options include: (1) 'none' [default] for no trace \
       information; (2) 'call' for tracing function calls and return values; \
       (3) 'step' for tracing ECMA-SL statements; (4) 'full' for tracing \
       ECMA-SL statements and expression evaluations; and (5) 'core' for \
       tracing Core ECMA-SL intermediate steps."
    in
    let tracers = Arg.enum Enums.InterpTracer.(args @@ all ()) in
    Arg.(value & opt tracers None & info [ "trace" ] ~docv ~doc)

  let tracer_loc =
    let doc =
      "Show the locations of every language construct under evaluation. This \
       option is only used when the trace mode is not set to 'none'."
    in
    Arg.(value & flag & info [ "trace-loc" ] ~doc)

  let tracer_depth =
    let doc =
      "Specifies the maximum call stack depth logged by the tracer. \
       Non-positive depth values are ignored by the command. This option is \
       only used when the trace mode is not set to 'none'."
    in
    Arg.(value & opt int 0 & info [ "trace-depth" ] ~doc)

  let debugger =
    let doc =
      "Enable the ECMA-SL debugger. The debug prompt will on encountering the \
       first  breakpoint. Breakpoints can be inserted in ECMA-SL (.esl) or \
       Core ECMA-SL (.cesl) code by preceding any statement with the '#' \
       character."
    in
    Arg.(value & flag & info [ "db"; "debugger" ] ~doc)

  let main =
    let docv = "FUNC" in
    let doc =
      "Designated entry point function for the interpreter. Caution: modifying \
       this function can lead to unforeseen outcomes during interpretation, as \
       certain constraints enforced by the ECMA-SL compiler may be affected \
       (e.g., accesses to global variables)."
    in
    Arg.(value & opt string "main" & info [ "main" ] ~docv ~doc)

  let exitval =
    let doc =
      "Display the value returned by the top-level function, typically the \
       'main' function, at the end of the program interpretation."
    in
    Arg.(value & flag & info [ "exitval" ] ~doc)
end

module InterpretCmd = struct
  let sdocs = Manpage.s_common_options
  let doc = "Interprets a Core ECMA-SL program"

  let description =
    [| "Given an ECMA-SL (.esl) or Core ECMA-SL (.cesl) file, executes the \
        program using the concrete interpreter for Core ECMA-SL. When provided \
        with an ECMA-SL (.esl) file, defaults to compiling the program into \
        Core ECMA-SL (.cesl) before execution, while keeping important \
        metadata regarding the original ECMA-SL source code."
     ; "Some of the options from the 'compile' command are also available when \
        interpreting an ECMA-SL (.esl) file. When running the interpreter \
        directly on a Core ECMA-SL file, these options are ignored."
    |]

  let man =
    [ `S Manpage.s_description
    ; `P (Array.get description 0)
    ; `P (Array.get description 1)
    ]

  let man_xrefs = [ `Page ("ecma-sl compile", 1) ]

  let exits =
    List.append Cmd.Exit.defaults
      [ Cmd.Exit.info ~doc:"on application failure" 1
      ; Cmd.Exit.info ~doc:"on generic execution error" 2
      ; Cmd.Exit.info ~doc:"on compilation error" 3
      ; Cmd.Exit.info ~doc:"on interpretation runtime error" 4
      ]
end

module EncodeOpts = struct
  let builder =
    let docv = "FUNC" in
    let doc =
      "Name of the function responsible for reconstructing the Abstract Syntax \
       Tree (AST) of the JavaScript program in the ECMA-SL memory. By default, \
       this function is called 'buildAST'"
    in
    Arg.(value & opt (some string) None & info [ "builder" ] ~docv ~doc)
end

module EncodeCmd = struct
  let sdocs = Manpage.s_common_options
  let doc = "Encodes a JavaScript program in Core ECMA-SL"

  let description =
    [| "Given a JavaScript (.js) file, encodes the program in the Core ECMA-SL \
        (.cesl) language. This is done through a two-stage process. First, \
        generate the Abstract Syntax Tree (AST) of the JavaScript program \
        using Esprima, an official JavaScript parser. Then, translate the \
        resulting AST into the Core ECMA-SL language (.cesl), introducing \
        minor adjustments to meet the expectations of ECMARef interpreters."
    |]

  let man = [ `S Manpage.s_description; `P (Array.get description 0) ]
  let man_xrefs = []

  let exits =
    List.append Cmd.Exit.defaults
      [ Cmd.Exit.info ~doc:"on application failure" 1
      ; Cmd.Exit.info ~doc:"on generic execution error" 2
      ; Cmd.Exit.info ~doc:"on encoding error" 5
      ]
end

module ExecuteOpts = struct
  let lang =
    let docv = "LANG" in
    let doc =
      "Language of the program to be executed. Options include: (1) 'auto' \
       [default] for inferring the language based on the file extension; (2) \
       'js' for JavaScript (.js) files; and (3) 'cesl' for Core ECMA-SL \
       (.cesl) "
    in
    let langs = Arg.enum Enums.Lang.(args Cmd_execute.Options.langs) in
    Arg.(value & opt langs Auto & info [ "lang" ] ~docv ~doc)

  let jsinterp =
    let docv = "INTERP" in
    let doc =
      "Version of the reference interpreter. Options include: (1) 'main' \
       [default] for the most complete interpreter; (2) 'latest' for the most \
       recent interpreter; (3) 'ecmaref5' for the ES5 reference interpreter; \
       (4) 'ecmaref6' for the ES6 reference interpreter; and (5) \
       'ecmaref6-sym' for the ES6 symbolic interpreter."
    in
    let interps = Arg.enum Enums.JSInterp.(args @@ all ()) in
    Arg.(value & opt interps Main & info [ "interp" ] ~docv ~doc)

  let harness =
    let docv = "FILE" in
    let doc = "Name of the JavaScript (.js) harness file." in
    Arg.(value & opt (some non_dir_fpath) None & info [ "harness" ] ~docv ~doc)
end

module ExecuteCmd = struct
  let sdocs = Manpage.s_common_options
  let doc = "Executes an encoded JavaScript program"

  let description =
    [| "Given an JavaScript program encoded in Core ECMA-SL (.cesl), executes \
        the program using a JavaScript interpreter. When provided with an \
        unencoded JavaScript (.js) program, defaults to encoding the program \
        into Core ECMA-SL (.cesl) before execution."
     ; "The JavaScript reference interpreter (ECMARef interpreters) are \
        written in ECMA-SL, and adhere to the JavaScript standard \
        line-by-line. The option '--interp' specifies the interpreter that \
        will be used to execute the program. Besides the ES5 and ES6 reference \
        interpreters, there are other interpreters available such as the \
        symbolic ES6 interpreter. Additionaly, the '--harness' flag can be \
        used to specify a JavaScript program that will be executed before the \
        main program."
     ; "Some of the options of the 'interpret' command are also available. \
        These include the ability to enable the ECMA-SL and debugger, show the \
        final result of the program, among others."
    |]

  let man =
    [ `S Manpage.s_description
    ; `P (Array.get description 0)
    ; `P (Array.get description 1)
    ; `P (Array.get description 2)
    ]

  let man_xrefs =
    [ `Page ("ecma-sl encode", 1); `Page ("ecma-sl interpret", 2) ]

  let exits =
    List.append Cmd.Exit.defaults
      [ Cmd.Exit.info ~doc:"on application failure" 1
      ; Cmd.Exit.info ~doc:"on generic execution error" 2
      ; Cmd.Exit.info ~doc:"on compilation error" 3
      ; Cmd.Exit.info ~doc:"on interpretation runtime error" 4
      ; Cmd.Exit.info ~doc:"on encoding error" 5
      ]
end

module TestCmd = struct
  let sdocs = Manpage.s_common_options
  let doc = "Executes an encoded JavaScript test"

  let description =
    [| "Given a JavaScript test file encoded in Core ECMA-SL (.cesl), executes \
        the test using the JavaScript interpreter. When provided with an \
        unencoded JavaScript (.js) test, defaults to encoding the test into \
        Core ECMA-SL (.cesl) before execution."
     ; "Running a JavaScript test is similar to executing a regular JavaScript \
        program with the following distinctions. All JavaScript logs (prints) \
        are discarded, and the test's final return value is analyzed to \
        determine if the test ran successfully or not. As a result, most of \
        the options from the 'execute' command are also available."
    |]

  let man =
    [ `S Manpage.s_description
    ; `P (Array.get description 0)
    ; `P (Array.get description 1)
    ]

  let man_xrefs = [ `Page ("ecma-sl execute", 1) ]

  let exits =
    List.append Cmd.Exit.defaults
      [ Cmd.Exit.info ~doc:"on application failure" 1
      ; Cmd.Exit.info ~doc:"on generic execution error" 2
      ]
end

module SymbolicOpts = struct
  let target =
    let docv = "FUNC" in
    let doc = "Designated entry point for the analysis." in
    Arg.(value & opt string "main" & info [ "target"; "t" ] ~docv ~doc)

  let workspace =
    let docv = "DIR" in
    let doc = "The workspace directory for the results of the analysis." in
    let default = Fpath.v "ecma-out" in
    Arg.(value & opt fpath default & info [ "workspace"; "w" ] ~docv ~doc)
end

module SymbolicCmd = struct
  (* Improve the command documentation *)
  let sdocs = Manpage.s_common_options
  let doc = "Performs symbolic analysis on an ECMA-SL program"

  let description =
    [| "Given an JavaScript (.js), ECMA-SL (.esl), or Core ECMA-SL (.cesl) \
        file, runs the program using the ECMA-SL symbolic engine."
    |]

  let man = [ `S Manpage.s_description; `P (Array.get description 0) ]
  let man_xrefs = []
  let exits = List.append Cmd.Exit.defaults []
end

module ReplayOpts = struct
  let testsuit =
    let docv = "DIR" in
    let doc = "The directory containing concrete testsuites to validate." in
    Arg.(required & pos 1 (some fpath) None & info [] ~docv ~doc)
end

module ReplayCmd = struct
  (* Improve the command documentation *)

  let sdocs = Manpage.s_common_options
  let doc = "Validates the symbolic testsuit"

  let description =
    [| "Replays concrete testsuites generated in symbolic execution." |]

  let man = [ `S Manpage.s_description; `P (Array.get description 0) ]
  let man_xrefs = []
  let exits = List.append Cmd.Exit.defaults []
end

module ExplodeJSCmd = struct
  (* Improve the command documentation *)

  let sdocs = Manpage.s_common_options
  let doc = "Explode.js symbolic vulnerability confirmation engine"
  let description = [| "Tries to blow stuff up" |]
  let man = [ `S Manpage.s_description; `P (Array.get description 0) ]
  let man_xrefs = []
  let exits = List.append Cmd.Exit.defaults []
end

module Application = struct
  let sdocs = Manpage.s_common_options
  let doc = "Executable specification of the ECMAScript standard"
  let version = "1.0.0"

  let description =
    [| "ECMA-SL is a comprehensive platform designed for the specification and \
        execution of the ECMAScript standard, commonly known as JavaScript. \
        The platform introduces an intermediate language, ECMA-SL, which \
        serves as a bridge between JavaScript and its execution environment. \
        This intermediate language is used to provide a reference \
        implementation of the ECMAScript standard that adheres to JavaScript's \
        specification."
     ; "Key features of the platform include a JavaScript-to-ECMA-SL \
        (JS2ECMA-SL) parser, allowing the conversion of JavaScript code into \
        the ECMA-SL language. Additionally, ECMA-SL incorporates a compiler \
        from ECMA-SL to Core ECMA-SL, a simplified version of the platform's \
        language, as well as an interpreter for Core ECMA-SL. By combining \
        these tools, one can execute a JavaScript program using the reference \
        interpreters for JavaScript."
     ; "Use ecma-sl <command> --help for more information on a specific \
        command."
    |]

  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P (Array.get description 0)
    ; `P (Array.get description 1)
    ; `P (Array.get description 2)
    ; `S Manpage.s_common_options
    ; `P "These options are common to all commands."
    ; `S Manpage.s_bugs
    ; `P "Check bug reports at https://github.com/formalsec/ECMA-SL/issues."
    ]

  let man_xrefs = []

  let exits =
    List.append Cmd.Exit.defaults
      [ Cmd.Exit.info ~doc:"on application failure" 1
      ; Cmd.Exit.info ~doc:"on generic execution error" 2
      ]
end
