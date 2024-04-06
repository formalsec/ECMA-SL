Tests compilation of ecmaref5:
  $ cd ../../JS-Interpreters
  $ ecma-sl compile ecmaref5/main.esl --untyped
  ./esl_interpreter.esl:2649:96, last token: EParser.LBRACE: Error message found.
  ecma-sl: internal error, uncaught exception:
           EslSemantics.EParser.MenhirBasics.Error
           Raised at EslSemantics__EParsing.eparser.(fun) in file "ECMA-SL/semantics/extended/parser/eParsing.ml", line 23, characters 8-27
           Called from EslSemantics__EParsing.parse_eprog.(fun) in file "ECMA-SL/semantics/extended/parser/eParsing.ml", line 46, characters 10-62
           Called from EslSemantics__Preprocessor.Imports.load_dependency in file "ECMA-SL/semantics/extended/compiler/preprocessor.ml", line 8, characters 18-80
           Called from EslSemantics__Preprocessor.Imports.import_resolver.(fun) in file "ECMA-SL/semantics/extended/compiler/preprocessor.ml", line 40, characters 27-65
           Called from EslSemantics__Preprocessor.Imports.resolve_imports in file "ECMA-SL/semantics/extended/compiler/preprocessor.ml", line 56, characters 4-91
           Called from Dune__exe__Cmd_compile.compile_pipeline in file "bin/commands/cmd_compile.ml", line 22, characters 2-133
           Called from Dune__exe__Cmd_compile.compile in file "bin/commands/cmd_compile.ml", line 31, characters 13-40
           Called from Dune__exe__Cmd_compile.run in file "bin/commands/cmd_compile.ml", line 37, characters 13-31
           Called from Dune__exe__Cmd.eval_cmd in file "bin/commands/cmd.ml", line 36, characters 6-12
           Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
           Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
  Line number: 2649. File: ./esl_interpreter.esl
  [125]
