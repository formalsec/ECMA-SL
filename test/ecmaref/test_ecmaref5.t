Tests compilation of ecmaref5:
  $ cd ../../JS-Interpreters
  $ ecma-sl compile ecmaref5/main.esl --untyped
  Line number: 161. File: ./section 15/section_15.5.esl
  ./section 15/section_15.5.esl:161:7, last token: EParser.ELSE: Error message found.
  ecma-sl: internal error, uncaught exception:
           EslSemantics.EParser.MenhirBasics.Error
           Raised at EslSemantics__EParsing.eparser.(fun) in file "ECMA-SL/semantics/extended/parser/eParsing.ml", line 23, characters 8-27
           Called from EslSemantics__EParsing.parse_eprog.(fun) in file "ECMA-SL/semantics/extended/parser/eParsing.ml", line 46, characters 10-62
           Called from EslSemantics__Preprocessor.Imports.load_dependency in file "ECMA-SL/semantics/extended/compiler/preprocessor.ml", line 8, characters 18-80
           Called from EslSemantics__Preprocessor.Imports.import_resolver.(fun) in file "ECMA-SL/semantics/extended/compiler/preprocessor.ml", line 42, characters 27-65
           Called from EslSemantics__Preprocessor.Imports.resolve_imports in file "ECMA-SL/semantics/extended/compiler/preprocessor.ml", line 58, characters 4-91
           Called from Dune__exe__Cmd_compile.compile_pipeline in file "bin/commands/cmd_compile.ml", line 20, characters 2-145
           Called from Dune__exe__Cmd_compile.compile.(fun) in file "bin/commands/cmd_compile.ml", line 37, characters 11-47
           Called from Dune__exe__Result.esl_exec in file "bin/result.ml", line 46, characters 6-15
           Called from Dune__exe__Cmd_compile.run in file "bin/commands/cmd_compile.ml", line 43, characters 11-42
           Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
           Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [125]
