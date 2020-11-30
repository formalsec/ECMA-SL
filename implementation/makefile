OCAMLBUILDFLAGS=-use-ocamlfind

default:
	ocamlbuild ${OCAMLBUILDFLAGS} main.native

clean:
	ocamlbuild ${OCAMLBUILDFLAGS} -clean
	rm ../JS2ECMA-SL/ast.esl
	rm ../JS2ECMA-SL/test262_ast.esl
	rm ../JS2ECMA-SL/test_ast.esl
	rm ES5_interpreter/ast.esl
	rm ES5_interpreter/core.esl
	rm ES5_interpreter/core.json
	rm ES5_interpreter/test.esl
	rm ES5_interpreter/test262.esl
	rm ES5_interpreter/test262_ast.esl
	rm ES5_interpreter/test_ast.esl
	rm final_heap.json
	rm heap.json
	rm output.esl
	rm output.json
	rm output.log
	rm result.txt
	rm results.md
	rm simple_tests_result.md
	rm test/main.js
	rm test/main262.js
	rm test262_tests_result.md

