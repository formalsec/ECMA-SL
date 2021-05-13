OCAMLBUILDFLAGS=-use-ocamlfind

default:
	ocamlbuild ${OCAMLBUILDFLAGS} ecmasl.otarget

test:
	ocamlbuild ${OCAMLBUILDFLAGS} -pkgs oUnit test/String_Utils_test.byte
	./String_Utils_test.byte

clean:
	ocamlbuild ${OCAMLBUILDFLAGS} -clean
	rm -f ../JS2ECMA-SL/ast.esl
	rm -f ../JS2ECMA-SL/test262_ast.esl
	rm -f ../JS2ECMA-SL/test_ast.esl
	rm -f ES5_interpreter/ast.esl
	rm -f ES5_interpreter/core.esl
	rm -f ES5_interpreter/core.json
	rm -f ES5_interpreter/test.esl
	rm -f ES5_interpreter/test262.esl
	rm -f ES5_interpreter/test262_ast.esl
	rm -f ES5_interpreter/test_ast.esl
	rm -f final_heap.json
	rm -f heap.json
	rm -f output.esl
	rm -f output.json
	rm -f output.log
	rm -f result.txt
	rm -f results.md
	rm -f simple_tests_result.md
	rm -f test/main.js
	rm -f test/main262.js
	rm -f test262_tests_result.md
	rm -rf output
	rm *.byte

.PHONY: test