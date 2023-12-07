# ECMA-SL

This project is composed of three sub-projects, each presented in its own folder:

- **JS2ECMASL:** a tool that transforms a JSON AST created using the 
  [Esprima parser](https://esprima.org) and representing a JavaScript program 
  in an ECMA-SL function that returns an ECMA-SL object containing the AST.
- **Heap2HTML:** a tool that creates an HTML representation of a JSON file. 
  With this tool, we can visualise a Heap and navigate through its containing 
  objects.
- **ECMA-SL:** the ECMA-SL interpreter.

## ECMA-SL Syntax Highlighting

- [VSCode](https://github.com/luisloureiro-ist/ECMA-SL_VSCode)
- [Vim](https://github.com/formalsec/ecmasl-vim)

## Build and install ECMA-SL

```sh
cd ECMA-SL
dune build
dune install
```

We use the [dune](https://github.com/ocaml/dune) a A composable build system 
for OCaml to compile this project.

After installing ECMA-SL, an executable named `ECMA-SL` is installed in the 
system.

## Compilation of an ECMA-SL program written in Plus to one in Core

```sh
ECMA-SL -mode c -i ES5_interpreter/plus.esl -o ES5_interpreter/core.esl
```

This example expects that a file named `plus.esl` exists in the folder
`./ES5_interpreter` and the compilation output is written to a file
named `core.esl` put in the folder `./ES5_interpreter`.

### Create the AST file

The `./ES5_interpreter/plus.esl` file that already exists in this project
imports another file that contains the AST of a JavaScript program that
we want to evaluate. To generate a new one, please follow the instructions
in this [file](./JS2ECMA-SL/README.md).

## Interpretation of an ECMA-SL program written in the Core version

```
ECMA-SL -mode ci -i ES5_interpreter/core.esl
```

This example expects that a file named `core.esl` exists in the folder
`./ES5_interpreter`. **Note:** executing this command will print an extensive
list of logs to the command-line. To avoid this and to ease the analysis
of the output, one should redirect it to a file.
E.g., `ECMA-SL -mode ci -i ES5_interpreter/core.esl > output.txt`

To have the generated Heap written to a file, use the command-line flag
`-h` followed by the name of the file:

```
ECMA-SL -mode ci -i ES5_interpreter/core.esl -h heap.json
```

## Generate the HTML representation of the resulting Heap

To visualise the generated Heap in HTML, that must be previously exported to 
a file (in example above, the Heap is written to `heap.json`), please follow 
the instruction in this [file](./Heap2HTML/README.md).

## Testing the reference interpreter (BROKEN!!)

~~Some JS files are available in the directory `test/simple` with simple programs that one can use to test the implementation of the reference interpreter.~~

~~To automate the execution of these JS programs, a bash script file is provided: `implementation/exec_simple_tests.sh`:~~

- ~~e.g., `sh exec_simple_tests.sh`~~
- ~~as a result, a markdown file (`simple_tests_result.md`) is created containing~~
- ~~a simple report with the results of the execution.~~

## Test262

_test262_ is a test suite intended to check agreement between JavaScript 
implementations and ECMA-262, the ECMAScript Language Specification. The test 
suite contains thousands of individual tests, each of which tests some 
specific requirements of the ECMAScript Language Specification.

These tests are available in this project in the directory `test/test262/tests`.

We provide one bash script that automates the execution of these tests, either 
individually or in a batch: `JS-Interpreters/scripts/run_test262_tests.sh`.

```
Usage: run_test262_tests.sh [OPTION]... [-dfir]
  -d <dir>   Directory containing test files.
             All the tests available in the directory are executed.
  -f <file>  File to test.
  -i <file>  File containing the list of files to test.
  -r <dir>   Directory containing test files and/or directories.
             If the directories contain other directories, all the tests available in those directories are also executed.
  -s <dir>   Directory containing test files and/or directories.
             Executes all tests found in all levels of its sub-directories in parallel.
  -j <int>   The number of jobs with which to execute option -s.
  
  Options:
  -E         Enable logging to file the tests executed with errors. File is "errors.log"
  -F         Enable logging to file the failed tests. File is "failures.log"
  -O         Enable logging to file the passed tests. File is "oks.log"
```

**Note:** Every script execution creates a markdown file with a simple report 
of the tests executed. This markdown file is created in the directory 
`output/logs` and has the name `results_<dateTtime>.md` where `dateTtime` has 
the date and time of the execution. For more details, check the **Note 1** 
in the following subsection.

### Examples

- `./scripts/run_test262_tests.sh -f ../test/test262/tests/language/expressions/addition/S9.3_A1_T2.js` - compiles and interprets the JS program in the file `../test/test262/tests/language/expressions/addition/S9.3_A1_T2.js`. The file `output/result.txt` is created as a result of the execution containing all the logs returned.
- `./scripts/run_test262_tests.sh -d ../test/test262/tests/built-ins/Boolean/` - all the test files available in the directory _../test/test262/tests/built-ins/Boolean/_ are compiled and interpreted. Test files available in subdirectories **are not** considered.
- `./scripts/run_test262_tests.sh -r ../test/test262/tests/built-ins/Error/` - all the test files available in the directory _../test/test262/tests/built-ins/Error/_ are compiled and interpreted. Test files available in subdirectories **are** also considered.
- `./scripts/run_test262_tests.sh -s ../test/test262/tests/built-ins/Error/ -j 16` - all the test files available in the directory _../test/test262/tests/built-ins/Error/_ are compiled and interpreted with 16 parallel jobs. Test files available in subdirectories **are** also considered.
- `./scripts/run_test262_tests.sh -i file_with_list_of_tests.ext` - all the test files available as the contents of the file `file_with_list_of_tests.ext` are compiled and executed.
- `./scripts/run_test262_tests.sh -E -F -O -d ../test/test262/tests/language/expressions/` - all the test files available in the directory _../test/test262/tests/language/expressions/_ are compiled and interpreted and generates three extra files as a result:
  - `output/logs/oks_<dateTtime>.log` - text file containing the list of tests that were successfully executed;
  - `output/logs/failures_<dateTtime>.log` - text file containing the list of tests that were unsuccessfully executed because the returned value is not the one expected;
  - `output/logs/errors_<dateTtime>.log` - text file containing the list of tests that did not terminate execution due to a raised exception during the interpretation;
  - **Note 1:** if execution is performed at 16:45:23 of the day 06/11/2020 then `<dateTtime>` is equal to `061120T164523`;
  - **Note 2:** neither of these arguments is mandatory. We can pass just a single one of them or even none, like in the first four examples.

### Common Errors when interpreting an ECMA-SL program

TODO
