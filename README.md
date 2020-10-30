# ECMA-SL

This project is composed of two sub-projects, each presented in its own folder:

- **JS2ECMASL** - a tool that transforms a JSON AST created using the [Esprima parser](https://esprima.org) and representing a JavaScript program in an ECMA-SL function that returns an ECMA-SL object containing the AST.
- **Heap2HTML** - a tool that creates an HTML representation of a JSON file. With this tool, we can visualise a Heap and navigate through its containing objects.

## Compile ECMA-SL to an executable

```
cd implementation
make
```

We use the [OCamlbuild](https://ocaml.org/learn/tutorials/ocamlbuild/) automated build system to compile this project.
Only the OCaml modules created and present in the `lib/` and `src/` directories and their subdirectories are considered.

After compiling ECMA-SL, a file named `main.native` is created at the root of the project.

## Compilation of an ECMA-SL program written in Plus to one in Core

```
./main.native -mode c -i ES5_interpreter/plus.esl -o ES5_interpreter/core.esl
```

This example expects that a file named `plus.esl` exists in the folder `./ES5_interpreter` and the compilation output is written to a file named `core.esl` put in the folder `./ES5_interpreter`.

### Create the AST file

The `./ES5_interpreter/plus.esl` file that already exists in this project imports another file that contains the AST of a JavaScript program that we want to evaluate.
To generate a new one, please follow the instructions in this [file](./JS2ECMA-SL/README.md).

## Interpretation of an ECMA-SL program written in Core

```
./main.native -mode ci -i ES5_interpreter/core.esl
```

This example expects that a file named `core.esl` exists in the folder `./ES5_interpreter`.
**Note:** executing this command will print an extensive list of logs to the command-line. To avoid this and to ease the analysis of the output, one should redirect it to a file. E.g., `./main.native -mode ci -i ES5_interpreter/core.esl > output.txt`

To have the generated Heap written to a file, use the command-line flag `-h` followed by the name of the file:

```
./main.native -mode ci -i ES5_interpreter/core.esl -h heap.json
```

## Generate the HTML representation of the resulting Heap

To visualise the generated Heap in HTML, that must be previously exported to a file (in example above, the Heap is written to `heap.json`), please follow the instruction in this [file](./Heap2HTML/README.md).

### Common Errors when interpreting an ECMA-SL program

TODO
