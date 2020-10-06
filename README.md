# ECMA-SL

This project is composed of two sub-projects, each presented in its own folder:

- **JS2ECMASL** - a tool that transforms a JSON AST created using the [Esprima parser](https://esprima.org) and representing a JavaScript program in an ECMA-SL function that returns an ECMA-SL object containing the AST.
- **Heap2HTML** - a tool that creates an HTML representation of a JSON file. With this tool, we can visualise a Heap and navigate through its containing objects.

## Compile ECMA-SL to an executable

```
cd implementation
make
```

A file named `main.native` is created at the root of the project.

## Compilation of an ECMA-SL program written in Plus to one in Core

```
./main.native -i test/plus.esl -mode c -o test/core.esl
```

This example expects that a file named `plus.esl` exists in the folder `./test` and the compilation output is written to a file named `core.esl` put in the folder `./test`.

### Create the AST file

The `./test/plus.esl` file that already exists in this project imports another file that contains the AST of a JavaScript program that we want to evaluate.
To generate a new one, please follow the instructions in this [file](./JS2ECMASL/README.md).

## Interpretation of an ECMA-SL program written in Core

```
./main.native -i test/core.esl -mode ci
```

This example expects that a file named `core.esl` exists in the folder `./test`.
**Note:** executing this command will print an extensive list of logs to the command-line. To avoid this and to ease the analysis of the output, one should redirect it to a file. E.g., `./main.native -i test/core.esl -mode ci > output.txt`

To have the generated Heap written to a file, use the command-line flag `-h` followed by the name of the file:

```
./main.native -i test/core.esl -mode ci -h test/heap.json
```

## Generate the HTML representation of the resulting Heap

To visualise the generated Heap in HTML, that must be previously exported to a file (in example above, the Heap is written to `test/heap.json`), please follow the instruction in this [file](./Heap2HTML/README.md).

### Common Errors when interpreting an ECMA-SL program

TODO
