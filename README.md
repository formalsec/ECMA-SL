[![Build](https://github.com/formalsec/ECMA-SL/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/formalsec/ECMA-SL/actions/workflows/build.yml)

<h1 align="center", style="font-size: 40px">ECMA-SL Project</h1>

<p align="center">
  <a href="#about">About</a> •
  <a href="#installation">Installation</a> •
  <a href="#ecma-sl-execution">Execution</a> •
  <a href="#integrated-development-environment-ide">IDE</a> •
  <a href="#issues">Issues</a> •
  <a href="#license">License</a>
</p>

<br>

# About

ECMA-SL is a comprehensive platform designed for the specification and execution of the ECMAScript standard, commonly known as JavaScript.
The platform introduces an intermediate language, ECMA-SL, which
serves as a bridge between JavaScript and its execution environment.
This intermediate language is used to develop reference implementations of the ECMAScript standard that adhere to JavaScript's specification.

Key features of the platform include a JavaScript-to-ECMA-SL (JS2ECMA-SL) parser, allowing the conversion of JavaScript code into the ECMA-SL language.
Additionally, ECMA-SL incorporates a compiler from ECMA-SL to Core ECMA-SL, a simplified version of the intermediate language, as well as an interpreter for Core ECMA-SL.
The combination of these tools results in a mechanism to execute JavaScript programs using the reference interpreters for the language.

<br>
<br>



# Installation

The ECMA-SL platform is accessed through the `ecma-sl` application, which is written in the [OCaml](https://ocaml.org/) programming language.
To build this application, we employ [dune](https://github.com/ocaml/dune), a composable build system for OCaml.

1. Install [opam](https://opam.ocaml.org/doc/Install.html).
2. Bootstrap the OCaml compiler:
```sh
opam init
opam switch create ecma-sl 5.1.0
```

3. Install the library dependencies:
```sh
opam install . --deps-only
```

4. Build the application and run the available test suit:
```sh
dune build
dune runtest
```

5. Install the application on your path:
```sh
dune install
```

<br>
<br>



# ECMA-SL Reference Execution

The `ecma-sl` application provides the following commands, among others:

- `compile` to compile an ECMA-SL program (`.esl`) into Core ECMA-SL (`.cesl`)
- `interpret` to interpreter a Core ECMA-SL program
<!-- - `encode` to encode a JavaScript (`.js`) program in Core ECMA-SL (`.cesl`) -->
<!-- - `execute` to execute a JavaScript (`.js`) program using the reference interpreters for JavaScript -->
<!-- - `build` to create the reference interpreters for JavaScript -->
- ...

Use `ecma-sl --help` for more information on the overall application, or `ecma-sl <command> --help` for more information on a specific command.

<br>

## Compile and Interpret an ECMA-SL Program

- Compile an ECMA-SL `(.esl)` program into Core ECMA-SL (`.cesl`), and execute the Core ECMA-SL program:
```sh
ecma-sl compile <input.esl> -o <output-cesl>
ecma-sl interpret <output.cesl>
```

- Execute an ECMA-SL `(.esl)` program directly (the application extrapolates the language based on the file extension):
```sh
ecma-sl interpret <input.esl>
```

### Verbose / Debug Interpretation

- Execute an ECMA-SL `(.esl)` program in verbose mode (all interpretation steps are shown)
```sh
ecma-sl interpret <input.esl> --verbose
```

- Execute an ECMA-SL `(.esl)` program with the debug prompt (breakpoints can be added to `(.esl)` code by preceding the instruction with a `#` character):
```sh
ecma-sl interpret <input.esl> --db
```

<br>
<br>



# Integrated Development Environment (IDE)

## Syntax Highlighting for ECMA-SL

- **VSCode -** To start using the VSCode syntax highlighter for ECMA-SL, copy the [extension](extensions/ecmasl-vscode/) it into the <user home>/.vscode/extensions folder and restart VSCode.
- **Vim -** To start using the Vim syntax highlighter for ECMA-SL, refer to the [ecmasl-vim](https://github.com/formalsec/ecmasl-vim) project

<br>
<br>



# Issues

For the list containing all current issues, please consult our [issue-tracker](https://github.com/formalsec/ECMA-SL/issues).

<br>
<br>



# License

This project is licensed under the [GPL-3.0 License](https://www.gnu.org/licenses/gpl-3.0.en.html) - see the [LICENSE](LICENSE) file for details.
