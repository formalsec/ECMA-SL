# ECMA-SL Compiler/Interpreters (Reference & Symbolic)

## Installation

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

4. Build the interpreters and run the test suits:
```sh
dune build @install		# build the projects
dune build @runtest 	# run the test suits
```

5. Install the interpreters on your path:
```sh
dune install
```

<br>

## Execution (Reference Interpreter)

1. Compile the ECMA-SL (`.esl`) file to Core ECMA-SL (`.cesl`):
```sh
ECMA-SL -mode c -i <input.esl> -o <output.cesl>
```

1. Interpreter the Core ECMA.SL (`.cesl`) file:
```sh
ECMA-SL -mode ci -i <input.cesl>
```
