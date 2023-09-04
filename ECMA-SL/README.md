# ECMA-SL Reference and Symbolic Interpreters

## Build from source

- Install [opam](https://opam.ocaml.org/doc/Install.html).
- Bootstrap the OCaml compiler:

```sh
opam init
opam switch create 4.14.0 4.14.0
```

- Then, install the library dependencies:

```sh
opam install . --deps-only
```

- Build and test:

```sh
dune build @install
```

- Install the interpreters on your path by running:

```sh
dune install
```
