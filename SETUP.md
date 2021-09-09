# Setup

## Software to install

Arch Linux:

```sh
sudo pacman -Sy ocaml opam
opam install yojson
opam install containers
opam install menhir
opam install menhirLib
opam install extlib
opam install batteries
opam install ppx_import
opam install ppx_compare
opam install ppx_deriving
```

## Setting environment:
```sh
eval $(opam env)
# or
eval `opam env`
```

## Upgrade packages:
```sh
opam update
opam upgrade
```

### Text editor - VS Code

If using VS Code, install OCaml Platform extension for OCaml syntax highlighting.

For ECMA SL syntax highlighting, install extension made by Luís Loureiro:
1. Clone repository: https://github.com/luisloureiro-ist/ECMA-SL_VSCode
2. `cp -r ECMA-SL_VSCode/ ~/.vscode-oss/extensions`
3. Restart VS Code.

## Git repository

Original repo: https://github.com/FranciscoQuinaz/ECMA-SL

Fork the repository or create a new branch if you wish to work on it.

If forked, pull from the original repository with:
`git pull https://github.com/FranciscoQuinaz/ECMA-SL master`

Make a pull request to add changes to original repository.