(lang dune 3.2)

(using menhir 2.1)

(using dune_site 0.1)

(name ecma-sl)

(generate_opam_files true)

(source
 (github formalsec/ECMA-SL))

(authors "José Fragoso Santos")

(maintainers "José Fragoso Santos")

(license "GPL-3.0-or-later")

(documentation https://github.com/formalsec.github.io/ECMA-SL)

(package
 (name ecma-sl)
 (synopsis "ECMA-SL")
 (description
  "A platform for specifying and running the ECMAScript standard.")
 (depends
  (ocaml
   (>= "5.1.0"))
  (odoc :with-doc)
  (core
   (>= "v0.16.0"))
  (core_unix
   (>= "v0.16.0"))
  dune
  cmdliner
  bos
  batteries
  ppx_import
  ppx_deriving
  (ppx_inline_test :with-test)
  extlib
  menhir
  menhirLib
  (encoding
   (>= "0.0.1"))
  yojson)
 (tags
  (ECMAScript "Specification Language" "Reference Interpreters" "OCaml")))

(package
 (name ecma-sl-share)
 (synopsis "Ecma-sl-share")
 (depends dune-site)
 (sites
  (share interpreters)
  (share nodejs)))
