(lang dune 3.2)

(using menhir 2.1)

(using mdx 0.2)

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
   (>= "4.14.0"))
  (odoc :with-doc)
  dune
  cmdliner
  z3
  bos
  ppx_import
  ppx_deriving
  (ppx_inline_test :with-test)
  (mdx :with-test)
  menhir
  menhirLib
  (smtml
   (>= "0.2.0"))
  yojson
  dune-site
  (bisect_ppx
   (and
    :with-test
    (>= "2.5.0")
    :dev))
  conf-npm)
 (tags
  (ECMAScript "Specification Language" "Reference Interpreters" "OCaml"))
(sites
  (share interpreters)
  (share nodejs)))
