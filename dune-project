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
   (>= "4.14.0"))
  (odoc :with-doc)
  dune
  dune-site
  bos
  cmdliner
  curses
  cohttp
  cohttp-lwt-unix
  lwt
  menhir
  menhirLib
  tls-lwt
  yojson
  conf-npm
  (smtml (>= "0.2.5"))
  ppx_import
  ppx_deriving
  (ppx_inline_test :with-test)
  (bisect_ppx (and :with-test (>= "2.5.0") :dev)))
 (tags
  (ECMAScript "Specification Language" "Reference Interpreters" "OCaml"))
 (sites
  (share interpreters)
  (share stdlib)))
