(lang dune 2.6)
(name opam-tools)

(generate_opam_files true)

(source (github ocaml-opam/opam-tools))
(license ISC)
(authors "Anil Madhavapeddy")
(maintainers "anil@recoil.org")

(package
 (name opam-tools)
 (synopsis "Initialise a local OCaml development environment")
 (description "opam-tools sets up a local project to have a working set of opam libraries and development tools such as dune, odoc, ocamlformat and IDE helpers like merlin and ocaml-lsp.  This is all made available by simply typing in `opam tools` in your project directory.")
 (depends
  (ocaml (>= 4.08.0))
  dune-build-info
  rresult
  ocaml-version
  fmt
  cmdliner
  fpath
  ezjsonm
  bos))
