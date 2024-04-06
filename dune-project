(lang dune 3.14)

(name multy_poly_divy)

(generate_opam_files true)

(source
 (github benbellick/multy-poly-divy))

(authors "Ben Bellick")

(package
 (name multy_poly_divy)
 (synopsis "Multivariate polynomial division")
 (description "Still multivariate polynomial division")
 (depends ocaml dune containers zarith ppx_inline_test))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
