(lang dune 3.14)

(using melange 0.1)
 
(name multy_poly_divy)

(generate_opam_files true)

(source
 (github benbellick/multy-poly-divy))

(authors "Ben Bellick")
(maintainers "Ben Bellick")

(package
 (allow_empty)
 (name multy_poly_divy)
 (synopsis "Multivariate polynomial division")
 (description "Still multivariate polynomial division")
 (depends ocaml reason reason-react melange dune opam-check-npm-deps))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
