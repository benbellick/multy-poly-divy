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
 (depends ocaml dune containers zarith zarith_stubs_js ppx_inline_test ppx_deriving jsoo-react js_of_ocaml js_of_ocaml-compiler js_of_ocaml-ppx gen_js_api)
 (allow_empty)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
