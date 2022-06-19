(lang dune 3.2)
(name alan-lambda-calculus-interpreter-calculus)
(name alan-parser-in-ocaml-with-menhir)
(generate_opam_files true)

(package
 (name alan-lambda-calculus-interpreter-calculus)
 (name alan-parser-in-ocaml-with-menhir)
 (depends
  (ocaml
   (>= "4.08.0"))
    ppx_deriving
    menhir
    sedlex))
