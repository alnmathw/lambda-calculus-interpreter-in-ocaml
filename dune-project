(lang dune 3.2)
(using menhir 2.1)
(name alan-lambda-calculus-interpreter-calculus)
(generate_opam_files true)

(package
 (name alan-lambda-calculus-interpreter-calculus)
 (depends
  (ocaml
   (>= "4.08.0"))
    ppx_deriving
    menhir
    sedlex))
