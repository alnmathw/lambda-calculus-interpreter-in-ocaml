(lang dune 2.8)

(using menhir 2.1)

(name alan-interpreter)

(generate_opam_files true)

(package
 (name alan-interpreter)
 (depends
  (ocaml
   (>= "4.08.0"))
  ppx_deriving
  menhir
  sedlex))
