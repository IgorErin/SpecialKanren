(lang dune 3.10)

(name SpecialKanren)

(generate_opam_files true)

(source
 (github IgorErin/SpecialKanren))

(authors "Igor Erin")

(maintainers "Igor Erin")

(license MIT)

(package
 (name SpecialKanren)
 (synopsis "Relational program specializer")
 (depends 
   dune
   benchmark
  core
  (ocaml (>= "4.14"))
  ocamlfind
  GT
  (OCanren (>= "0.3.0~"))
  (ocamlformat
   (and
    (>= "0.26")
    :with-test))
  ppx_optcomp
  ppxlib))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
