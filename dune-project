(lang dune 3.17)

(using menhir 3.0)

(name basic_calc)

(package
 (name basic_calc)
 (synopsis "bc")
 (description "basic calculator")
 (depends ocaml sedlex menhirLib)
 )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
