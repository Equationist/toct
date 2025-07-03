(lang dune 3.0)

(name compilerkit)

(package
 (name compilerkit)
 (depends ocaml dune)
 (synopsis "Tiny OCaml Compiler Toolkit")
 (description "A modular compiler infrastructure with portable IR, frontend helpers, and optimizing backend"))

(package
 (name compilerkit-test)
 (depends compilerkit)
 (synopsis "Test suite for compilerkit")
 (allow_empty))