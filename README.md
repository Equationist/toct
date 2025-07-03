This is intended to create a compiler toolkit in OCaml which consists of an intermediate representation, an optimized backend with ports for common instruction set architectures, and a front-end toolkit with helpers for doing parsing (either recursive descent or using menhir generated parser) to create an AST and symbol table(s) and generate IR code from it.

To begin with, we'll have an example front end that preprocesses and then parses ANSI standard C89/90 and has a backend for Arm64 (on Mac). We'll add x86-64 support shortly after.

For testing various backends we might add start using a virtual machine or emulator.

The goal is for developers to be able to write compilers for existing or new programming languages and generate high performance (>90% of gcc -o2 performance) without needing to deal with the complexities of LLVM or the likes.

If designed well, this would be a very useful tool for developers of new languages, who right now have to stick to transpiling or interpreting their language, or go through a lot of trouble to implement a language backend. It is implemented in OCaml because that's the compiler / parser language of choice for implementing new languages.