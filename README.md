# clingo Haskell bindings

[![Hackage](https://img.shields.io/hackage/v/clingo.svg)](https://hackage.haskell.org/package/clingo)

This library provides bindings to the
[clingo](https://github.com/potassco/clingo) C API in Haskell. The aim is to
provide both a high-level idiomatic Haskell library as well as low-level
bindings. A low level (raw) binding is provided through the ``Clingo.Raw``
modules. This presents a 1:1 binding to the C version, and as such uses the
same conventions, e.g. return values indicating success or failure and
"logical" return values being given through pointer arguments.

## Caveat

Here be dragons, some features are untested and may blow up your program due to
some faulty FFI usage. Feel free to report any bugs you encounter.

## Clingo Version

In its current state this library provides bindings to the Clingo API version
5.3.0. Due to breaking API changes in 5.2.0, this will *not* compile with
earlier Clingo versions!
