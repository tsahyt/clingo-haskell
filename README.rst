clingo Haskell bindings
=======================

This library provides bindings to the clingo_ C API in Haskell. The aim is to provide both a high-level idiomatic Haskell library as well as low-level bindings. A low level (raw) binding is provided through the ``Clingo.Raw`` modules. This presents a 1:1 binding to the C version, and as such uses the same conventions, e.g. return values indicating success or failure and "logical" return values being given through pointer arguments.

.. _clingo: https://github.com/potassco/clingo
