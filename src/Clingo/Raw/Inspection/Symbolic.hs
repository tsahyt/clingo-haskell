module Clingo.Raw.Inspection.Symbolic
(
)
where

foreign import ccall "clingo.h clingo_symbolic_atoms_size" symbolicAtomsSizeFFI ::
foreign import ccall "clingo.h clingo_symbolic_atoms_begin" symbolicAtomsBeginFFI ::
foreign import ccall "clingo.h clingo_symbolic_atoms_end" symbolicAtomsEndFFI ::
foreign import ccall "clingo.h clingo_symbolic_atoms_fid" symbolicAtomsFidFFI ::
foreign import ccall "clingo.h clingo_symbolic_atoms_iterator_is_equal_to" symbolicAtomsIteratorIsEqualToFFI ::
foreign import ccall "clingo.h clingo_symbolic_atoms_symbol" symbolicAtomsSymbolFFI ::
foreign import ccall "clingo.h clingo_symbolic_atoms_is_fact" symbolicAtomsIsFactFFI ::
foreign import ccall "clingo.h clingo_symbolic_atoms_is_external" symbolicAtomsIsExternalFFI ::
foreign import ccall "clingo.h clingo_symbolic_atoms_literal" symbolicAtomsLiteralFFI ::
foreign import ccall "clingo.h clingo_symbolic_atoms_signatures_size" symbolicAtomsSignaturesSizeFFI ::
foreign import ccall "clingo.h clingo_symbolic_atoms_signatures" symbolicAtomsSignaturesFFI ::
foreign import ccall "clingo.h clingo_symbolic_atoms_atoms_next" symbolicAtomsAtomsNextFFI ::
foreign import ccall "clingo.h clingo_symbolic_atoms_atoms_is_valid" symbolicAtomsAtomsIsValidFFI ::
