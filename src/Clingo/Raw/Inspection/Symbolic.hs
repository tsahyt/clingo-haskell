{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Inspection.Symbolic
(
    symbolicAtomsSize,
    symbolicAtomsBegin,
    symbolicAtomsEnd,
    symbolicAtomsFind,
    symbolicAtomsIteratorIsEqualTo,
    symbolicAtomsSymbol,
    symbolicAtomsIsFact,
    symbolicAtomsIsExternal,
    symbolicAtomsLiteral,
    symbolicAtomsSignaturesSize,
    symbolicAtomsSignatures,
    symbolicAtomsAtomsNext,
    symbolicAtomsAtomsIsValid
)
where

import Control.Monad.IO.Class
import Foreign
import Foreign.C
import Clingo.Raw.Types

foreign import ccall "clingo.h clingo_symbolic_atoms_size" symbolicAtomsSizeFFI ::
    SymbolicAtoms -> Ptr CSize -> IO CBool 
foreign import ccall "clingo.h clingo_symbolic_atoms_begin" symbolicAtomsBeginFFI ::
    SymbolicAtoms -> Ptr Signature -> Ptr SymbolicAtomIterator -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_end" symbolicAtomsEndFFI ::
    SymbolicAtoms -> Ptr SymbolicAtomIterator -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_find" symbolicAtomsFindFFI ::
    SymbolicAtoms -> Symbol -> Ptr SymbolicAtomIterator -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_iterator_is_equal_to" symbolicAtomsIteratorIsEqualToFFI ::
    SymbolicAtoms -> SymbolicAtomIterator -> SymbolicAtomIterator -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_symbol" symbolicAtomsSymbolFFI ::
    SymbolicAtoms -> SymbolicAtomIterator -> Ptr Symbol -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_is_fact" symbolicAtomsIsFactFFI ::
    SymbolicAtoms -> SymbolicAtomIterator -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_is_external" symbolicAtomsIsExternalFFI ::
    SymbolicAtoms -> SymbolicAtomIterator -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_literal" symbolicAtomsLiteralFFI ::
    SymbolicAtoms -> SymbolicAtomIterator -> Ptr Literal -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_signatures_size" symbolicAtomsSignaturesSizeFFI ::
    SymbolicAtoms -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_signatures" symbolicAtomsSignaturesFFI ::
    SymbolicAtoms -> Ptr Signature -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_atoms_next" symbolicAtomsAtomsNextFFI ::
    SymbolicAtoms -> SymbolicAtomIterator -> Ptr SymbolicAtomIterator -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_atoms_is_valid" symbolicAtomsAtomsIsValidFFI ::
    SymbolicAtoms -> SymbolicAtomIterator -> Ptr CBool -> IO CBool

symbolicAtomsSize :: MonadIO m => SymbolicAtoms -> Ptr CSize -> m CBool 
symbolicAtomsSize a b = liftIO $ symbolicAtomsSizeFFI a b

symbolicAtomsBegin :: MonadIO m => SymbolicAtoms -> Ptr Signature -> Ptr SymbolicAtomIterator -> m CBool
symbolicAtomsBegin a b c = liftIO $ symbolicAtomsBeginFFI a b c

symbolicAtomsEnd :: MonadIO m => SymbolicAtoms -> Ptr SymbolicAtomIterator -> m CBool
symbolicAtomsEnd a b = liftIO $ symbolicAtomsEndFFI a b

symbolicAtomsFind :: MonadIO m => SymbolicAtoms -> Symbol -> Ptr SymbolicAtomIterator -> m CBool
symbolicAtomsFind a b c = liftIO $ symbolicAtomsFindFFI a b c

symbolicAtomsIteratorIsEqualTo :: MonadIO m => SymbolicAtoms -> SymbolicAtomIterator -> SymbolicAtomIterator -> Ptr CBool -> m CBool
symbolicAtomsIteratorIsEqualTo a b c d = liftIO $ symbolicAtomsIteratorIsEqualToFFI a b c d

symbolicAtomsSymbol :: MonadIO m => SymbolicAtoms -> SymbolicAtomIterator -> Ptr Symbol -> m CBool
symbolicAtomsSymbol a b c = liftIO $ symbolicAtomsSymbolFFI a b c

symbolicAtomsIsFact :: MonadIO m => SymbolicAtoms -> SymbolicAtomIterator -> Ptr CBool -> m CBool
symbolicAtomsIsFact a b c = liftIO $ symbolicAtomsIsFactFFI a b c

symbolicAtomsIsExternal :: MonadIO m => SymbolicAtoms -> SymbolicAtomIterator -> Ptr CBool -> m CBool
symbolicAtomsIsExternal a b c = liftIO $ symbolicAtomsIsExternalFFI a b c

symbolicAtomsLiteral :: MonadIO m => SymbolicAtoms -> SymbolicAtomIterator -> Ptr Literal -> m CBool
symbolicAtomsLiteral a b c = liftIO $ symbolicAtomsLiteralFFI a b c

symbolicAtomsSignaturesSize :: MonadIO m => SymbolicAtoms -> Ptr CSize -> m CBool
symbolicAtomsSignaturesSize a b = liftIO $ symbolicAtomsSignaturesSizeFFI a b

symbolicAtomsSignatures :: MonadIO m => SymbolicAtoms -> Ptr Signature -> m CBool
symbolicAtomsSignatures a b = liftIO $ symbolicAtomsSignaturesFFI a b

symbolicAtomsAtomsNext :: MonadIO m => SymbolicAtoms -> SymbolicAtomIterator -> Ptr SymbolicAtomIterator -> m CBool
symbolicAtomsAtomsNext a b c = liftIO $ symbolicAtomsAtomsNextFFI a b c

symbolicAtomsAtomsIsValid :: MonadIO m => SymbolicAtoms -> SymbolicAtomIterator -> Ptr CBool -> m CBool
symbolicAtomsAtomsIsValid a b c = liftIO $ symbolicAtomsAtomsIsValidFFI a b c
