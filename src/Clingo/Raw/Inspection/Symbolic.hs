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
    Ptr SymbolicAtoms -> Ptr CSize -> IO CBool 
foreign import ccall "clingo.h clingo_symbolic_atoms_begin" symbolicAtomsBeginFFI ::
    Ptr SymbolicAtoms -> Ptr Signature -> Ptr SymbolicAtomIterator -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_end" symbolicAtomsEndFFI ::
    Ptr SymbolicAtoms -> Ptr SymbolicAtomIterator -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_find" symbolicAtomsFindFFI ::
    Ptr SymbolicAtoms -> Symbol -> Ptr SymbolicAtomIterator -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_iterator_is_equal_to" symbolicAtomsIteratorIsEqualToFFI ::
    Ptr SymbolicAtoms -> SymbolicAtomIterator -> SymbolicAtomIterator -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_symbol" symbolicAtomsSymbolFFI ::
    Ptr SymbolicAtoms -> SymbolicAtomIterator -> Ptr Symbol -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_is_fact" symbolicAtomsIsFactFFI ::
    Ptr SymbolicAtoms -> SymbolicAtomIterator -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_is_external" symbolicAtomsIsExternalFFI ::
    Ptr SymbolicAtoms -> SymbolicAtomIterator -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_literal" symbolicAtomsLiteralFFI ::
    Ptr SymbolicAtoms -> SymbolicAtomIterator -> Ptr Literal -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_signatures_size" symbolicAtomsSignaturesSizeFFI ::
    Ptr SymbolicAtoms -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_signatures" symbolicAtomsSignaturesFFI ::
    Ptr SymbolicAtoms -> Ptr Signature -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_atoms_next" symbolicAtomsAtomsNextFFI ::
    Ptr SymbolicAtoms -> SymbolicAtomIterator -> Ptr SymbolicAtomIterator -> IO CBool
foreign import ccall "clingo.h clingo_symbolic_atoms_atoms_is_valid" symbolicAtomsAtomsIsValidFFI ::
    Ptr SymbolicAtoms -> SymbolicAtomIterator -> Ptr CBool -> IO CBool

symbolicAtomsSize :: MonadIO m => Ptr SymbolicAtoms -> Ptr CSize -> m CBool 
symbolicAtomsSize a b = liftIO $ symbolicAtomsSizeFFI a b

symbolicAtomsBegin :: MonadIO m => Ptr SymbolicAtoms -> Ptr Signature -> Ptr SymbolicAtomIterator -> m CBool
symbolicAtomsBegin a b c = liftIO $ symbolicAtomsBeginFFI a b c

symbolicAtomsEnd :: MonadIO m => Ptr SymbolicAtoms -> Ptr SymbolicAtomIterator -> m CBool
symbolicAtomsEnd a b = liftIO $ symbolicAtomsEndFFI a b

symbolicAtomsFind :: MonadIO m => Ptr SymbolicAtoms -> Symbol -> Ptr SymbolicAtomIterator -> m CBool
symbolicAtomsFind a b c = liftIO $ symbolicAtomsFindFFI a b c

symbolicAtomsIteratorIsEqualTo :: MonadIO m => Ptr SymbolicAtoms -> SymbolicAtomIterator -> SymbolicAtomIterator -> Ptr CBool -> m CBool
symbolicAtomsIteratorIsEqualTo a b c d = liftIO $ symbolicAtomsIteratorIsEqualToFFI a b c d

symbolicAtomsSymbol :: MonadIO m => Ptr SymbolicAtoms -> SymbolicAtomIterator -> Ptr Symbol -> m CBool
symbolicAtomsSymbol a b c = liftIO $ symbolicAtomsSymbolFFI a b c

symbolicAtomsIsFact :: MonadIO m => Ptr SymbolicAtoms -> SymbolicAtomIterator -> Ptr CBool -> m CBool
symbolicAtomsIsFact a b c = liftIO $ symbolicAtomsIsFactFFI a b c

symbolicAtomsIsExternal :: MonadIO m => Ptr SymbolicAtoms -> SymbolicAtomIterator -> Ptr CBool -> m CBool
symbolicAtomsIsExternal a b c = liftIO $ symbolicAtomsIsExternalFFI a b c

symbolicAtomsLiteral :: MonadIO m => Ptr SymbolicAtoms -> SymbolicAtomIterator -> Ptr Literal -> m CBool
symbolicAtomsLiteral a b c = liftIO $ symbolicAtomsLiteralFFI a b c

symbolicAtomsSignaturesSize :: MonadIO m => Ptr SymbolicAtoms -> Ptr CSize -> m CBool
symbolicAtomsSignaturesSize a b = liftIO $ symbolicAtomsSignaturesSizeFFI a b

symbolicAtomsSignatures :: MonadIO m => Ptr SymbolicAtoms -> Ptr Signature -> m CBool
symbolicAtomsSignatures a b = liftIO $ symbolicAtomsSignaturesFFI a b

symbolicAtomsAtomsNext :: MonadIO m => Ptr SymbolicAtoms -> SymbolicAtomIterator -> Ptr SymbolicAtomIterator -> m CBool
symbolicAtomsAtomsNext a b c = liftIO $ symbolicAtomsAtomsNextFFI a b c

symbolicAtomsAtomsIsValid :: MonadIO m => Ptr SymbolicAtoms -> SymbolicAtomIterator -> Ptr CBool -> m CBool
symbolicAtomsAtomsIsValid a b c = liftIO $ symbolicAtomsAtomsIsValidFFI a b c
