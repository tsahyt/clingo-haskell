{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Inspection.Theory
(
    -- * Term Inspection
    theoryAtomsTermType,
    theoryAtomsTermNumber,
    theoryAtomsTermName,
    theoryAtomsTermArguments,
    theoryAtomsTermToStringSize,
    theoryAtomsTermToString,

    -- * Element Inspection
    theoryAtomsElementTuple,
    theoryAtomsElementCondition,
    theoryAtomsElementConditionId,
    theoryAtomsElementToStringSize,
    theoryAtomsElementToString,

    -- * Atom Inspection
    theoryAtomsSize,
    theoryAtomsAtomTerm,
    theoryAtomsAtomElements,
    theoryAtomsAtomHasGuard,
    theoryAtomsAtomGuard,
    theoryAtomsAtomLiteral,
    theoryAtomsAtomToStringSize,
    theoryAtomsAtomToString
)
where

import Control.Monad.IO.Class
import Foreign
import Foreign.C
import Clingo.Raw.Types
import Clingo.Raw.Enums

foreign import ccall "clingo.h clingo_theory_atoms_term_type" theoryAtomsTermTypeFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr TheoryTermType -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_term_number" theoryAtomsTermNumberFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr CInt -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_term_name" theoryAtomsTermNameFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr CString -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_term_arguments" theoryAtomsTermArgumentsFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr (Ptr Identifier) -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_term_to_string_size" theoryAtomsTermToStringSizeFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_term_to_string" theoryAtomsTermToStringFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr CChar -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_element_tuple" theoryAtomsElementTupleFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr (Ptr Identifier) -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_element_condition" theoryAtomsElementConditionFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr (Ptr Literal) -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_element_condition_id" theoryAtomsElementConditionIdFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr Literal -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_element_to_string_size" theoryAtomsElementToStringSizeFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_element_to_string" theoryAtomsElementToStringFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr CChar -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_size" theoryAtomsSizeFFI ::
    Ptr TheoryAtoms -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_atom_term" theoryAtomsAtomTermFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr Identifier -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_atom_elements" theoryAtomsAtomElementsFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr (Ptr Identifier) -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_atom_has_guard" theoryAtomsAtomHasGuardFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_atom_guard" theoryAtomsAtomGuardFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr CString -> Ptr Identifier -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_atom_literal" theoryAtomsAtomLiteralFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr Literal -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_atom_to_string_size" theoryAtomsAtomToStringSizeFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_theory_atoms_atom_to_string" theoryAtomsAtomToStringFFI ::
    Ptr TheoryAtoms -> Identifier -> Ptr CChar -> CSize -> IO CBool

theoryAtomsTermType :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr TheoryTermType -> m CBool
theoryAtomsTermType a b c = liftIO $ theoryAtomsTermTypeFFI a b c

theoryAtomsTermNumber :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr CInt -> m CBool
theoryAtomsTermNumber a b c = liftIO $ theoryAtomsTermNumberFFI a b c

theoryAtomsTermName :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr CString -> m CBool
theoryAtomsTermName a b c = liftIO $ theoryAtomsTermNameFFI a b c

theoryAtomsTermArguments :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr (Ptr Identifier) -> Ptr CSize -> m CBool
theoryAtomsTermArguments a b c d = liftIO $ theoryAtomsTermArgumentsFFI a b c d

theoryAtomsTermToStringSize :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr CSize -> m CBool
theoryAtomsTermToStringSize a b c = liftIO $ theoryAtomsTermToStringSizeFFI a b c

theoryAtomsTermToString :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr CChar -> CSize -> m CBool
theoryAtomsTermToString a b c d = liftIO $ theoryAtomsTermToStringFFI a b c d

theoryAtomsElementTuple :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr (Ptr Identifier) -> Ptr CSize -> m CBool
theoryAtomsElementTuple a b c d = liftIO $ theoryAtomsElementTupleFFI a b c d

theoryAtomsElementCondition :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr (Ptr Literal) -> Ptr CSize -> m CBool
theoryAtomsElementCondition a b c d = liftIO $ theoryAtomsElementConditionFFI a b c d

theoryAtomsElementConditionId :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr Literal -> m CBool
theoryAtomsElementConditionId a b c = liftIO $ theoryAtomsElementConditionIdFFI a b c

theoryAtomsElementToStringSize :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr CSize -> m CBool
theoryAtomsElementToStringSize a b c = liftIO $ theoryAtomsElementToStringSizeFFI a b c

theoryAtomsElementToString :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr CChar -> CSize -> m CBool
theoryAtomsElementToString a b c d = liftIO $ theoryAtomsElementToStringFFI a b c d

theoryAtomsSize :: MonadIO m => Ptr TheoryAtoms -> Ptr CSize -> m CBool
theoryAtomsSize a b = liftIO $ theoryAtomsSizeFFI a b

theoryAtomsAtomTerm :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr Identifier -> m CBool
theoryAtomsAtomTerm a b c = liftIO $ theoryAtomsAtomTermFFI a b c

theoryAtomsAtomElements :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr (Ptr Identifier) -> Ptr CSize -> m CBool
theoryAtomsAtomElements a b c d = liftIO $ theoryAtomsAtomElementsFFI a b c d

theoryAtomsAtomHasGuard :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr CBool -> m CBool
theoryAtomsAtomHasGuard a b c = liftIO $ theoryAtomsAtomHasGuardFFI a b c

theoryAtomsAtomGuard :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr CString -> Ptr Identifier -> m CBool
theoryAtomsAtomGuard a b c d = liftIO $ theoryAtomsAtomGuardFFI a b c d

theoryAtomsAtomLiteral :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr Literal -> m CBool
theoryAtomsAtomLiteral a b c = liftIO $ theoryAtomsAtomLiteralFFI a b c

theoryAtomsAtomToStringSize :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr CSize -> m CBool
theoryAtomsAtomToStringSize a b c = liftIO $ theoryAtomsAtomToStringSizeFFI a b c 

theoryAtomsAtomToString :: MonadIO m => Ptr TheoryAtoms -> Identifier -> Ptr CChar -> CSize -> m CBool
theoryAtomsAtomToString a b c d = liftIO $ theoryAtomsAtomToStringFFI a b c d
