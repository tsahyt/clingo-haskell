-- | Module providing direct access to the C style iterator interface for
-- symbolic atom inspection. A version abstracting this into lists is provided
-- in 'Clingo.Inspection.Symbolic'. This module is exported for users who want
-- to build their own iterator abstraction.
module Clingo.Internal.Inspection.Symbolic
(
    SymbolicAtoms,
    SIterator,
    symbolicAtomsSize,
    symbolicAtomsBegin,
    symbolicAtomsEnd,
    symbolicAtomsNext,
    symbolicAtomsIsValid,
    symbolicAtomsFind,
    symbolicAtomsIteratorEq,
    symbolicAtomsSymbol,
    symbolicAtomsIsFact,
    symbolicAtomsIsExternal,
    symbolicAtomsLiteral,
    symbolicAtomsSignatures,
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch

import Numeric.Natural
import Foreign
import Foreign.C

import qualified Clingo.Raw as Raw
import Clingo.Internal.Types
import Clingo.Internal.Utils

newtype SIterator s = SIterator Raw.SymbolicAtomIterator

symbolicAtomsSize :: (MonadIO m, MonadThrow m)
                  => SymbolicAtoms s -> m Natural
symbolicAtomsSize (SymbolicAtoms s) = 
    fromIntegral <$> marshall1 (Raw.symbolicAtomsSize s)

symbolicAtomsBegin :: (MonadIO m, MonadThrow m)
                   => SymbolicAtoms s -> Maybe (Signature s) -> m (SIterator s)
symbolicAtomsBegin (SymbolicAtoms s) sig = SIterator <$> marshall1 go
    where go x = case sig of
                     Nothing -> Raw.symbolicAtomsBegin s nullPtr x
                     Just y  -> with (rawSignature y) $ \ptr ->
                                    Raw.symbolicAtomsBegin s ptr x

symbolicAtomsEnd :: (MonadIO m, MonadThrow m)
                   => SymbolicAtoms s -> m (SIterator s)
symbolicAtomsEnd (SymbolicAtoms s) = SIterator <$> marshall1 go
    where go = Raw.symbolicAtomsEnd s

symbolicAtomsFind :: (MonadIO m, MonadThrow m)
                  => SymbolicAtoms s -> Symbol s -> m (SIterator s)
symbolicAtomsFind (SymbolicAtoms s) sym = SIterator <$> marshall1
    (Raw.symbolicAtomsFind s (rawSymbol sym))

symbolicAtomsIteratorEq :: (MonadIO m, MonadThrow m)
                        => SymbolicAtoms s -> SIterator s -> SIterator s
                        -> m Bool
symbolicAtomsIteratorEq (SymbolicAtoms s) (SIterator a) (SIterator b) =
    toBool <$> marshall1 (Raw.symbolicAtomsIteratorIsEqualTo s a b)

symbolicAtomsSymbol :: (MonadIO m, MonadThrow m)
                    => SymbolicAtoms s -> SIterator s -> m (Symbol s)
symbolicAtomsSymbol (SymbolicAtoms s) (SIterator i) =
    Symbol <$> marshall1 (Raw.symbolicAtomsSymbol s i)

symbolicAtomsIsFact :: (MonadIO m, MonadThrow m)
                    => SymbolicAtoms s -> SIterator s -> m Bool
symbolicAtomsIsFact (SymbolicAtoms s) (SIterator i) = 
    toBool <$> marshall1 (Raw.symbolicAtomsIsFact s i)

symbolicAtomsIsExternal :: (MonadIO m, MonadThrow m)
                        => SymbolicAtoms s -> SIterator s -> m Bool
symbolicAtomsIsExternal (SymbolicAtoms s) (SIterator i) = 
    toBool <$> marshall1 (Raw.symbolicAtomsIsExternal s i)

symbolicAtomsLiteral :: (MonadIO m, MonadThrow m)
                     => SymbolicAtoms s -> SIterator s -> m (Literal s)
symbolicAtomsLiteral (SymbolicAtoms s) (SIterator i) =
    Literal <$> marshall1 (Raw.symbolicAtomsLiteral s i)

symbolicAtomsSignatures :: (MonadIO m, MonadThrow m)
                        => SymbolicAtoms s -> m [Signature s]
symbolicAtomsSignatures (SymbolicAtoms s) = do
    len <- marshall1 (Raw.symbolicAtomsSignaturesSize s)
    liftIO $ allocaArray (fromIntegral len) $ \arr -> do
        marshall0 (Raw.symbolicAtomsSignatures s arr len)
        map Signature <$> peekArray (fromIntegral len) arr

symbolicAtomsNext :: (MonadIO m, MonadThrow m)
                  => SymbolicAtoms s -> SIterator s -> m (SIterator s)
symbolicAtomsNext (SymbolicAtoms s) (SIterator i) = SIterator <$>
    marshall1 (Raw.symbolicAtomsNext s i)

symbolicAtomsIsValid :: (MonadIO m, MonadThrow m)
                     => SymbolicAtoms s -> SIterator s -> m Bool
symbolicAtomsIsValid (SymbolicAtoms s) (SIterator i) = toBool <$>
    marshall1 (Raw.symbolicAtomsIsValid s i)
