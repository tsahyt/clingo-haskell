module Clingo.Inspection.Theory
(
    -- * Direct access
    TermId,
    ElementId,
    AtomId,
    TheoryTermType,

    theoryAtomsSize,
    theoryAtomsTermType,
    theoryAtomsTermNumber,
    theoryAtomsTermName,
    theoryAtomsTermArguments,
    theoryAtomsTermToString,
    theoryAtomsElementTuple,
    theoryAtomsElementCondition,
    theoryAtomsElementConditionId,
    theoryAtomsElementToString,
    theoryAtomsAtomTerm,
    theoryAtomsAtomElements,
    theoryAtomsAtomHasGuard,
    theoryAtomsAtomGuard,
    theoryAtomsAtomLiteral,
    theoryAtomsAtomToString
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Text (Text, pack)
import Data.Bifunctor

import Numeric.Natural
import Foreign
import Foreign.C

import qualified Clingo.Raw as Raw
import Clingo.Internal.Types
import Clingo.Internal.Utils

newtype TermId = TermId Raw.Identifier
newtype ElementId = ElementId Raw.Identifier
newtype AtomId = AtomId Raw.Identifier

newtype TheoryTermType = TheoryTermType Raw.TheoryTermType

theoryAtomsTermType :: (MonadIO m, MonadThrow m) 
                    => TheoryAtoms s -> TermId -> m TheoryTermType
theoryAtomsTermType (TheoryAtoms h) (TermId k) = TheoryTermType <$>
    marshall1 (Raw.theoryAtomsTermType h k)

theoryAtomsTermNumber :: (MonadIO m, MonadThrow m) 
                      => TheoryAtoms s -> TermId -> m Integer
theoryAtomsTermNumber (TheoryAtoms h) (TermId k) = fromIntegral <$>
    marshall1 (Raw.theoryAtomsTermNumber h k)

theoryAtomsTermName :: (MonadIO m, MonadThrow m) 
                    => TheoryAtoms s -> TermId -> m Text
theoryAtomsTermName (TheoryAtoms h) (TermId k) = pack <$> 
    (liftIO . peekCString =<< marshall1 (Raw.theoryAtomsTermName h k))

theoryAtomsTermArguments :: (MonadIO m, MonadThrow m) 
                         => TheoryAtoms s -> TermId -> m [TermId]
theoryAtomsTermArguments (TheoryAtoms h) (TermId k) = map TermId <$> marshall1A
    (Raw.theoryAtomsTermArguments h k)

theoryAtomsTermToString :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> TermId -> m Text
theoryAtomsTermToString (TheoryAtoms h) (TermId k) = do
    len <- marshall1 (Raw.theoryAtomsTermToStringSize h k)
    liftIO $ allocaArray (fromIntegral len) $ \arr -> do
        marshall0 (Raw.theoryAtomsTermToString h k arr len)
        pack <$> peekCStringLen (arr, fromIntegral len)

theoryAtomsElementTuple :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> ElementId -> m [TermId]
theoryAtomsElementTuple (TheoryAtoms h) (ElementId k) = 
    map TermId <$> marshall1A (Raw.theoryAtomsElementTuple h k)

theoryAtomsElementCondition :: (MonadIO m, MonadThrow m) 
                            => TheoryAtoms s -> ElementId -> m [Literal s]
theoryAtomsElementCondition (TheoryAtoms h) (ElementId k) = 
    map Literal <$> marshall1A (Raw.theoryAtomsElementCondition h k)

theoryAtomsElementConditionId :: (MonadIO m, MonadThrow m) 
                              => TheoryAtoms s -> ElementId -> m (Literal s)
theoryAtomsElementConditionId (TheoryAtoms h) (ElementId k) = Literal <$>
    marshall1 (Raw.theoryAtomsElementConditionId h k)

theoryAtomsElementToString :: (MonadIO m, MonadThrow m) 
                           => TheoryAtoms s -> ElementId -> m Text
theoryAtomsElementToString (TheoryAtoms h) (ElementId k) = do
    len <- marshall1 (Raw.theoryAtomsElementToStringSize h k)
    liftIO $ allocaArray (fromIntegral len) $ \arr -> do
        marshall0 (Raw.theoryAtomsElementToString h k arr len)
        pack <$> peekCStringLen (arr, fromIntegral len)

theoryAtomsSize :: (MonadIO m, MonadThrow m) => TheoryAtoms s -> m Natural
theoryAtomsSize (TheoryAtoms h) = 
    fromIntegral <$> marshall1 (Raw.theoryAtomsSize h)

theoryAtomsAtomTerm :: (MonadIO m, MonadThrow m) 
                    => TheoryAtoms s -> AtomId -> m TermId
theoryAtomsAtomTerm (TheoryAtoms h) (AtomId k) = TermId <$> marshall1
    (Raw.theoryAtomsAtomTerm h k)

theoryAtomsAtomElements :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> AtomId -> m [ElementId]
theoryAtomsAtomElements (TheoryAtoms h) (AtomId k) = 
    map ElementId <$> marshall1A (Raw.theoryAtomsAtomElements h k)

theoryAtomsAtomHasGuard :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> AtomId -> m Bool
theoryAtomsAtomHasGuard (TheoryAtoms h) (AtomId k) = toBool <$> marshall1
    (Raw.theoryAtomsAtomHasGuard h k)

theoryAtomsAtomGuard :: (MonadIO m, MonadThrow m) 
                     => TheoryAtoms s -> AtomId -> m (Text, TermId)
theoryAtomsAtomGuard (TheoryAtoms h) (AtomId k) = bimap pack TermId <$> go
    where go = do
              (x,y) <- marshall2 $ Raw.theoryAtomsAtomGuard h k
              x' <- liftIO $ peekCString x
              return (x',y)

theoryAtomsAtomLiteral :: (MonadIO m, MonadThrow m) 
                       => TheoryAtoms s -> AtomId -> m (Literal s)
theoryAtomsAtomLiteral (TheoryAtoms h) (AtomId k) = Literal <$> marshall1
    (Raw.theoryAtomsAtomLiteral h k)

theoryAtomsAtomToString :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> AtomId -> m Text
theoryAtomsAtomToString (TheoryAtoms h) (AtomId k) = do
    len <- marshall1 (Raw.theoryAtomsAtomToStringSize h k)
    liftIO $ allocaArray (fromIntegral len) $ \arr -> do
        marshall0 (Raw.theoryAtomsAtomToString h k arr len)
        pack <$> peekCStringLen (arr, fromIntegral len)
