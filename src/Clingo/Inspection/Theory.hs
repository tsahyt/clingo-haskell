module Clingo.Inspection.Theory
(
    -- * Direct access
    TKey,
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

newtype TKey = TKey Raw.Identifier

newtype TheoryTermType = TheoryTermType Raw.TheoryTermType

theoryAtomsTermType :: (MonadIO m, MonadThrow m) 
                    => TheoryAtoms s -> TKey -> m TheoryTermType
theoryAtomsTermType (TheoryAtoms h) (TKey k) = TheoryTermType <$>
    marshall1 (Raw.theoryAtomsTermType h k)

theoryAtomsTermNumber :: (MonadIO m, MonadThrow m) 
                      => TheoryAtoms s -> TKey -> m Integer
theoryAtomsTermNumber (TheoryAtoms h) (TKey k) = fromIntegral <$>
    marshall1 (Raw.theoryAtomsTermNumber h k)

theoryAtomsTermName :: (MonadIO m, MonadThrow m) 
                    => TheoryAtoms s -> TKey -> m Text
theoryAtomsTermName (TheoryAtoms h) (TKey k) = pack <$> 
    (liftIO . peekCString =<< marshall1 (Raw.theoryAtomsTermName h k))

theoryAtomsTermArguments :: (MonadIO m, MonadThrow m) 
                         => TheoryAtoms s -> TKey -> m [TKey]
theoryAtomsTermArguments (TheoryAtoms h) (TKey k) = map TKey <$> marshall1A
    (Raw.theoryAtomsTermArguments h k)

theoryAtomsTermToString :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> TKey -> m Text
theoryAtomsTermToString (TheoryAtoms h) (TKey k) = do
    len <- marshall1 (Raw.theoryAtomsTermToStringSize h k)
    liftIO $ allocaArray (fromIntegral len) $ \arr -> do
        marshall0 (Raw.theoryAtomsTermToString h k arr len)
        pack <$> peekCStringLen (arr, fromIntegral len)

theoryAtomsElementTuple :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> TKey -> m [TKey]
theoryAtomsElementTuple (TheoryAtoms h) (TKey k) = map TKey <$> marshall1A
    (Raw.theoryAtomsElementTuple h k)

theoryAtomsElementCondition :: (MonadIO m, MonadThrow m) 
                            => TheoryAtoms s -> TKey -> m [Literal s]
theoryAtomsElementCondition (TheoryAtoms h) (TKey k) = 
    map Literal <$> marshall1A (Raw.theoryAtomsElementCondition h k)

theoryAtomsElementConditionId :: (MonadIO m, MonadThrow m) 
                              => TheoryAtoms s -> TKey -> m (Literal s)
theoryAtomsElementConditionId (TheoryAtoms h) (TKey k) = Literal <$>
    marshall1 (Raw.theoryAtomsElementConditionId h k)

theoryAtomsElementToString :: (MonadIO m, MonadThrow m) 
                           => TheoryAtoms s -> TKey -> m Text
theoryAtomsElementToString (TheoryAtoms h) (TKey k) = do
    len <- marshall1 (Raw.theoryAtomsElementToStringSize h k)
    liftIO $ allocaArray (fromIntegral len) $ \arr -> do
        marshall0 (Raw.theoryAtomsElementToString h k arr len)
        pack <$> peekCStringLen (arr, fromIntegral len)

theoryAtomsSize :: (MonadIO m, MonadThrow m) => TheoryAtoms s -> m Natural
theoryAtomsSize (TheoryAtoms h) = 
    fromIntegral <$> marshall1 (Raw.theoryAtomsSize h)

theoryAtomsAtomTerm :: (MonadIO m, MonadThrow m) 
                    => TheoryAtoms s -> TKey -> m TKey
theoryAtomsAtomTerm (TheoryAtoms h) (TKey k) = TKey <$> marshall1
    (Raw.theoryAtomsAtomTerm h k)

theoryAtomsAtomElements :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> TKey -> m [TKey]
theoryAtomsAtomElements (TheoryAtoms h) (TKey k) = map TKey <$> marshall1A
    (Raw.theoryAtomsAtomElements h k)

theoryAtomsAtomHasGuard :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> TKey -> m Bool
theoryAtomsAtomHasGuard (TheoryAtoms h) (TKey k) = toBool <$> marshall1
    (Raw.theoryAtomsAtomHasGuard h k)

theoryAtomsAtomGuard :: (MonadIO m, MonadThrow m) 
                     => TheoryAtoms s -> TKey -> m (Text, TKey)
theoryAtomsAtomGuard (TheoryAtoms h) (TKey k) = bimap pack TKey <$> go
    where go = do
              (x,y) <- marshall2 $ Raw.theoryAtomsAtomGuard h k
              x' <- liftIO $ peekCString x
              return (x',y)

theoryAtomsAtomLiteral :: (MonadIO m, MonadThrow m) 
                       => TheoryAtoms s -> TKey -> m (Literal s)
theoryAtomsAtomLiteral (TheoryAtoms h) (TKey k) = Literal <$> marshall1
    (Raw.theoryAtomsAtomLiteral h k)

theoryAtomsAtomToString :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> TKey -> m Text
theoryAtomsAtomToString (TheoryAtoms h) (TKey k) = do
    len <- marshall1 (Raw.theoryAtomsAtomToStringSize h k)
    liftIO $ allocaArray (fromIntegral len) $ \arr -> do
        marshall0 (Raw.theoryAtomsAtomToString h k arr len)
        pack <$> peekCStringLen (arr, fromIntegral len)
