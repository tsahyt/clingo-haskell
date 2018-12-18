{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Internal.Inspection.Theory
(
    TheoryAtoms,
    TermId,
    ElementId,
    AtomId,
    TheoryTermType,

    pattern TheoryTuple,
    pattern TheoryList,
    pattern TheorySet,
    pattern TheoryFunction,
    pattern TheoryNumber,
    pattern TheorySymbol,

    theoryAtomsSize,
    theoryAtomsId,
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

pattern TheoryTuple = TheoryTermType Raw.TheoryTuple
pattern TheoryList = TheoryTermType Raw.TheoryList
pattern TheorySet = TheoryTermType Raw.TheorySet
pattern TheoryFunction = TheoryTermType Raw.TheoryFunction
pattern TheoryNumber = TheoryTermType Raw.TheoryNumber
pattern TheorySymbol = TheoryTermType Raw.TheorySymbol

theoryAtomsTermType :: (MonadIO m, MonadThrow m) 
                    => TheoryAtoms s -> TermId -> m TheoryTermType
theoryAtomsTermType (TheoryAtoms h) (TermId k) = TheoryTermType <$>
    marshal1 (Raw.theoryAtomsTermType h k)

theoryAtomsTermNumber :: (MonadIO m, MonadThrow m) 
                      => TheoryAtoms s -> TermId -> m Integer
theoryAtomsTermNumber (TheoryAtoms h) (TermId k) = fromIntegral <$>
    marshal1 (Raw.theoryAtomsTermNumber h k)

theoryAtomsTermName :: (MonadIO m, MonadThrow m) 
                    => TheoryAtoms s -> TermId -> m Text
theoryAtomsTermName (TheoryAtoms h) (TermId k) = pack <$> 
    (liftIO . peekCString =<< marshal1 (Raw.theoryAtomsTermName h k))

theoryAtomsTermArguments :: (MonadIO m, MonadThrow m) 
                         => TheoryAtoms s -> TermId -> m [TermId]
theoryAtomsTermArguments (TheoryAtoms h) (TermId k) = map TermId <$> marshal1A
    (Raw.theoryAtomsTermArguments h k)

theoryAtomsTermToString :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> TermId -> m Text
theoryAtomsTermToString (TheoryAtoms h) (TermId k) = do
    len <- marshal1 (Raw.theoryAtomsTermToStringSize h k)
    liftIO $ allocaArray (fromIntegral len) $ \arr -> do
        marshal0 (Raw.theoryAtomsTermToString h k arr len)
        pack <$> peekCStringLen (arr, fromIntegral len)

theoryAtomsElementTuple :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> ElementId -> m [TermId]
theoryAtomsElementTuple (TheoryAtoms h) (ElementId k) = 
    map TermId <$> marshal1A (Raw.theoryAtomsElementTuple h k)

theoryAtomsElementCondition :: (MonadIO m, MonadThrow m) 
                            => TheoryAtoms s -> ElementId -> m [AspifLiteral s]
theoryAtomsElementCondition (TheoryAtoms h) (ElementId k) = 
    map AspifLiteral <$> marshal1A (Raw.theoryAtomsElementCondition h k)

theoryAtomsElementConditionId :: (MonadIO m, MonadThrow m) 
                              => TheoryAtoms s -> ElementId 
                              -> m (AspifLiteral s)
theoryAtomsElementConditionId (TheoryAtoms h) (ElementId k) = AspifLiteral <$>
    marshal1 (Raw.theoryAtomsElementConditionId h k)

theoryAtomsElementToString :: (MonadIO m, MonadThrow m) 
                           => TheoryAtoms s -> ElementId -> m Text
theoryAtomsElementToString (TheoryAtoms h) (ElementId k) = do
    len <- marshal1 (Raw.theoryAtomsElementToStringSize h k)
    liftIO $ allocaArray (fromIntegral len) $ \arr -> do
        marshal0 (Raw.theoryAtomsElementToString h k arr len)
        pack <$> peekCStringLen (arr, fromIntegral len)

theoryAtomsSize :: (MonadIO m, MonadThrow m) => TheoryAtoms s -> m Natural
theoryAtomsSize (TheoryAtoms h) = 
    fromIntegral <$> marshal1 (Raw.theoryAtomsSize h)

theoryAtomsId :: (MonadIO m, MonadThrow m) 
              => TheoryAtoms s -> Natural -> m (Maybe AtomId)
theoryAtomsId h x = do
    lim <- theoryAtomsSize h
    return $ if x < lim then Just (AtomId (fromIntegral x)) else Nothing

theoryAtomsAtomTerm :: (MonadIO m, MonadThrow m) 
                    => TheoryAtoms s -> AtomId -> m TermId
theoryAtomsAtomTerm (TheoryAtoms h) (AtomId k) = TermId <$> marshal1
    (Raw.theoryAtomsAtomTerm h k)

theoryAtomsAtomElements :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> AtomId -> m [ElementId]
theoryAtomsAtomElements (TheoryAtoms h) (AtomId k) = 
    map ElementId <$> marshal1A (Raw.theoryAtomsAtomElements h k)

theoryAtomsAtomHasGuard :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> AtomId -> m Bool
theoryAtomsAtomHasGuard (TheoryAtoms h) (AtomId k) = toBool <$> marshal1
    (Raw.theoryAtomsAtomHasGuard h k)

theoryAtomsAtomGuard :: (MonadIO m, MonadThrow m) 
                     => TheoryAtoms s -> AtomId -> m (Text, TermId)
theoryAtomsAtomGuard (TheoryAtoms h) (AtomId k) = bimap pack TermId <$> go
    where go = do
              (x,y) <- marshal2 $ Raw.theoryAtomsAtomGuard h k
              x' <- liftIO $ peekCString x
              return (x',y)

theoryAtomsAtomLiteral :: (MonadIO m, MonadThrow m) 
                       => TheoryAtoms s -> AtomId -> m (AspifLiteral s)
theoryAtomsAtomLiteral (TheoryAtoms h) (AtomId k) = AspifLiteral <$> marshal1
    (Raw.theoryAtomsAtomLiteral h k)

theoryAtomsAtomToString :: (MonadIO m, MonadThrow m) 
                        => TheoryAtoms s -> AtomId -> m Text
theoryAtomsAtomToString (TheoryAtoms h) (AtomId k) = do
    len <- marshal1 (Raw.theoryAtomsAtomToStringSize h k)
    liftIO $ allocaArray (fromIntegral len) $ \arr -> do
        marshal0 (Raw.theoryAtomsAtomToString h k arr len)
        pack <$> peekCStringLen (arr, fromIntegral len)
