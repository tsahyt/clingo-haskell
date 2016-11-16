{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Model
(
    Model,
    SolveControl,
    SymbolicLiteral,
    SymbolSelection (..),

    ModelType,
    pattern StableModel,
    pattern BraveConsequences,
    pattern CautiousConsequences,

    selectAll,
    selectNone,

    modelType,
    modelNumber,
    modelSymbols,

    contains,
    costVectors,
    optimalityProven,
    context,

    threadID,
    modelAddClause,
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Foldable
import Foreign

import Numeric.Natural

import qualified Clingo.Raw as Raw
import Clingo.Internal.Types
import Clingo.Internal.Utils

newtype SolveControl s = SolveControl Raw.SolveControl

newtype ModelType = ModelType Raw.ModelType

pattern StableModel = ModelType Raw.StableModel
pattern BraveConsequences = ModelType Raw.BraveConsequences
pattern CautiousConsequences = ModelType Raw.CautiousConsequences

data SymbolSelection = SymbolSelection 
    { selectCSP     :: Bool
    , selectShown   :: Bool
    , selectAtoms   :: Bool
    , selectTerms   :: Bool
    , selectExtra   :: Bool
    , useComplement :: Bool }
    deriving (Eq, Show, Read, Ord)

selectAll :: SymbolSelection
selectAll = SymbolSelection True True True True True False

rawSymbolSelection :: SymbolSelection -> Raw.ShowFlag
rawSymbolSelection s = foldr ((.|.) . fst) zeroBits . filter snd $
    [ (Raw.ShowCSP, selectCSP s)
    , (Raw.ShowShown, selectShown s)
    , (Raw.ShowAtoms, selectAtoms s)
    , (Raw.ShowTerms, selectTerms s)
    , (Raw.ShowExtra, selectExtra s)
    , (Raw.ShowComplement, useComplement s) ]

selectNone :: SymbolSelection
selectNone = SymbolSelection False False False False False False

modelType :: (MonadIO m, MonadThrow m) => Model s -> m ModelType
modelType (Model m) = ModelType <$> marshall1 (Raw.modelType m)

modelNumber :: (MonadIO m, MonadThrow m) => Model s -> m Natural
modelNumber (Model m) = fromIntegral <$> marshall1 (Raw.modelNumber m)

modelSymbols :: (MonadIO m) => Model s -> SymbolSelection -> m [Symbol s]
modelSymbols (Model m) selection = liftIO $ do
    let flags = rawSymbolSelection selection
    len <- marshall1 (Raw.modelSymbolsSize m flags)
    allocaArray (fromIntegral len) $ \arr -> do
        marshall0 (Raw.modelSymbols m flags arr len)
        as <- peekArray (fromIntegral len) arr
        return $ fmap Symbol as

contains :: (MonadIO m, MonadThrow m) => Model s -> Symbol s -> m Bool
contains (Model m) s = toBool <$> marshall1 (Raw.modelContains m (rawSymbol s))

costVectors :: (MonadIO m) => Model s -> m [Integer]
costVectors (Model m) = liftIO $ do
    len <- marshall1 (Raw.modelCostSize m)
    allocaArray (fromIntegral len) $ \arr -> do
        marshall0 (Raw.modelCost m arr len)
        as <- peekArray (fromIntegral len) arr
        return $ fmap fromIntegral as

optimalityProven :: (MonadIO m, MonadThrow m) => Model s -> m Bool
optimalityProven (Model m) = toBool <$> marshall1 (Raw.modelOptimalityProven m)

context :: (MonadIO m, MonadThrow m) => Model s -> m (SolveControl s)
context (Model m) = SolveControl <$> marshall1 (Raw.modelContext m)

threadID :: (MonadIO m, MonadThrow m) => SolveControl s -> m Integer
threadID (SolveControl s) = fromIntegral <$>
    marshall1 (Raw.solveControlThreadId s)

modelAddClause :: (MonadIO m, MonadThrow m, Foldable t)
               => SolveControl s -> t (SymbolicLiteral s) -> m ()
modelAddClause (SolveControl s) lits = marshall0 $ 
    withArrayLen (map rawSymLit . toList $ lits) $ \len arr ->
        Raw.solveControlAddClause s arr (fromIntegral len)
