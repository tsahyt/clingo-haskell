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

    MonadModel (..)
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Foldable
import Foreign

import Numeric.Natural

import qualified Clingo.Raw as Raw
import Clingo.Internal.Symbol
import Clingo.Internal.Types
import Clingo.Internal.Utils

newtype SolveControl s = SolveControl Raw.SolveControl

newtype ModelType = ModelType Raw.ModelType

pattern StableModel = ModelType Raw.StableModel
pattern BraveConsequences = ModelType Raw.BraveConsequences
pattern CautiousConsequences = ModelType Raw.CautiousConsequences

-- | Type for building symbol selections.
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

class MonadSymbol m => MonadModel m where
    -- | Get the type of a Model.
    modelType :: Model s -> m s ModelType
    -- | Get the number of a Model.
    modelNumber :: Model s -> m s Natural
    -- | Get the selected symbols from a Model.
    modelSymbols :: Model s -> SymbolSelection -> m s [Symbol s]
    -- | Constant time lookup to test whether an atom is in a model.
    contains :: Model s -> Symbol s -> m s Bool
    -- | Get the cost vector of a Model
    costVector :: Model s -> m s [Integer]
    -- | Check whether optimality of a model has been proven.
    optimalityProven :: Model s -> m s Bool
    -- | Get the associated 'SolveControl' of a Model.
    context :: Model s -> m s (SolveControl s)
    -- | Add a clause from the model callback.
    modelAddClause :: Foldable t 
                   => SolveControl s -> t (SymbolicLiteral s) -> m s ()

instance MonadModel IOSym where
    modelType = modelType'
    modelNumber = modelNumber'
    modelSymbols = modelSymbols'
    contains = contains'
    costVector = costVector'
    optimalityProven = optimalityProven'
    context = context'
    modelAddClause = modelAddClause'

instance MonadModel Clingo where
    modelType = modelType'
    modelNumber = modelNumber'
    modelSymbols = modelSymbols'
    contains = contains'
    costVector = costVector'
    optimalityProven = optimalityProven'
    context = context'
    modelAddClause = modelAddClause'

modelType' :: (MonadIO m, MonadThrow m) => Model s -> m ModelType
modelType' (Model m) = ModelType <$> marshall1 (Raw.modelType m)

modelNumber' :: (MonadIO m, MonadThrow m) => Model s -> m Natural
modelNumber' (Model m) = fromIntegral <$> marshall1 (Raw.modelNumber m)

modelSymbols' :: (MonadIO m) => Model s -> SymbolSelection -> m [Symbol s]
modelSymbols' (Model m) selection = liftIO $ do
    let flags = rawSymbolSelection selection
    len <- marshall1 (Raw.modelSymbolsSize m flags)
    allocaArray (fromIntegral len) $ \arr -> do
        marshall0 (Raw.modelSymbols m flags arr len)
        as <- peekArray (fromIntegral len) arr
        mapM pureSymbol as

contains' :: (MonadIO m, MonadThrow m) => Model s -> Symbol s -> m Bool
contains' (Model m) s = toBool <$> marshall1 (Raw.modelContains m (rawSymbol s))

costVector' :: (MonadIO m) => Model s -> m [Integer]
costVector' (Model m) = liftIO $ do
    len <- marshall1 (Raw.modelCostSize m)
    allocaArray (fromIntegral len) $ \arr -> do
        marshall0 (Raw.modelCost m arr len)
        as <- peekArray (fromIntegral len) arr
        return $ fmap fromIntegral as

optimalityProven' :: (MonadIO m, MonadThrow m) => Model s -> m Bool
optimalityProven' (Model m) = toBool <$> marshall1 (Raw.modelOptimalityProven m)

context' :: (MonadIO m, MonadThrow m) => Model s -> m (SolveControl s)
context' (Model m) = SolveControl <$> marshall1 (Raw.modelContext m)

modelAddClause' :: (MonadIO m, MonadThrow m, Foldable t)
                => SolveControl s -> t (SymbolicLiteral s) -> m ()
modelAddClause' (SolveControl s) lits = marshall0 $ 
    withArrayLen (map rawSymLit . toList $ lits) $ \len arr ->
        Raw.solveControlAddClause s arr (fromIntegral len)
