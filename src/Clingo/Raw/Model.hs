{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Model
(
    -- * Inspecting Models
    modelType,
    modelNumber,
    modelSymbolsSize,
    modelSymbols,
    modelContains,
    modelCostSize,
    modelCost,
    modelOptimalityProven,

    -- * Adding Clauses
    modelContext,
    solveControlThreadId,
    solveControlAddClause
)
where

import Control.Monad.IO.Class
import Foreign
import Foreign.C
import Clingo.Raw.Types
import Clingo.Raw.Enums

foreign import ccall "clingo.h clingo_model_type" modelTypeFFI ::
    Model -> ModelType -> IO CBool
foreign import ccall "clingo.h clingo_model_number" modelNumberFFI ::
    Model -> Ptr Word64 -> IO CBool
foreign import ccall "clingo.h clingo_model_symbols_size" modelSymbolsSizeFFI ::
    Model -> ShowFlag -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_model_symbols" modelSymbolsFFI ::
    Model -> ShowFlag -> Ptr Symbol -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_model_contains" modelContainsFFI ::
    Model -> Symbol -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_model_cost_size" modelCostSizeFFI ::
    Model -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_model_cost" modelCostFFI ::
    Model -> Ptr Int64 -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_model_optimality_proven" 
    modelOptimalityProvenFFI ::
    Model -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_model_context" modelContextFFI ::
    Model -> Ptr SolveControl -> IO CBool
foreign import ccall "clingo.h clingo_solve_control_thread_id" 
    solveControlThreadIdFFI ::
    SolveControl -> Ptr Identifier -> IO CBool
foreign import ccall "clingo.h clingo_solve_control_add_clause" 
    solveControlAddClauseFFI ::
    SolveControl -> Ptr SymbolicLiteral -> CSize -> IO CBool

modelType :: MonadIO m => Model -> ModelType -> m CBool
modelType a b = liftIO $ modelTypeFFI a b

modelNumber :: MonadIO m => Model -> Ptr Word64 -> m CBool
modelNumber a b = liftIO $ modelNumberFFI a b

modelSymbolsSize :: MonadIO m => Model -> ShowFlag -> Ptr CSize -> m CBool
modelSymbolsSize a b c = liftIO $ modelSymbolsSizeFFI a b c

modelSymbols :: MonadIO m => Model -> ShowFlag -> Ptr Symbol -> CSize 
                          -> m CBool
modelSymbols a b c d = liftIO $ modelSymbolsFFI a b c d

modelContains :: MonadIO m => Model -> Symbol -> Ptr CBool -> m CBool
modelContains a b c = liftIO $ modelContainsFFI a b c

modelCostSize :: MonadIO m => Model -> Ptr CSize -> m CBool
modelCostSize a b = liftIO $ modelCostSizeFFI a b

modelCost :: MonadIO m => Model -> Ptr Int64 -> CSize -> m CBool
modelCost a b c = liftIO $ modelCostFFI a b c

modelOptimalityProven :: MonadIO m => Model -> Ptr CBool -> m CBool
modelOptimalityProven a b = liftIO $ modelOptimalityProvenFFI a b

modelContext :: MonadIO m => Model -> Ptr SolveControl -> m CBool
modelContext a b = liftIO $ modelContextFFI a b

solveControlThreadId :: MonadIO m => SolveControl -> Ptr Identifier 
                     -> m CBool
solveControlThreadId a b = liftIO $ solveControlThreadIdFFI a b

solveControlAddClause :: MonadIO m => SolveControl -> Ptr SymbolicLiteral 
                                   -> CSize -> m CBool
solveControlAddClause a b c = liftIO $ solveControlAddClauseFFI a b c
