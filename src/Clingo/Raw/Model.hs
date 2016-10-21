{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Model
(
)
where

foreign import ccall "clingo.h clingo_model_type" modelTypeFFI ::
foreign import ccall "clingo.h clingo_model_number" modelNumberFFI ::
foreign import ccall "clingo.h clingo_model_symbols_size" modelSymbolsSizeFFI ::
foreign import ccall "clingo.h clingo_model_symbols" modelSymbolsFFI ::
foreign import ccall "clingo.h clingo_model_contains" modelContainsFFI ::
foreign import ccall "clingo.h clingo_model_cost_size" modelCostSizeFFI ::
foreign import ccall "clingo.h clingo_model_cost" modelCostFFI ::
foreign import ccall "clingo.h clingo_model_optimality_proven" modelOptimalityProvenFFI ::
foreign import ccall "clingo.h clingo_model_context" modelContextFFI ::
foreign import ccall "clingo.h clingo_solve_control_thread_id" solveControlThreadIdFFI ::
foreign import ccall "clingo.h clingo_solve_control_add_clause" solveControlAddClauseFFI ::
