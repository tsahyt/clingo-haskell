module Clingo.Raw.Iterative
(
)
where

foreign import ccall "clingo.h clingo_solve_iteratively_next" solveIterativelyNextFFI ::
foreign import ccall "clingo.h clingo_solve_iteratively_get" solveIterativelyGetFFI ::
foreign import ccall "clingo.h clingo_solve_iteratively_close" solveIterativelyCloseFFI ::
