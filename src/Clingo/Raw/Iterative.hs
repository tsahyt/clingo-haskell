{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Iterative
(
    solveIterativelyNext,
    solveIterativelyGet,
    solveIterativelyClose
)
where

import Control.Monad.IO.Class
import Clingo.Raw.Enums
import Clingo.Raw.Types
import Foreign

foreign import ccall "clingo.h clingo_solve_iteratively_next" solveIterativelyNextFFI ::
    Ptr IterSolver -> Ptr (Ptr Model) -> IO CBool
foreign import ccall "clingo.h clingo_solve_iteratively_get" solveIterativelyGetFFI ::
    Ptr IterSolver -> Ptr SolveResult -> IO CBool
foreign import ccall "clingo.h clingo_solve_iteratively_close" solveIterativelyCloseFFI ::
    Ptr IterSolver -> IO CBool

solveIterativelyNext :: MonadIO m 
                     => Ptr IterSolver -> Ptr (Ptr Model) -> m CBool
solveIterativelyNext a b = liftIO $ solveIterativelyNextFFI a b

solveIterativelyGet :: MonadIO m 
                    => Ptr IterSolver -> Ptr SolveResult -> m CBool
solveIterativelyGet a b = liftIO $ solveIterativelyGetFFI a b

solveIterativelyClose :: MonadIO m 
                      => Ptr IterSolver -> m CBool
solveIterativelyClose a = liftIO $ solveIterativelyCloseFFI a
