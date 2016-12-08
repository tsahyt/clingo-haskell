{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Asynchronous
(
    solveAsyncGet,
    solveAsyncWait,
    solveAsyncCancel
)
where

import Control.Monad.IO.Class
import Foreign
import Foreign.C
import Clingo.Raw.Enums
import Clingo.Raw.Types

foreign import ccall "clingo.h clingo_solve_async_get" 
    solveAsyncGetFFI :: AsyncSolver -> Ptr SolveResult -> IO CBool
foreign import ccall "clingo.h clingo_solve_async_wait" 
    solveAsyncWaitFFI :: AsyncSolver -> CDouble -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_solve_async_cancel" 
    solveAsyncCancelFFI :: AsyncSolver -> IO CBool

solveAsyncGet :: MonadIO m => AsyncSolver -> Ptr SolveResult -> m CBool
solveAsyncGet a b = liftIO $ solveAsyncGetFFI a b

solveAsyncWait :: MonadIO m => AsyncSolver -> CDouble -> Ptr CBool 
                            -> m CBool
solveAsyncWait a b c = liftIO $ solveAsyncWaitFFI a b c

solveAsyncCancel :: MonadIO m => AsyncSolver -> m CBool
solveAsyncCancel a = liftIO $ solveAsyncCancelFFI a
