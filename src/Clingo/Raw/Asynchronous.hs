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

foreign import ccall "clingo.h clingo_solve_async_get" solveAsyncGetFFI ::
    Ptr AsyncSolver -> Ptr SolveResult -> IO CBool
foreign import ccall "clingo.h clingo_solve_async_wait" solveAsyncWaitFFI ::
    Ptr AsyncSolver -> CDouble -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_solve_async_cancel" solveAsyncCancelFFI ::
    Ptr AsyncSolver -> IO CBool

solveAsyncGet :: MonadIO m => Ptr AsyncSolver -> Ptr SolveResult -> m CBool
solveAsyncGet a b = liftIO $ solveAsyncGetFFI a b

solveAsyncWait :: MonadIO m => Ptr AsyncSolver -> CDouble -> Ptr CBool 
                            -> m CBool
solveAsyncWait a b c = liftIO $ solveAsyncWaitFFI a b c

solveAsyncCancel :: MonadIO m => Ptr AsyncSolver -> m CBool
solveAsyncCancel a = liftIO $ solveAsyncCancelFFI a
