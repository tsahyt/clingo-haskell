{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI #-}
module Clingo.Raw.Solving
(
    solveHandleGet,
    solveHandleWait,
    solveHandleModel,
    solveHandleResume,
    solveHandleCancel,
    solveHandleClose
)
where

import Control.Monad.IO.Class

import Foreign
import Foreign.C.Types

import Clingo.Raw.Types
import Clingo.Raw.Enums

foreign import ccall "clingo.h clingo_solve_handle_get" 
                     solveHandleGetFFI ::
    SolveHandle -> Ptr SolveResult -> IO CBool
foreign import ccall interruptible "clingo.h clingo_solve_handle_wait"
                     solveHandleWaitFFI ::
    SolveHandle -> CDouble -> Ptr CBool -> IO ()
foreign import ccall "clingo.h clingo_solve_handle_model"
                     solveHandleModelFFI ::
    SolveHandle -> Ptr Model -> IO CBool
foreign import ccall "clingo.h clingo_solve_handle_resume"
                     solveHandleResumeFFI ::
    SolveHandle -> IO CBool
foreign import ccall "clingo.h clingo_solve_handle_cancel"
                     solveHandleCancelFFI ::
    SolveHandle -> IO CBool
foreign import ccall "clingo.h clingo_solve_handle_close"
                     solveHandleCloseFFI ::
    SolveHandle -> IO CBool

solveHandleGet :: MonadIO m => SolveHandle -> Ptr SolveResult -> m CBool
solveHandleGet a b = liftIO $ solveHandleGetFFI a b

solveHandleWait :: MonadIO m => SolveHandle -> CDouble -> Ptr CBool -> m ()
solveHandleWait a b c = liftIO $ solveHandleWaitFFI a b c

solveHandleModel :: MonadIO m => SolveHandle -> Ptr Model -> m CBool
solveHandleModel a b = liftIO $ solveHandleModelFFI a b

solveHandleResume :: MonadIO m => SolveHandle -> m CBool
solveHandleResume a = liftIO $ solveHandleResumeFFI a

solveHandleCancel :: MonadIO m => SolveHandle -> m CBool
solveHandleCancel a = liftIO $ solveHandleCancelFFI a

solveHandleClose :: MonadIO m => SolveHandle -> m CBool
solveHandleClose a = liftIO $ solveHandleCloseFFI a
