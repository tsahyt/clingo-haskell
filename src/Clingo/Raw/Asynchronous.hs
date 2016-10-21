module Clingo.Raw.Asynchronous
(
)
where

foreign import ccall "clingo.h clingo_solve_async_get" solveAsyncGetFFI ::
foreign import ccall "clingo.h clingo_solve_async_wait" solveAsyncWaitFFI ::
foreign import ccall "clingo.h clingo_solve_async_cancel" solveAsyncCancelFFI ::
