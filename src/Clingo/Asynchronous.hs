module Clingo.Asynchronous
(
    AsyncSolver,
    SolveResult (..),
    exhausted,
    asyncGet,
    asyncWait,
    asyncCancel
)
where

import Foreign.Marshal.Utils

import qualified Clingo.Raw as Raw
import Clingo.Internal.Utils
import Clingo.Internal.Types

asyncGet :: AsyncSolver s -> Clingo s SolveResult
asyncGet (AsyncSolver a) = 
    fromRawSolveResult <$> marshall1 (Raw.solveAsyncGet a)

asyncWait :: AsyncSolver s -> Double -> Clingo s Bool
asyncWait (AsyncSolver a) timeout = 
    toBool <$> marshall1 (Raw.solveAsyncWait a (realToFrac timeout))

asyncCancel :: AsyncSolver s -> Clingo s ()
asyncCancel (AsyncSolver a) = marshall0 (Raw.solveAsyncCancel a)
