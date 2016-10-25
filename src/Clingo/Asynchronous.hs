module Clingo.Asynchronous
(
    asyncGet,
    asyncWait,
    asyncCancel
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch

import Foreign.Marshal.Utils

import qualified Clingo.Raw as Raw
import Clingo.Internal.Utils
import Clingo.Internal.Types

asyncGet :: (MonadIO m, MonadThrow m) 
         => AsyncSolver s -> m Raw.SolveResult
asyncGet (AsyncSolver a) = marshall1 (Raw.solveAsyncGet a)

asyncWait :: (MonadIO m, MonadThrow m)
          => AsyncSolver s -> Double -> m Bool
asyncWait (AsyncSolver a) timeout = 
    toBool <$> marshall1 (Raw.solveAsyncWait a (realToFrac timeout))

asyncCancel :: (MonadIO m, MonadThrow m)
            => AsyncSolver s -> m ()
asyncCancel (AsyncSolver a) = marshall0 (Raw.solveAsyncCancel a)
