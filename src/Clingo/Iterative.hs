module Clingo.Iterative
(
    iterativelyNext,
    iterativelyGet,
    iterativelyClose
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch

import qualified Clingo.Raw as Raw

import Clingo.Internal.Types
import Clingo.Internal.Utils

iterativelyNext :: (MonadIO m, MonadThrow m)
                => IterSolver s -> m (Model s)
iterativelyNext (IterSolver s) = Model <$> 
    marshall1 (Raw.solveIterativelyNext s)

iterativelyGet :: (MonadIO m, MonadThrow m)
               => IterSolver s -> m SolveResult
iterativelyGet (IterSolver s) = 
    fromRawSolveResult <$> marshall1 (Raw.solveIterativelyGet s)

iterativelyClose :: (MonadIO m, MonadThrow m)
                 => IterSolver s -> m ()
iterativelyClose (IterSolver s) = marshall0 (Raw.solveIterativelyClose s)
