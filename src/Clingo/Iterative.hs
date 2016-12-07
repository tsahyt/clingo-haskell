module Clingo.Iterative
(
    IterSolver,
    SolveResult (..),
    exhausted,
    iterativelyNext,
    iterativelyGet,
    iterativelyClose
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Foreign

import qualified Clingo.Raw as Raw

import Clingo.Internal.Types
import Clingo.Internal.Utils

iterativelyNext :: (MonadIO m, MonadThrow m)
                => IterSolver s -> m (Maybe (Model s))
iterativelyNext (IterSolver s) = do
    Raw.Model mptr <- marshall1 (Raw.solveIterativelyNext s)
    return $ if mptr == nullPtr 
        then Nothing
        else Just (Model (Raw.Model mptr))

iterativelyGet :: (MonadIO m, MonadThrow m)
               => IterSolver s -> m SolveResult
iterativelyGet (IterSolver s) = 
    fromRawSolveResult <$> marshall1 (Raw.solveIterativelyGet s)

iterativelyClose :: (MonadIO m, MonadThrow m)
                 => IterSolver s -> m ()
iterativelyClose (IterSolver s) = marshall0 (Raw.solveIterativelyClose s)
