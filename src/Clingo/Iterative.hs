module Clingo.Iterative
(
    IterSolver,
    SolveResult (..),
    exhausted,
    nextModel,
    getSolveResult,
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Foreign

import qualified Clingo.Raw as Raw

import Clingo.Internal.Types
import Clingo.Internal.Utils

nextModel :: IterSolver s (Maybe (Model s))
nextModel = askIter >>= \s -> do
    Raw.Model mptr <- marshall1 (Raw.solveIterativelyNext s)
    return $ if mptr == nullPtr 
        then Nothing
        else Just (Model (Raw.Model mptr))

getSolveResult :: IterSolver s SolveResult
getSolveResult = askIter >>= \s ->  
    fromRawSolveResult <$> marshall1 (Raw.solveIterativelyGet s)
