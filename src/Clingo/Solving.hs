module Clingo.Solving
(
    ResultReady(..),
    MonadSolve(..),
    solverClose,
    allModels
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Clingo.Model (MonadModel)
import Clingo.Internal.Types
import Foreign.Ptr
import Foreign.Marshal.Utils
import Clingo.Internal.Utils
import qualified Clingo.Raw as Raw

data ResultReady = Ready | NotReady
    deriving (Eq, Show, Read, Ord)

toRR :: Bool -> ResultReady
toRR True  = Ready
toRR False = NotReady

class MonadModel m => MonadSolve m where
    -- | Get the next solve result.
    getResult    :: Solver s -> m s SolveResult

    -- | Get the next model if it exists.
    getModel     :: Solver s -> m s (Maybe (Model s))

    -- | Wait for the specified time to check if the result is ready.
    solverWait   :: Solver s -> Double -> m s ResultReady

    -- | Discard the last model and start search for the next.
    solverResume :: Solver s -> m s ()

    -- | Stop the running search and block until done.
    solverCancel :: Solver s -> m s ()

instance MonadSolve IOSym where
    getResult = getResult'
    getModel = getModel'
    solverWait = solverWait'
    solverResume = solverResume'
    solverCancel = solverCancel'

instance (MonadThrow m, MonadIO m) => MonadSolve (ClingoT m) where
    getResult = getResult'
    getModel = getModel'
    solverWait = solverWait'
    solverResume = solverResume'
    solverCancel = solverCancel'

-- | Convenience method to get all models. Note that this is dependent on the
-- solver configuration!
allModels :: (Monad (m s), MonadSolve m) => Solver s -> m s [Model s]
allModels solver = do
    solverResume solver
    m <- getModel solver
    case m of
        Nothing -> pure []
        Just x  -> (x :) <$> allModels solver

getResult' :: (MonadThrow m, MonadIO m) => Solver s -> m SolveResult
getResult' (Solver s) = fromRawSolveResult <$> marshal1 (Raw.solveHandleGet s)

getModel' :: (MonadThrow m, MonadIO m) => Solver s -> m (Maybe (Model s))
getModel' (Solver s) = do
    m@(Raw.Model x) <- marshal1 $ Raw.solveHandleModel s
    pure $ if x == nullPtr then Nothing else Just (Model m)

solverWait' :: MonadIO m => Solver s -> Double -> m ResultReady
solverWait' (Solver s) timeout = do
    x <- marshal1V $ Raw.solveHandleWait s (realToFrac timeout)
    pure . toRR . toBool $ x

solverResume' :: (MonadThrow m, MonadIO m) => Solver s -> m ()
solverResume' (Solver s) = marshal0 $ Raw.solveHandleResume s

solverCancel' :: (MonadThrow m, MonadIO m) => Solver s -> m ()
solverCancel' (Solver s) = marshal0 $ Raw.solveHandleCancel s

-- | Stops the running search and releases the handle. Blocks until search is
-- stopped.
solverClose :: (MonadThrow m, MonadIO m) => Solver s -> ClingoT m s ()
solverClose (Solver s) = marshal0 $ Raw.solveHandleClose s
