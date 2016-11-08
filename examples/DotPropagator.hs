{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Clingo.Control
import Clingo.Propagation
import Clingo.Inspection.Symbolic

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

sleepTime :: Int
sleepTime = 10000

mapIO :: (MonadIO m, Foldable t) => IO b -> t a -> m ()
mapIO = mapM_ . const . liftIO

writeDots :: [Literal s] -> Propagation 'Solving s ()
writeDots = mapIO (putChar '.' >> hFlush stdout >> threadDelay sleepTime)

takeDots :: [Literal s] -> Propagation 'Solving s ()
takeDots = mapIO (putStr "\b \b" >> hFlush stdout >> threadDelay sleepTime)

watchAll :: Propagation 'Init s ()
watchAll = mapM_ ((addWatch =<<) . solverLiteral . literal)
           =<< flip fromSymbolicAtoms id =<< propSymbolicAtoms

main :: IO ()
main = withDefaultClingo $ \ctrl -> do
    path <- head <$> liftIO getArgs
    registerPropagator ctrl False $ emptyPropagator
        { propInit = Just watchAll
        , propPropagate = Just writeDots
        , propUndo = Just takeDots
        }
    loadProgram ctrl path
    ground ctrl [Part "base" []] Nothing
    solve ctrl Nothing []
    putChar '\n'
