{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Clingo.Control
import Clingo.Propagation

import System.Environment (getArgs)

sleepTime :: Int
sleepTime = 100

mapIO :: (MonadIO m, Foldable t) => IO b -> t a -> m ()
mapIO = mapM_ . const . liftIO

writeDots :: [Literal s] -> Propagation 'Solving s ()
writeDots = mapIO (putChar '.' >> threadDelay sleepTime)

takeDots :: [Literal s] -> Propagation 'Solving s ()
takeDots = mapIO (putStr "\b \b" >> threadDelay sleepTime)

main :: IO ()
main = withDefaultClingo $ \ctrl -> do
    path <- head <$> liftIO getArgs
    registerPropagator ctrl False $ emptyPropagator
        { propPropagate = Just writeDots
        , propUndo = Just takeDots
        }
    loadProgram ctrl path
    ground ctrl [Part "base" []] Nothing
    void $ solve ctrl Nothing []
