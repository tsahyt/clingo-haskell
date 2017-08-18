{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Random
import Control.Monad.Loops
import Control.Monad.IO.Class
import Clingo.Control
import Clingo.Symbol
import Clingo.Solving
import Clingo.Model
import Data.IORef

import Text.Printf
import qualified Data.Text.IO as T

-- | Approximate Pi with a rather literal translation of the C example.
approxPi :: MonadIO m => MVar Bool -> m Double
approxPi running = liftIO $ do
    let rmax = 512
    samples <- newIORef 0
    incircle <- newIORef 0
    whileM (readMVar running) $ do
        modifyIORef' samples succ
        (x :: Int) <- getRandomR (-rmax, rmax)
        (y :: Int) <- getRandomR (-rmax, rmax)
        when (x * x + y * y <= rmax * rmax) $
            modifyIORef' incircle succ
    s <- readIORef samples
    c <- readIORef incircle
    return $ 4 * fromIntegral c / fromIntegral s
    
main :: IO ()
main = withDefaultClingo $ do
    addProgram "base" [] $
        "#const n = 17.\
       \ 1 { p(X); q(X) } 1 :- X = 1..n.\
       \ :- not n+1 { p(1..n); q(1..n) }."
    ground [Part "base" []] Nothing

    running <- liftIO (newMVar True)
    solver  <- solve SolveModeAsync [] $ Just $ \_ -> do
                   void (liftIO (swapMVar running False))
                   return Continue

    pi <- approxPi running
    liftIO (putStrLn $ "pi = " ++ show pi)
    liftIO . print =<< getResult solver

    solverClose solver
