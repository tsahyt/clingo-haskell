{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Clingo.Control
import Clingo.Symbol
import Clingo.Model
import Clingo.Iterative

import Text.Printf
import qualified Data.Text.IO as T

printModel :: Model s -> Clingo s ()
printModel m = do
    syms <- mapM prettySymbol
        =<< modelSymbols m (selectNone { selectShown = True }) 
    liftIO (putStr "Model: " >> print syms)
    
main :: IO ()
main = withDefaultClingo $ \ctrl -> do
    addProgram ctrl "base" [] "a :- not b. b :- not a."
    ground ctrl [Part "base" []] Nothing
    loop =<< solveIterative ctrl []

    where loop iter = do
              m <- iterativelyNext iter
              case m of
                  Nothing -> iterativelyClose iter >> return ()
                  Just m' -> printModel m' >> loop iter
