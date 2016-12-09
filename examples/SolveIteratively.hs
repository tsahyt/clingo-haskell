{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.IO.Class
import Clingo.Control
import Clingo.Symbol
import Clingo.Model
import Clingo.Iterative

import Text.Printf
import qualified Data.Text.IO as T

printModel :: (MonadIO (m s), MonadModel m) => Model s -> m s ()
printModel m = do
    syms <- map prettySymbol
        <$> modelSymbols m (selectNone { selectShown = True }) 
    liftIO (putStr "Model: " >> print syms)
    
main :: IO ()
main = withDefaultClingo $ do
    addProgram "base" [] "a :- not b. b :- not a."
    ground [Part "base" []] Nothing
    solveIterative [] $ unfoldM_ $ do
        m <- nextModel
        mapM_ printModel m
        return m
