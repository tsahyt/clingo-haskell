{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Clingo.Control
import Clingo.Symbol
import Clingo.Model

import Text.Printf
import qualified Data.Text.IO as T

onModel :: Model s -> Clingo s Continue
onModel m = do
    syms <- mapM prettySymbol
        =<< modelSymbols m (selectNone { selectShown = True }) 
    liftIO (putStr "Model: " >> print syms)
    return Continue
    
main :: IO ()
main = withDefaultClingo $ do
    addProgram "base" [] "a :- not b. b :- not a."
    ground [Part "base" []] Nothing
    void $ solve (Just onModel) []
