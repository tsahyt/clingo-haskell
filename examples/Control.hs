{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Clingo.Control
import Clingo.Symbol
import Clingo.Model

import Text.Printf
import qualified Data.Text.IO as T

onModel :: Model s -> IO Bool
onModel m = do
    syms <- modelSymbols m (selectNone { selectShown = True }) 
            >>= mapM prettySymbol
    print syms
    return True
    
main :: IO ()
main = withClingo $ \ctrl -> do
    addProgram ctrl "base" [] "a :- not b. b :- not a."
    ground ctrl [] Nothing
    void $ solve ctrl (Just onModel) []
