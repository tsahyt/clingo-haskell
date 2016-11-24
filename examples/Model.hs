{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Clingo.Control
import Clingo.Symbol
import Clingo.Model
import Data.List (intersperse)
import Data.Text (Text)
import System.Environment (getArgs)

import Text.Printf
import qualified Data.Text.IO as T

printModel :: Model s -> Text -> SymbolSelection -> IO ()
printModel m label s = do
    syms <- mapM prettySymbol =<< modelSymbols m s
    T.putStr (label `mappend` ": ")
    T.putStrLn . mconcat $ intersperse " " syms

onModel :: Model s -> IO Continue
onModel m = do  
    t <- modelType m
    n <- modelNumber m
    let tstring = case t of
            StableModel -> "Stable model"
            BraveConsequences -> "Brave consequences"
            CautiousConsequences -> "Cautious consequences"

    printf "%s %d:\n" (tstring :: String) n
    mapM_ (uncurry (printModel m)) 
        [ ("  shown", selectNone { selectShown = True })
        , ("  atoms", selectNone { selectAtoms = True })
        , ("  terms", selectNone { selectTerms = True })
        , (" ~atoms", selectNone { selectAtoms = True
                                 , useComplement = True })
        ]

    return Continue
    
main :: IO ()
main = getArgs >>= \args -> 
    withClingo (defaultClingo { clingoArgs = args }) $ \ctrl -> do
        addProgram ctrl "base" [] "1 {a; b} 1. #show c : b. #show a/0."
        ground ctrl [Part "base" []] Nothing
        void $ solve ctrl (Just onModel) []
