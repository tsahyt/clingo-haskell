{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Maybe
import Clingo.Control
import Clingo.Symbol
import Clingo.Model

import Text.Printf
import qualified Data.Text as T

import Clingo.Inspection.Theory

onModel :: Model s -> IO Continue
onModel m = do
    syms <- mapM prettySymbol
        =<< modelSymbols m (selectNone { selectShown = True }) 
    putStr "Model: " >> print syms
    return Continue

theory :: TheoryAtoms s -> IO ()
theory t = do
    size <- fromTheoryAtoms t length 
    putStrLn $ "number of grounded theory atoms: " ++ show size

    names <- fromTheoryAtoms t (mapMaybe (termName . atomTerm))
    forM_ names $ \n ->
        putStrLn $ "atom: " ++ T.unpack n
    
main :: IO ()
main = withDefaultClingo $ \ctrl -> do
    addProgram ctrl "base" [] $ mconcat
        [ "#theory t {"
        , "  term   { + : 1, binary, left };"
        , "  &a/0 : term, any;"
        , "  &b/1 : term, {=}, term, any"
        , "}."
        , "x :- &a { 1+2 }."
        , "y :- &b(3) { } = 17." ]
    ground ctrl [Part "base" []] Nothing
    theoryAtoms ctrl >>= theory
    void $ solve ctrl (Just onModel) []
