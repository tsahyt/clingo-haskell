{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Clingo.Control
import Clingo.Symbol
import Clingo.Model
import Clingo.ProgramBuilding
import Clingo.Inspection.Symbolic

import Text.Printf
import qualified Data.Text.IO as T

onModel :: Model s -> IO Continue
onModel m = do
    syms <- mapM prettySymbol
        =<< modelSymbols m (selectNone { selectShown = True }) 
    putStr "Model: " >> print syms
    return Continue
    
main :: IO ()
main = withDefaultClingo $ \ctrl -> do
    addProgram ctrl "base" [] "{a; b; c}."
    ground ctrl [Part "base" []] Nothing

    atoms   <- flip fromSymbolicAtoms (map literal) =<< symbolicAtoms ctrl
    backend ctrl >>= \b -> do
        atomD <- atom b
        addGroundStatements b
            [ rule False [atomD] (take 2 atoms)
            , rule False [] 
                  [ negateAspifLiteral (atomAspifLiteral atomD)
                  , atoms !! 2]
            ]

    void $ solve ctrl (Just onModel) []
