{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Clingo.Control
import Clingo.Symbol
import Clingo.Model
import Clingo.ProgramBuilding
import Clingo.Inspection.Symbolic

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
    addProgram "base" [] "{a; b; c}."
    ground [Part "base" []] Nothing

    atoms <- flip fromSymbolicAtoms (map literal) =<< symbolicAtoms
    backend >>= \b -> do
        atomD <- atom b
        addGroundStatements b
            [ rule False [atomD] (take 2 atoms)
            , rule False [] 
                  [ negateAspifLiteral (atomAspifLiteral atomD)
                  , atoms !! 2]
            ]

    void $ solve (Just onModel) []
