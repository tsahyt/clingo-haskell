{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import Control.Monad
import Clingo.Control
import Clingo.Symbol
import Clingo.Model
import Clingo.Inspection.Symbolic

import Data.Maybe
import Data.Text (Text)

import qualified Data.Text.IO as T

onModel :: Model s -> IO Continue
onModel m = do
    syms <- mapM prettySymbol
        =<< modelSymbols m (selectNone { selectShown = True }) 
    putStr "Model: " >> print syms
    return Continue

printSymbol :: SymbolicAtom s -> IO Text
printSymbol atom = do
    let isFact = guard (fact atom) *> pure ", fact"
        isExternal = guard (external atom) *> pure ", external"
    name <- prettySymbol (symbol atom)
    return $ mconcat . catMaybes $ [ Just "  ", Just name, isFact, isExternal ]
    
main :: IO ()
main = withDefaultClingo $ \ctrl -> do
    addProgram ctrl "base" [] "a. {b}. #external c."
    ground ctrl [Part "base" []] Nothing

    T.putStrLn "Symbolic Atoms:"
    sa <- symbolicAtoms ctrl
    mapM_ (T.putStrLn <=< printSymbol) =<< fromSymbolicAtoms sa id
