{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Clingo.Control
import Clingo.Symbol
import Clingo.Model
import Clingo.Inspection.Symbolic

import Data.Maybe
import Data.Text (Text)

import qualified Data.Text.IO as T

printSymbol :: SymbolicAtom s -> Text
printSymbol atom =
    let isFact = guard (fact atom) *> pure ", fact"
        isExternal = guard (external atom) *> pure ", external"
        name = prettySymbol (symbol atom)
     in mconcat . catMaybes $ [ Just "  ", Just name, isFact, isExternal ]
    
main :: IO ()
main = withDefaultClingo $ do
    addProgram "base" [] "a. {b}. #external c."
    ground [Part "base" []] Nothing

    liftIO (T.putStrLn "Symbolic Atoms:")
    sa <- symbolicAtoms 
    mapM_ (liftIO . T.putStrLn . printSymbol) =<< fromSymbolicAtoms sa id
