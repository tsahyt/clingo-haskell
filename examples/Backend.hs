{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Clingo.Control
import Clingo.Symbol
import Clingo.Solving
import Clingo.Model
import Clingo.ProgramBuilding
import Clingo.Inspection.Symbolic

import Text.Printf
import qualified Data.Text.IO as T

printModel :: (MonadIO (m s), MonadModel m) => Model s -> m s ()
printModel m = do
    syms <- map prettySymbol
        <$> modelSymbols m (selectNone { selectShown = True }) 
    liftIO (putStr "Model: " >> print syms)
    
main :: IO ()
main = withDefaultClingo $ do
    addProgram "base" [] "{a; b; c}."
    ground [Part "base" []] Nothing

    atoms <- flip fromSymbolicAtoms (map literal) =<< symbolicAtoms
    backend >>= \b -> do
        atomD <- atom b Nothing
        liftIO $ print atomD
        addGroundStatements b
            [ rule False [atomD] (take 2 atoms)
            , rule False [] 
                  [ negateAspifLiteral (atomAspifLiteral atomD)
                  , atoms !! 2]
            ]

    withSolver [] (withModel printModel)
