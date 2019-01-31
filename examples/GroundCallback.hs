{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clingo
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

type SymbolInjectionM
     = Location -> Text -> [PureSymbol] -> ExceptT Text IO [PureSymbol]

groundCallback :: SymbolInjectionM
groundCallback _ "empty" _ = pure []
groundCallback _ "gcd" [PureNumber a, PureNumber b] =
    pure [PureNumber $ gcd a b]
groundCallback _ _ _ = throwError "function not found"

printModel :: Model s -> IOSym s ()
printModel m = do
    syms <-
        map prettySymbol <$> modelSymbols m (selectNone {selectShown = True})
    liftIO (putStr "Model: " >> print syms)

main :: IO ()
main =
    withDefaultClingo $ do
        addProgram
            "base"
            []
            "p(210,213). p(1365,385). gcd(X,Y,@gcd(X,Y)) :- p(X,Y). foo :- a(@empty(3))."
        ground
            [Part "base" []]
            (Just $ \l t s -> runExceptT (groundCallback l t s))
        withSolver [] (allModels >=> mapM_ printModel)
