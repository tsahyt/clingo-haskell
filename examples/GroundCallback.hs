{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clingo
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

type SymbolInjectionM
     = Location -> Text -> [PureSymbol] -> ExceptT Text IO (NonEmpty PureSymbol)

groundCallback :: SymbolInjectionM
groundCallback _ "gcd" [PureNumber a, PureNumber b] =
    pure (pure (PureNumber $ gcd a b))
groundCallback _ _ _ = throwError "function not found"

printModel :: (MonadIO (m s), MonadModel m) => Model s -> m s ()
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
            "p(210,213). p(1365,385). gcd(X,Y,@gcd(X,Y)) :- p(X,Y)."
        ground
            [Part "base" []]
            (Just $ \l t s -> runExceptT (groundCallback l t s))
        withSolver [] (allModels >=> mapM_ printModel)
