{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clingo
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

groundCallback ::
       Location
    -> Text
    -> [PureSymbol]
    -> IO (Either Text (NonEmpty PureSymbol))
groundCallback _ "gcd" [PureNumber a, PureNumber b] =
    pure . Right . pure . PureNumber $ gcd a b
groundCallback _ _ _ = pure $ Left "function not found"

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
        ground [Part "base" []] (Just groundCallback)
        withSolver [] (allModels >=> mapM_ printModel)
