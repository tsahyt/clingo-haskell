{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Clingo.Control
import Clingo.Configuration
import Clingo.Symbol
import Clingo.Model
import Data.StateVar

onModel :: Model s -> IO Continue
onModel m = do
    syms <- mapM prettySymbol
        =<< modelSymbols m (selectNone { selectShown = True }) 
    putStr "Model: " >> print syms
    return Continue

(>>?=) :: (Monad m, Foldable t) => m (t a) -> (a -> m b) -> m ()
a >>?= b = a >>= mapM_ b
    
main :: IO ()
main = withDefaultClingo $ \ctrl -> do
    conf <- configuration ctrl

    -- enumerate all models
    fromConfig conf (atMap "solve" >=> atMap "models" >=> value) >>?= ($= "0")

    -- use berkmin
    fromConfig conf (atMap "solver" >=> atArray 0 >=> atMap "heuristic" 
                      >=> value)
        >>?= ($= "berkmin")
    
    addProgram ctrl "base" [] "a :- not b. b :- not a."
    ground ctrl [Part "base" []] Nothing
    void $ solve ctrl (Just onModel) []
