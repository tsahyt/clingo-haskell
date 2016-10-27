{-# LANGUAGE OverloadedStrings #-}
module Main where

import Clingo.Control

main :: IO ()
main = withClingo $ \ctrl -> do
    addProgram ctrl "base" [] "a :- not b. b :- not a."
    ground ctrl [] Nothing
    res <- solve ctrl (Just $ const (pure True)) []
    print res
