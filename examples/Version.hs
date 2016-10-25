module Main where

import Text.Printf
import Clingo.Control

main = do
    (a,b,c) <- version
    printf "Hello, this is clingo version %d.%d.%d.\n" a b c
