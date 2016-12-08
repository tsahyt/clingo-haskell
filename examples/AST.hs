{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class

import Clingo.AST
import Clingo.Control
import Clingo.Model
import Clingo.ProgramBuilding
import Clingo.Symbol

onModel :: Model s -> IOSym s Continue
onModel m = do
    syms <- map prettySymbol
        <$> modelSymbols m (selectNone { selectShown = True }) 
    liftIO (putStr "Model: " >> print syms)
    return Continue

rewrite :: Term -> Statement -> Statement
rewrite a@(TermSymbol loc sym) (StmtRule l (Rule h b)) = 
    let lit = BodyLiteral loc NoSign (LiteralTerm loc NoSign a)
     in StmtRule l (Rule h (lit : b))
rewrite _ x = x

main :: IO ()
main = withDefaultClingo $ do
    builder <- programBuilder

    -- create enable atom
    sym  <- createId "enable" True
    let loc  = Location "<rewrite>" "<rewrite>" 0 0 0 0
        atom = TermSymbol loc (wrapSymbol sym)

    -- add rewritten statements into the builder
    addStatements builder . map (rewrite atom)
        =<< parseProgram "a :- not b. b :- not a." Nothing 20

    -- add #external enable.
    addStatements builder [ StmtExternal loc (External atom []) ]

    ground [Part "base" []] Nothing

    liftIO $ putStrLn "Solving with enable = false..."
    void $ solve (Just onModel) []

    liftIO $ putStrLn "Solving with enable = true..."
    assignExternal sym TruthTrue
    void $ solve (Just onModel) []

    liftIO $ putStrLn "Solving with enable = false..."
    assignExternal sym TruthFalse
    void $ solve (Just onModel) []
