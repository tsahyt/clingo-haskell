{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import Clingo.AST
import Clingo.Control
import Clingo.Model
import Clingo.ProgramBuilding
import Clingo.Symbol

onModel :: Model s -> IO Continue
onModel m = do
    syms <- mapM prettySymbol
        =<< modelSymbols m (selectNone { selectShown = True }) 
    putStr "Model: " >> print syms
    return Continue

rewrite :: Term -> Statement -> Statement
rewrite a@(TermSymbol loc sym) (StmtRule l (Rule h b)) = 
    let lit = BodyLiteral loc NoSign (LiteralTerm loc NoSign a)
     in StmtRule l (Rule h (lit : b))
rewrite _ x = x

main :: IO ()
main = withDefaultClingo $ \ctrl -> do
    builder <- programBuilder ctrl

    -- create enable atom
    sym  <- createId ctrl "enable" True
    let loc  = Location "<rewrite>" "<rewrite>" 0 0 0 0
        atom = TermSymbol loc (wrapSymbol sym)

    -- add rewritten statements into the builder
    addStatements builder . map (rewrite atom)
        =<< parseProgram ctrl "a :- not b. b :- not a." Nothing 20

    -- add #external enable.
    addStatements builder [ StmtExternal loc (External atom []) ]

    ground ctrl [Part "base" []] Nothing

    putStrLn "Solving with enable = false..."
    void $ solve ctrl (Just onModel) []

    putStrLn "Solving with enable = true..."
    assignExternal ctrl sym TruthTrue
    void $ solve ctrl (Just onModel) []

    putStrLn "Solving with enable = false..."
    assignExternal ctrl sym TruthFalse
    void $ solve ctrl (Just onModel) []
