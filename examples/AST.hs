{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class

import Clingo.AST
import Clingo.Control
import Clingo.Model
import Clingo.ProgramBuilding
import Clingo.Symbol
import Clingo.Solving

printModel :: (MonadIO (m s), MonadModel m) => Model s -> m s ()
printModel m = do
    syms <- map prettySymbol
        <$> modelSymbols m (selectNone { selectShown = True }) 
    liftIO (putStr "Model: " >> print syms)

rewrite :: Term a -> Statement a b -> Statement a b
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
        atom = TermSymbol loc sym

    -- add rewritten statements into the builder
    addStatements builder . map (rewrite atom)
        =<< parseProgram "a :- not b. b :- not a." Nothing 20

    -- add #external enable.
    addStatements builder [ StmtExternal loc (External atom []) ]

    ground [Part "base" []] Nothing

    liftIO $ putStrLn "Solving with enable = false..."
    withSolver [] (allModels >=> mapM_ printModel)

    liftIO $ putStrLn "Solving with enable = true..."
    assignExternal sym TruthTrue
    withSolver [] (allModels >=> mapM_ printModel)

    liftIO $ putStrLn "Solving with enable = false..."
    assignExternal sym TruthFalse
    withSolver [] (allModels >=> mapM_ printModel)
