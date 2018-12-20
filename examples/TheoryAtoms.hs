{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clingo.Control
import Clingo.Model
import Clingo.ProgramBuilding
import Clingo.Solving
import Clingo.Symbol
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Maybe

import Clingo.Inspection.Theory

printModel :: (MonadIO (m s), MonadModel m) => Model s -> m s ()
printModel m = do
    syms <-
        map prettySymbol <$> modelSymbols m (selectNone {selectShown = True})
    liftIO (putStr "Model: " >> print syms)

theory ::
       (MonadThrow m, MonadIO m)
    => TheoryAtoms s
    -> ClingoT m s (AspifLiteral s)
theory t
    -- obtain number of theory atoms via length
 = do
    size <- fromTheoryAtoms t length
    liftIO (putStrLn $ "number of grounded theory atoms: " ++ show size)
    -- find the atom b/1 and determine whether it has a guard
    atomB <- fromTheoryAtoms t (head . filter (nameIs "b"))
    liftIO
        (putStrLn $
         "theory atom b/1 has guard: " ++ show (isJust . atomGuard $ atomB))
    return (atomLiteral atomB)
  where
    nameIs a x =
        case termName (atomTerm x) of
            Nothing -> False
            Just b -> a == b

main :: IO ()
main =
    withDefaultClingo $ do
        addProgram "base" [] $
            mconcat
                [ "#theory t {"
                , "  term   { + : 1, binary, left };"
                , "  &a/0 : term, any;"
                , "  &b/1 : term, {=}, term, any"
                , "}."
                , "x :- &a { 1+2 }."
                , "y :- &b(3) { } = 17."
                ]
        ground [Part "base" []] Nothing
        lit <- theory =<< theoryAtoms
        flip addGroundStatements [assume [lit]] =<< backend
        withSolver [] (allModels >=> mapM_ printModel)
