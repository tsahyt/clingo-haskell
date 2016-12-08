{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Clingo.Symbol
import Clingo.Control
import Clingo.Configuration
import Clingo.Model
import Clingo.Statistics

import Data.StateVar

import Data.Text.Lazy (fromStrict)
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

instance Pretty v => Pretty (StatsTree v) where
    pretty (SValue v) = pretty v
    pretty (SArray x) = vcat $ map (nest 1 . pretty . snd) x
    pretty (SMap s)   = vcat $ map (nest 1 . go) s
        where go (k,t) = text (fromStrict k) <> colon <> line 
                      <> nest 1 (pretty t)

onModel :: Model s -> IOSym s Continue
onModel m = do
    syms <- map prettySymbol
        <$> modelSymbols m (selectNone { selectShown = True }) 
    liftIO (putStr "Model: " >> print syms)
    return Continue

main :: IO ()
main = withDefaultClingo $ do
    -- Set configuration to put out more stats
    Just sconfig <- flip fromConfig (atMap "stats" >=> value) =<< configuration
    sconfig $= "1"

    -- Ground and solve a simple program
    addProgram "base" [] "a :- not b. b :- not a."
    ground [Part "base" []] Nothing
    _ <- solve (Just onModel) []
    stats <- statistics

    -- Print whole stats tree
    liftIO (putStrLn "\nStatistics")
    fullTree <- subStats stats pure
    liftIO (putDoc (pretty fullTree <> line))

    -- Print just the solving subtree
    liftIO (putStrLn "\nSelected solving.solver statistics")
    solving <- subStats stats (atMap "solving" >=> atMap "solvers")
    liftIO (putDoc (pretty solving <> line))

    -- Selecting only number of equations
    liftIO (putStrLn "\nNumber of equations")
    eqs <- fromStats stats (atMap "problem" >=> atMap "lp" >=> atMap "eqs"
                            >=> value)
    liftIO (putDoc (pretty eqs <> line))
