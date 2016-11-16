{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Clingo.Control
import Clingo.Symbol
import Clingo.Model
import Clingo.Propagation
import Clingo.Inspection.Symbolic

import Data.Proxy
import Data.Map (Map)
import Data.Maybe
import Data.IntMap (IntMap)
import qualified Data.Map as M
import qualified Data.IntMap as I

import Text.Printf
import qualified Data.Text.IO as T

newtype Hole = Hole Int
    deriving (Eq, Show, Ord)

data PigeonData s = PigeonData
    { placements  :: Map (Literal s) Hole
    , assignments :: IntMap (Map Hole (Literal s))
    }

assignHole :: Integer -> Hole -> Literal s -> PigeonData s -> PigeonData s
assignHole tid hole lit pd = pd 
    { assignments = 
        I.adjust (M.insert hole lit) (fromIntegral tid) (assignments pd) }

unassignHole :: Integer -> Hole -> PigeonData s -> PigeonData s
unassignHole tid hole pd = pd
    { assignments =
        I.adjust (M.delete hole) (fromIntegral tid) (assignments pd) }

forceMVar :: MonadIO m => MVar a -> a -> m ()
forceMVar mvar x = liftIO $ do
    _ <- tryTakeMVar mvar
    putMVar mvar x

onModel :: Model s -> IO Continue
onModel m = do
    syms <- mapM prettySymbol
        =<< modelSymbols m (selectNone { selectShown = True }) 
    putStr "Model: " >> print syms
    return Continue
    
main :: IO ()
main = withDefaultClingo $ \ctrl -> do
    addProgram ctrl "pigeon" ["h","p"] 
        "1 { place(P,H) : H = 1..h } 1 :- P = 1..p."

    holes <- createNumber ctrl 8
    pigeons <- createNumber ctrl 9
    ground ctrl [Part "pigeon" [holes, pigeons]] Nothing

    propState <- liftIO newEmptyMVar
    registerPropagator ctrl False (pigeonator ctrl propState)

    void $ solve ctrl (Just onModel) []

pigeonator :: Clingo s -> MVar (PigeonData s) -> Propagator s
pigeonator ctrl mvar = emptyPropagator
    { propInit = Just $ do
        -- obtain place/2 atoms
        placeSig <- signatureCreate ctrl "place" 2 True
        watches <- propSymbolicAtoms 
               >>= \sa -> fromSymbolicAtomsSig sa placeSig id

        -- watch and create initial placements
        mapM_ ((addWatch =<<) . solverLiteral . literal) watches
        ps <- forM watches $ \atom -> do
                  hole <- runMaybeT $ do
                              arg <- MaybeT $ symbolGetArg (symbol atom) 1
                              Hole . fromIntegral <$> MaybeT (symbolNumber arg)
                  lit <- solverLiteral . literal $ atom
                  return $ (,) <$> pure lit <*> hole

        -- initialize the PigeonData structure for all threads
        threads <- fromIntegral <$> countThreads
        forceMVar mvar $ 
            let xs = zip (take threads [0..]) (repeat M.empty)
             in PigeonData (M.fromList . catMaybes $ ps) (I.fromList xs)

    , propPropagate = Just $ \changes -> do
        -- get previous assignment
        thread <- getThreadId
        state  <- liftIO (readMVar mvar)
        let Just holes = fromIntegral thread `I.lookup` assignments state

        -- apply and check assignments done by solver
        forM_ changes $ \lit -> do
            let Just hole = lit `M.lookup` placements state
                prev = hole `M.lookup` holes
            case prev of
                Nothing -> liftIO $ 
                    modifyMVar_ mvar (return . assignHole thread hole lit)
                Just p  -> do
                    let c = Clause (map negateLiteral [lit, p]) ClauseLearnt
                    addClause c
                    propagate

    , propUndo = Just $ \changes -> do
        thread <- getThreadId
        state  <- liftIO (readMVar mvar)
        let cs' = mapMaybe (`M.lookup` placements state) changes
        liftIO $ 
            modifyMVar_ mvar (return . flip (foldr (unassignHole thread)) cs')
    }
