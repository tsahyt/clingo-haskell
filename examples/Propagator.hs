{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Clingo.Control
import Clingo.Symbol
import Clingo.Solving
import Clingo.Model
import Clingo.Propagation
import Clingo.Inspection.Symbolic

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
    deriving (Show)

assignHole :: Integer -> Hole -> Literal s -> PigeonData s -> PigeonData s
assignHole tid hole lit pd = pd 
    { assignments = 
        I.adjust (M.insert hole lit) (fromIntegral tid) (assignments pd) }

unassignHole :: Integer -> Literal s -> Hole -> PigeonData s -> PigeonData s
unassignHole tid lit hole pd = pd
    { assignments = I.adjust go (fromIntegral tid) (assignments pd) }
    where go m | Just l <- M.lookup hole m
               , l == lit  = M.delete hole m
               | otherwise = m

forceMVar :: MonadIO m => MVar a -> a -> m ()
forceMVar mvar x = liftIO $ do
    _ <- tryTakeMVar mvar
    putMVar mvar x

onModel :: Model s -> IOSym s Continue
onModel m = do
    syms <- map prettySymbol
        <$> modelSymbols m (selectNone { selectShown = True }) 
    liftIO (putStr "Model: " >> print syms)
    return Continue
    
main :: IO ()
main = withDefaultClingo $ do
    addProgram "pigeon" ["h","p"] 
        "1 { place(P,H) : H = 1..h } 1 :- P = 1..p."

    holes <- createNumber 8
    pigeons <- createNumber 9
    ground [Part "pigeon" [holes, pigeons]] Nothing

    propState <- liftIO newEmptyMVar
    registerPropagator False (pigeonator propState)

    solveRet <- withSolver [] getResult
    liftIO (print solveRet)

-- TODO: Propagator with symbol inspection methods?
pigeonator :: MVar (PigeonData s) -> Propagator s
pigeonator mvar = emptyPropagator
    { propInit = Just $ do
        -- obtain place/2 atoms
        placeSig <- createSignature "place" 2 True
        watches <- propSymbolicAtoms 
               >>= \sa -> fromSymbolicAtomsSig sa placeSig id

        -- watch and create initial placements
        mapM_ ((addWatch =<<) . solverLiteral . literal) watches
        ps <- forM watches $ \atom -> do
                  let hole = do 
                             arg <- symbolGetArg (symbol atom) 1
                             Hole . fromIntegral <$> symbolNumber arg
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

        -- apply and check assignments done by solver
        forM_ changes $ \lit -> do
            state  <- liftIO (readMVar mvar)
            let Just holes = fromIntegral thread `I.lookup` assignments state
                Just hole = lit `M.lookup` placements state
                prev = hole `M.lookup` holes
            case prev of
                Nothing -> liftIO $
                    modifyMVar_ mvar (return . assignHole thread hole lit)
                Just p -> do
                    let c = Clause (map negateLiteral [lit, p]) ClauseLearnt
                    addClause c
                    propagate

    , propUndo = Just $ \changes -> do
        thread <- getThreadId
        state  <- liftIO (readMVar mvar)

        let cs' = mapMaybe 
                      (\l -> (,) <$> pure l <*> M.lookup l (placements state))
                      changes
        liftIO $ 
            modifyMVar_ mvar $
            return . flip (foldr (uncurry (unassignHole thread))) cs'
    }
