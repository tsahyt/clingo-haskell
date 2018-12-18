{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Internal.Propagation
(
    Assignment,
    Literal,

    -- * Propagation Handles and Types
    PropagateInit,
    PropagateCtrl,
    IOPropagator (..),
    PropagationStop (..),

    -- * Truth values
    TruthValue,
    pattern TruthFree,
    pattern TruthFalse,
    pattern TruthTrue,

    -- * Assignment
    decisionLevel,
    hasConflict,
    hasLiteral,
    levelOf,
    decision,
    isFixed,
    truthValue,

    -- * Clauses
    Clause (..),
    ClauseType (..),

    -- * Propagation
    assignment,
    addClause,
    addLiteral,
    addWatch,
    hasWatch,
    removeWatch,
    propagate,
    getThreadId,

    -- * Initialization
    initAddWatch,
    countThreads,
    solverLiteral,
    symbolicAtoms,
    theoryAtoms
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch

import Numeric.Natural

import Foreign

import qualified Clingo.Raw as Raw
import Clingo.Internal.Utils
import Clingo.Internal.Types

newtype Assignment s = Assignment Raw.Assignment

-- | Get the current decision level.
decisionLevel :: (MonadIO m) => Assignment s -> m Natural
decisionLevel (Assignment a) = fromIntegral <$> Raw.assignmentDecisionLevel a

-- | Determine whether assignment has a conflict.
hasConflict :: (MonadIO m) => Assignment s -> m Bool
hasConflict (Assignment a) = toBool <$> Raw.assignmentHasConflict a

-- | Determine whether a literal is part of an assignment.
hasLiteral :: (MonadIO m) => Assignment s -> Literal s -> m Bool
hasLiteral (Assignment a) lit = toBool <$> Raw.assignmentHasLiteral a 
                                           (rawLiteral lit)

-- | Find the decision level of a given literal in an assignment.
levelOf :: (MonadIO m, MonadThrow m) => Assignment s -> Literal s -> m Natural
levelOf (Assignment a) lit = fromIntegral <$> marshal1 go
    where go = Raw.assignmentLevel a (rawLiteral lit)

-- | Determine the decision literal given a decision level.
decision :: (MonadIO m, MonadThrow m) 
         => Assignment s -> Natural -> m (Literal s)
decision (Assignment a) level = Literal <$> marshal1 go
    where go = Raw.assignmentDecision a (fromIntegral level)

-- | Check if a literal has a fixed truth value.
isFixed :: (MonadIO m, MonadThrow m) => Assignment s -> Literal s -> m Bool
isFixed (Assignment a) lit = toBool <$> marshal1 go
    where go = Raw.assignmentIsFixed a (rawLiteral lit)

-- | Obtain the truth value of a literal
truthValue :: (MonadIO m, MonadThrow m) 
           => Assignment s -> Literal s -> m TruthValue
truthValue (Assignment a) lit = TruthValue <$> marshal1 go
    where go = Raw.assignmentTruthValue a (rawLiteral lit)

data Clause s = Clause [Literal s] ClauseType
    deriving (Show)

data ClauseType = ClauseLearnt 
                | ClauseVolatile Bool       -- ^ Bool determines static
                | ClauseStatic
    deriving (Eq, Show, Ord)

rawClauseType :: ClauseType -> Raw.ClauseType
rawClauseType c = case c of
    ClauseLearnt -> Raw.ClauseLearnt
    ClauseStatic -> Raw.ClauseStatic
    ClauseVolatile static -> if static then Raw.ClauseVolatileStatic
                                       else Raw.ClauseVolatile

data PropagationStop = Continue | Stop
    deriving (Eq, Show, Ord, Read, Enum, Bounded)

pstopFromBool :: Bool -> PropagationStop
pstopFromBool True = Continue
pstopFromBool False = Stop

addClause :: (MonadIO m, MonadThrow m)
          => PropagateCtrl s -> Clause s -> m PropagationStop
addClause (PropagateCtrl c) (Clause ls t) = pstopFromBool . toBool <$> 
    marshal1 go
    where go x = withArrayLen (map rawLiteral ls) $ \len arr ->
                     Raw.propagateControlAddClause c arr (fromIntegral len)
                                                   (rawClauseType t) x

addLiteral :: (MonadIO m, MonadThrow m)
           => PropagateCtrl s -> m (Literal s)
addLiteral (PropagateCtrl c) = Literal <$> marshal1 
    (Raw.propagateControlAddLiteral c)

getThreadId :: MonadIO m => PropagateCtrl s -> m Integer
getThreadId (PropagateCtrl c) = fromIntegral <$> Raw.propagateControlThreadId c

addWatch :: (MonadIO m, MonadThrow m) => PropagateCtrl s -> Literal s -> m ()
addWatch (PropagateCtrl c) lit = 
    marshal0 (Raw.propagateControlAddWatch c (rawLiteral lit))

hasWatch :: (MonadIO m) => PropagateCtrl s -> Literal s -> m Bool
hasWatch (PropagateCtrl c) lit = 
    toBool <$> Raw.propagateControlHasWatch c (rawLiteral lit)

removeWatch :: (MonadIO m) => PropagateCtrl s -> Literal s -> m ()
removeWatch (PropagateCtrl c) lit =
    Raw.propagateControlRemoveWatch c (rawLiteral lit)

propagate :: (MonadIO m, MonadThrow m) => PropagateCtrl s -> m PropagationStop
propagate (PropagateCtrl c) = pstopFromBool . toBool <$>
    marshal1 (Raw.propagateControlPropagate c)

assignment :: (MonadIO m) => PropagateCtrl s -> m (Assignment s)
assignment (PropagateCtrl c) = Assignment <$> Raw.propagateControlAssignment c

initAddWatch :: (MonadIO m, MonadThrow m) 
             => PropagateInit s -> Literal s -> m ()
initAddWatch (PropagateInit c) lit = 
    marshal0 (Raw.propagateInitAddWatch c (rawLiteral lit))

countThreads :: MonadIO m => PropagateInit s -> m Integer
countThreads (PropagateInit c) = fromIntegral <$> 
    Raw.propagateInitNumberOfThreads c

solverLiteral :: (MonadIO m, MonadThrow m) 
              => PropagateInit s -> AspifLiteral s -> m (Literal s)
solverLiteral (PropagateInit c) lit = Literal <$> marshal1
    (Raw.propagateInitSolverLiteral c (rawAspifLiteral lit))

symbolicAtoms :: (MonadIO m, MonadThrow m)
              => PropagateInit s -> m (SymbolicAtoms s)
symbolicAtoms (PropagateInit c) = SymbolicAtoms <$> marshal1
    (Raw.propagateInitSymbolicAtoms c)

theoryAtoms :: (MonadIO m, MonadThrow m)
              => PropagateInit s -> m (TheoryAtoms s)
theoryAtoms (PropagateInit c) = TheoryAtoms <$> marshal1
    (Raw.propagateInitTheoryAtoms c)
