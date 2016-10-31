module Clingo.Propagation
(
    Assignment,
    PropagateInit,
    PropagateCtrl,
    Propagator (..),

    -- * Assignment
    decisionLevel,
    hasConflict,
    hasLiteral,
    levelOf,
    decision,
    isFixed,
    truthValue,

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
import Foreign.C

import qualified Clingo.Raw as Raw
import Clingo.Internal.Utils
import Clingo.Internal.Types

decisionLevel :: (MonadIO m) => Assignment s -> m Natural
decisionLevel (Assignment a) = fromIntegral <$> Raw.assignmentDecisionLevel a

hasConflict :: (MonadIO m) => Assignment s -> m Bool
hasConflict (Assignment a) = toBool <$> Raw.assignmentHasConflict a

hasLiteral :: (MonadIO m) => Assignment s -> Literal s -> m Bool
hasLiteral (Assignment a) lit = toBool <$> Raw.assignmentHasLiteral a 
                                           (rawLiteral lit)

levelOf :: (MonadIO m, MonadThrow m) => Assignment s -> Literal s -> m Natural
levelOf (Assignment a) lit = fromIntegral <$> marshall1 go
    where go = Raw.assignmentLevel a (rawLiteral lit)

decision :: (MonadIO m, MonadThrow m) 
         => Assignment s -> Natural -> m (Literal s)
decision (Assignment a) level = Literal <$> marshall1 go
    where go = Raw.assignmentDecision a (fromIntegral level)

isFixed :: (MonadIO m, MonadThrow m) => Assignment s -> Literal s -> m Bool
isFixed (Assignment a) lit = toBool <$> marshall1 go
    where go = Raw.assignmentIsFixed a (rawLiteral lit)

truthValue :: (MonadIO m, MonadThrow m) 
           => Assignment s -> Literal s -> m TruthValue
truthValue (Assignment a) lit = TruthValue <$> marshall1 go
    where go = Raw.assignmentTruthValue a (rawLiteral lit)

data Clause s = Clause [Literal s] ClauseType

data PropagationStop = Continue | Stop
    deriving (Eq, Show, Ord, Read, Enum, Bounded)

pstopFromBool :: Bool -> PropagationStop
pstopFromBool True = Continue
pstopFromBool False = Stop

addClause :: (MonadIO m, MonadThrow m)
          => PropagateCtrl s -> Clause s -> m PropagationStop
addClause (PropagateCtrl c) (Clause ls t) = pstopFromBool . toBool <$> 
    marshall1 go
    where go x = withArrayLen (map rawLiteral ls) $ \len arr ->
                     Raw.propagateControlAddClause c arr (fromIntegral len)
                                                   (rawClauseType t) x

addLiteral :: (MonadIO m, MonadThrow m)
           => PropagateCtrl s -> m (Literal s)
addLiteral (PropagateCtrl c) = Literal <$> marshall1 
    (Raw.propagateControlAddLiteral c)

getThreadId :: MonadIO m => PropagateCtrl s -> m Integer
getThreadId (PropagateCtrl c) = fromIntegral <$> Raw.propagateControlThreadId c

addWatch :: (MonadIO m, MonadThrow m) => PropagateCtrl s -> Literal s -> m ()
addWatch (PropagateCtrl c) lit = 
    marshall0 (Raw.propagateControlAddWatch c (rawLiteral lit))

hasWatch :: (MonadIO m) => PropagateCtrl s -> Literal s -> m Bool
hasWatch (PropagateCtrl c) lit = 
    toBool <$> Raw.propagateControlHasWatch c (rawLiteral lit)

removeWatch :: (MonadIO m) => PropagateCtrl s -> Literal s -> m ()
removeWatch (PropagateCtrl c) lit =
    Raw.propagateControlRemoveWatch c (rawLiteral lit)

propagate :: (MonadIO m, MonadThrow m) => PropagateCtrl s -> m PropagationStop
propagate (PropagateCtrl c) = pstopFromBool . toBool <$>
    marshall1 (Raw.propagateControlPropagate c)

assignment :: (MonadIO m) => PropagateCtrl s -> m (Assignment s)
assignment (PropagateCtrl c) = Assignment <$> Raw.propagateControlAssignment c

initAddWatch :: (MonadIO m, MonadThrow m) 
             => PropagateInit s -> Literal s -> m ()
initAddWatch (PropagateInit c) lit = 
    marshall0 (Raw.propagateInitAddWatch c (rawLiteral lit))

countThreads :: MonadIO m => PropagateInit s -> m Integer
countThreads (PropagateInit c) = fromIntegral <$> 
    Raw.propagateInitNumberOfThreads c

solverLiteral :: (MonadIO m, MonadThrow m) 
              => PropagateInit s -> Literal s -> m (Literal s)
solverLiteral (PropagateInit c) lit = Literal <$> marshall1
    (Raw.propagateInitSolverLiteral c (rawLiteral lit))

symbolicAtoms :: (MonadIO m, MonadThrow m)
              => PropagateInit s -> m (SymbolicAtoms s)
symbolicAtoms (PropagateInit c) = SymbolicAtoms <$> marshall1
    (Raw.propagateInitSymbolicAtoms c)

theoryAtoms :: (MonadIO m, MonadThrow m)
              => PropagateInit s -> m (TheoryAtoms s)
theoryAtoms (PropagateInit c) = TheoryAtoms <$> marshall1
    (Raw.propagateInitTheoryAtoms c)
