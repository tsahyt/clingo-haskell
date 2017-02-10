-- | High level API for writing propagators.
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module Clingo.Propagation
(
    Propagation,
    PropagationPhase (..),
    Assignment,
    Literal,
    Signed (..),
    negateLiteral,

    -- | A propagator is defined by four functions. The first is executed during
    -- the initialization phase. The remaining three are called during solving,
    -- depending on how the propagator is initialized.
    --
    -- 
    -- Initialization: This function is called once before each solving step.
    -- It is used to map relevant program literals to solver literals, add
    -- watches for solver literals, and initialize the data structures used
    -- during propagation.  This is the last point to access symbolic and theory
    -- atoms.  Once the search has started, they are no longer accessible.
    --
    -- Propagation: Can be used to add conflicts in order to propagate solver
    -- literals.
    --
    -- Undo: Is called on backjumping and can be used to synchronize internal
    -- data structures of the propagator.
    --
    -- Check: Can be used to check whether an assignment is valid.
    Propagator (..),
    emptyPropagator,
    propagatorToIO,

    addWatch,

    -- * Initialization
    countThreads,
    solverLiteral,
    propSymbolicAtoms,
    propTheoryAtoms,

    -- * Actions During Solving
    P.Clause (..),
    P.ClauseType (..),
    addClause,
    propagate,
    hasWatch,
    removeWatch,
    getThreadId,
    newLiteral,
    assignment,

    -- * Assignment
    decisionLevel,
    hasConflict,
    hasLiteral,
    levelOf,
    decision,
    isFixed,
    truthValue,

    -- * Truth Values
    TruthValue,
    pattern TruthFree,
    pattern TruthFalse,
    pattern TruthTrue,
    negateTruth
)
where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Numeric.Natural

import Clingo.Internal.Types
import Clingo.Internal.Symbol
import Clingo.Symbol
import qualified Clingo.Internal.Propagation as P
import Clingo.Internal.Propagation (Assignment, Clause)

-- | Propagators can be in one of two phases, initialization and solving.
data PropagationPhase = Init | Solving

type family PhaseHandle (k :: PropagationPhase) s where
    PhaseHandle 'Init s = PropagateInit s
    PhaseHandle 'Solving s = PropagateCtrl s

-- | A concrete monad for propagators, which observes the invariants
-- stipulated by clingo.
newtype Propagation (phase :: PropagationPhase) s a
    = Propagation { runPropagator :: MaybeT 
                                         (ReaderT (PhaseHandle phase s) IO) a }
        deriving ( Functor, Monad, Applicative
                 , MonadIO, MonadThrow )

getIOAction :: Propagation phase s () -> PhaseHandle phase s -> IO ()
getIOAction = (void .) . runReaderT . runMaybeT . runPropagator

instance MonadReader (PropagateInit s) (Propagation 'Init s) where
    ask = Propagation ask
    local f (Propagation x) = Propagation (local f x)
    reader = Propagation . reader

instance MonadReader (PropagateCtrl s) (Propagation 'Solving s) where
    ask = Propagation ask
    local f (Propagation x) = Propagation (local f x)
    reader = Propagation . reader

instance MonadSymbol (Propagation phase) where
    createSignature = createSignature'
    createNumber = createNumber'
    createSupremum = createSupremum'
    createInfimum = createInfimum'
    createString = createString'
    createFunction = createFunction'

handleStop :: P.PropagationStop -> Propagation phase s ()
handleStop P.Continue = return ()
handleStop P.Stop = Propagation empty

-- Propagator and wrapping
-- -----------------------

-- | A propagator is defined by four functions. No function is mandatory.
data Propagator s = Propagator
    { propInit      :: Maybe (Propagation 'Init s ())
    , propPropagate :: Maybe ([Literal s] -> Propagation 'Solving s ())
    , propUndo      :: Maybe ([Literal s] -> Propagation 'Solving s ())
    , propCheck     :: Maybe (Propagation 'Solving s ())
    }
    
-- | The empty propagator for convenience.
emptyPropagator :: Propagator s
emptyPropagator = Propagator Nothing Nothing Nothing Nothing

propagatorToIO :: Propagator s -> IOPropagator s
propagatorToIO prop = IOPropagator
    { propagatorInit = getIOAction <$> propInit prop
    , propagatorPropagate = runLitSolv <$> propPropagate prop
    , propagatorUndo = runLitSolv <$> propUndo prop
    , propagatorCheck = getIOAction <$> propCheck prop
    }
    where runLitSolv = flip . (getIOAction .)

-- Operations that are always available
-- ------------------------------------
class CanAddWatch (phase :: PropagationPhase) where
    mAddWatch :: Literal s -> Propagation phase s ()

instance CanAddWatch 'Init where
    mAddWatch l = ask >>= flip P.initAddWatch l

instance CanAddWatch 'Solving where
    mAddWatch l = ask >>= flip P.addWatch l

-- | Watches can be added in any phase of the propagation. The propagate and
-- undo functions will only be called on changes to watched literals!
addWatch :: CanAddWatch phase => Literal s -> Propagation phase s ()
addWatch = mAddWatch

-- Actions during initialization
-- -----------------------------

-- | Obtain the number of solver threads.
countThreads :: Propagation 'Init s Integer
countThreads = ask >>= P.countThreads

-- | Convert an 'AspifLiteral' to a solver 'Literal'.
solverLiteral :: AspifLiteral s -> Propagation 'Init s (Literal s)
solverLiteral l = ask >>= flip P.solverLiteral l

-- | Obtain a handle to the symbolic atoms, see 'Clingo.Inspection.Symbolic'
propSymbolicAtoms :: Propagation 'Init s (SymbolicAtoms s)
propSymbolicAtoms = ask >>= P.symbolicAtoms

-- | Obtain a handle to the theory atoms, see 'Clingo.Inspection.Theory'
propTheoryAtoms :: Propagation 'Init s (TheoryAtoms s)
propTheoryAtoms = ask >>= P.theoryAtoms

-- Actions during Solving
-- ----------------------

-- | Check whether a 'Literal' is watched.
hasWatch :: Literal s -> Propagation 'Solving s Bool
hasWatch l = ask >>= flip P.hasWatch l

-- | Stop watching a literal.
removeWatch :: Literal s -> Propagation 'Solving s ()
removeWatch l = ask >>= flip P.removeWatch l

-- | Get the thread id of the calling solver thread.
getThreadId :: Propagation 'Solving s Integer
getThreadId = ask >>= P.getThreadId

-- | Introduce a new literal to the solver.
newLiteral :: Propagation 'Solving s (Literal s)
newLiteral = ask >>= P.addLiteral

-- | Add a clause. This call might result in termination of the propagation
-- function, when backjumping is necessary.
addClause :: Clause s -> Propagation 'Solving s ()
addClause c = ask >>= flip P.addClause c >>= handleStop

-- | Propagate implied literals from added clauses. Might result in backjumping
-- and hence termination of the propagation.
propagate :: Propagation 'Solving s ()
propagate = ask >>= P.propagate >>= handleStop

-- | Obtain the current (partial) assignment.
assignment :: Propagation 'Solving s (Assignment s)
assignment = ask >>= P.assignment

-- | Get the current decision level.
decisionLevel :: Assignment s -> Propagation 'Solving s Natural
decisionLevel = P.decisionLevel

-- | Determine whether assignment has a conflict.
hasConflict :: Assignment s -> Propagation 'Solving s Bool
hasConflict = P.hasConflict

-- | Determine whether a literal is part of an assignment.
hasLiteral :: Assignment s -> Literal s -> Propagation 'Solving s Bool
hasLiteral = P.hasLiteral

-- | Find the decision level of a given literal in an assignment.
levelOf :: Assignment s -> Literal s -> Propagation 'Solving s Natural
levelOf = P.levelOf

-- | Determine the decision literal given a decision level.
decision :: Assignment s -> Natural -> Propagation 'Solving s (Literal s)
decision = P.decision

-- | Check if a literal has a fixed truth value.
isFixed :: Assignment s -> Literal s -> Propagation 'Solving s Bool
isFixed = P.isFixed

-- | Obtain the truth value of a literal
truthValue :: Assignment s -> Literal s -> Propagation 'Solving s TruthValue
truthValue = P.truthValue
