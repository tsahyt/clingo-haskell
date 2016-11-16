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
    negateLiteral,
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
    P.ClauseType,
    pattern P.ClauseLearnt,
    pattern P.ClauseStatic,
    pattern P.ClauseVolatile,
    pattern P.ClauseVolatileStatic,
    addClause,
    propagate,
    hasWatch,
    removeWatch,
    getThreadId,
    newLiteral,
    assignment,

    -- * Assignment
    P.decisionLevel,
    P.hasConflict,
    P.hasLiteral,
    P.levelOf,
    P.decision,
    P.isFixed,
    P.truthValue
)
where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Clingo.Internal.Types
import qualified Clingo.Internal.Propagation as P
import Clingo.Internal.Propagation (Assignment, Clause)

data PropagationPhase = Init | Solving

type family PhaseHandle (k :: PropagationPhase) s where
    PhaseHandle 'Init s = PropagateInit s
    PhaseHandle 'Solving s = PropagateCtrl s

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

handleStop :: P.PropagationStop -> Propagation phase s ()
handleStop P.Continue = return ()
handleStop P.Stop = Propagation empty

-- Propagator and wrapping
-- -----------------------

data Propagator s = Propagator
    { propInit      :: Maybe (Propagation 'Init s ())
    , propPropagate :: Maybe ([Literal s] -> Propagation 'Solving s ())
    , propUndo      :: Maybe ([Literal s] -> Propagation 'Solving s ())
    , propCheck     :: Maybe (Propagation 'Solving s ())
    }
    
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

addWatch :: CanAddWatch phase => Literal s -> Propagation phase s ()
addWatch = mAddWatch

-- Actions during initialization
-- -----------------------------

countThreads :: Propagation 'Init s Integer
countThreads = ask >>= P.countThreads

solverLiteral :: AspifLiteral s -> Propagation 'Init s (Literal s)
solverLiteral l = ask >>= flip P.solverLiteral l

propSymbolicAtoms :: Propagation 'Init s (SymbolicAtoms s)
propSymbolicAtoms = ask >>= P.symbolicAtoms

propTheoryAtoms :: Propagation 'Init s (TheoryAtoms s)
propTheoryAtoms = ask >>= P.theoryAtoms

-- Actions during Solving
-- ----------------------

hasWatch :: Literal s -> Propagation 'Solving s Bool
hasWatch l = ask >>= flip P.hasWatch l

removeWatch :: Literal s -> Propagation 'Solving s ()
removeWatch l = ask >>= flip P.removeWatch l

getThreadId :: Propagation 'Solving s Integer
getThreadId = ask >>= P.getThreadId

newLiteral :: Propagation 'Solving s (Literal s)
newLiteral = ask >>= P.addLiteral

addClause :: Clause s -> Propagation 'Solving s ()
addClause c = ask >>= flip P.addClause c >>= handleStop

propagate :: Propagation 'Solving s ()
propagate = ask >>= P.propagate >>= handleStop

assignment :: Propagation 'Solving s (Assignment s)
assignment = ask >>= P.assignment
