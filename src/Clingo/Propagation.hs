{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Clingo.Propagation
(
    Propagation,
    Assignment,
    addWatch,

    -- * Initialization
    countThreads,
    solverLiteral,
    symbolicAtoms,
    theoryAtoms,

    -- * Actions During Solving
    hasWatch,
    removeWatch,
    getThreadId,
    newLiteral,
    assignment
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader

import Numeric.Natural

import Clingo.Internal.Types
import qualified Clingo.Internal.Propagation as P
import Clingo.Internal.Propagation (Assignment)

data PropagationPhase = Init | Solving

type family PhaseHandle (k :: PropagationPhase) s where
    PhaseHandle 'Init s = PropagateInit s
    PhaseHandle 'Solving s = PropagateCtrl s

newtype Propagation (phase :: PropagationPhase) s a
    = Propagation { runPropagator :: ReaderT (PhaseHandle phase s) IO a }
        deriving ( Functor, Monad, Applicative
                 , MonadIO, MonadThrow )

instance MonadReader (PropagateInit s) (Propagation 'Init s) where
    ask = Propagation ask
    local f (Propagation x) = Propagation (local f x)
    reader = Propagation . reader

instance MonadReader (PropagateCtrl s) (Propagation 'Solving s) where
    ask = Propagation ask
    local f (Propagation x) = Propagation (local f x)
    reader = Propagation . reader

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

solverLiteral :: Literal s -> Propagation 'Init s (Literal s)
solverLiteral l = ask >>= flip P.solverLiteral l

symbolicAtoms :: Propagation 'Init s (SymbolicAtoms s)
symbolicAtoms = ask >>= P.symbolicAtoms

theoryAtoms :: Propagation 'Init s (TheoryAtoms s)
theoryAtoms = ask >>= P.theoryAtoms

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

-- TODO: propagate method
-- TODO: addClause

assignment :: Propagation 'Solving s (Assignment s)
assignment = ask >>= P.assignment

-- Operations on Assignment
-- ------------------------
--
-- TODO
