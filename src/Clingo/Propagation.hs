module Clingo.Propagation
(
    -- * Assignment
    decisionLevel,
    hasConflict,
    hasLiteral,
    levelOf,
    decision,
    isFixed,
    truthValue
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
