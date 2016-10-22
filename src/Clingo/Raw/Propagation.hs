{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Propagation
(
    -- * Initialization
    propagateInitSolverLiteral,
    propagateInitAddWatch,
    propagateInitSymbolicAtoms,
    propagateInitTheoryAtoms,
    propagateInitNumberOfThreads,

    -- * Assignment
    assignmentDecisionLevel,
    assignmentHasConflict,
    assignmentHasLiteral,
    assignmentLevel,
    assignmentDecision,
    assignmentIsFixed,
    assignmentIsTrue,
    assignmentIsFalse,
    assignmentTruthValue,

    -- * Propagation
    propagateControlThreadId,
    propagateControlAssignment,
    propagateControlAddLiteral,
    propagateControlAddWatch,
    propagateControlHasWatch,
    propagateControlRemoveWatch,
    propagateControlAddClause,
    propagateControlPropagate
)
where

import Control.Monad.IO.Class
import Clingo.Raw.Enums
import Clingo.Raw.Types
import Foreign
import Foreign.C

foreign import ccall "clingo.h clingo_propagate_init_solver_literal" propagateInitSolverLiteralFFI ::
    Ptr PropagateInit -> Literal -> Ptr Literal -> IO CBool
foreign import ccall "clingo.h clingo_propagate_init_add_watch" propagateInitAddWatchFFI ::
    Ptr PropagateInit -> Literal -> IO CBool
foreign import ccall "clingo.h clingo_propagate_init_symbolic_atoms" propagateInitSymbolicAtomsFFI ::
    Ptr PropagateInit -> Ptr (Ptr SymbolicAtoms) -> IO CBool
foreign import ccall "clingo.h clingo_propagate_init_theory_atoms" propagateInitTheoryAtomsFFI ::
    Ptr PropagateInit -> Ptr (Ptr TheoryAtoms) -> IO CBool
foreign import ccall "clingo.h clingo_propagate_init_number_of_threads" propagateInitNumberOfThreadsFFI ::
    Ptr PropagateInit -> IO CInt
foreign import ccall "clingo.h clingo_assignment_decision_level" assignmentDecisionLevelFFI ::
    Ptr Assignment -> IO Word32
foreign import ccall "clingo.h clingo_assignment_has_conflict" assignmentHasConflictFFI ::
    Ptr Assignment -> IO CBool
foreign import ccall "clingo.h clingo_assignment_has_literal" assignmentHasLiteralFFI ::
    Ptr Assignment -> Literal -> IO CBool
foreign import ccall "clingo.h clingo_assignment_level" assignmentLevelFFI ::
    Ptr Assignment -> Literal -> Ptr Word32 -> IO CBool
foreign import ccall "clingo.h clingo_assignment_decision" assignmentDecisionFFI ::
    Ptr Assignment -> Word32 -> Ptr Literal -> IO CBool
foreign import ccall "clingo.h clingo_assignment_is_fixed" assignmentIsFixedFFI ::
    Ptr Assignment -> Literal -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_assignment_is_true" assignmentIsTrueFFI ::
    Ptr Assignment -> Literal -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_assignment_is_false" assignmentIsFalseFFI ::
    Ptr Assignment -> Literal -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_assignment_truth_value" assignmentTruthValueFFI ::
    Ptr Assignment -> Literal -> Ptr TruthValue -> IO CBool
foreign import ccall "clingo.h clingo_propagate_control_thread_id" propagateControlThreadIdFFI ::
    Ptr PropagateControl -> IO Identifier
foreign import ccall "clingo.h clingo_propagate_control_assignment" propagateControlAssignmentFFI ::
    Ptr PropagateControl -> IO (Ptr Assignment)
foreign import ccall "clingo.h clingo_propagate_control_add_literal" propagateControlAddLiteralFFI ::
    Ptr PropagateControl -> Ptr Literal -> IO CBool
foreign import ccall "clingo.h clingo_propagate_control_add_watch" propagateControlAddWatchFFI ::
    Ptr PropagateControl -> Literal -> IO CBool
foreign import ccall "clingo.h clingo_propagate_control_has_watch" propagateControlHasWatchFFI ::
    Ptr PropagateControl -> Literal -> IO CBool
foreign import ccall "clingo.h clingo_propagate_control_remove_watch" propagateControlRemoveWatchFFI ::
    Ptr PropagateControl -> Literal -> IO ()
foreign import ccall "clingo.h clingo_propagate_control_add_clause" propagateControlAddClauseFFI ::
    Ptr PropagateControl -> Ptr Literal -> CSize -> ClauseType -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_propagate_control_propagate" propagateControlPropagateFFI ::
    Ptr PropagateControl -> Ptr CBool -> IO CBool

propagateInitSolverLiteral :: MonadIO m => Ptr PropagateInit -> Literal -> Ptr Literal -> m CBool
propagateInitSolverLiteral a b c = liftIO $ propagateInitSolverLiteralFFI a b c

propagateInitAddWatch :: MonadIO m => Ptr PropagateInit -> Literal -> m CBool
propagateInitAddWatch a b = liftIO $ propagateInitAddWatchFFI a b

propagateInitSymbolicAtoms :: MonadIO m => Ptr PropagateInit -> Ptr (Ptr SymbolicAtoms) -> m CBool
propagateInitSymbolicAtoms a b = liftIO $ propagateInitSymbolicAtomsFFI a b

propagateInitTheoryAtoms :: MonadIO m => Ptr PropagateInit -> Ptr (Ptr TheoryAtoms) -> m CBool
propagateInitTheoryAtoms a b = liftIO $ propagateInitTheoryAtomsFFI a b

propagateInitNumberOfThreads :: MonadIO m => Ptr PropagateInit -> m CInt
propagateInitNumberOfThreads a = liftIO $ propagateInitNumberOfThreadsFFI a

assignmentDecisionLevel :: MonadIO m => Ptr Assignment -> m Word32
assignmentDecisionLevel a = liftIO $ assignmentDecisionLevelFFI a

assignmentHasConflict :: MonadIO m => Ptr Assignment -> m CBool
assignmentHasConflict a = liftIO $ assignmentHasConflictFFI a

assignmentHasLiteral :: MonadIO m => Ptr Assignment -> Literal -> m CBool
assignmentHasLiteral a b = liftIO $ assignmentHasLiteralFFI a b

assignmentLevel :: MonadIO m => Ptr Assignment -> Literal -> Ptr Word32 -> m CBool
assignmentLevel a b c = liftIO $ assignmentLevelFFI a b c

assignmentDecision :: MonadIO m => Ptr Assignment -> Word32 -> Ptr Literal -> m CBool
assignmentDecision a b c = liftIO $ assignmentDecisionFFI a b c

assignmentIsFixed :: MonadIO m => Ptr Assignment -> Literal -> Ptr CBool -> m CBool
assignmentIsFixed a b c = liftIO $ assignmentIsFixedFFI a b c

assignmentIsTrue :: MonadIO m => Ptr Assignment -> Literal -> Ptr CBool -> m CBool
assignmentIsTrue a b c = liftIO $ assignmentIsTrueFFI a b c

assignmentIsFalse :: MonadIO m => Ptr Assignment -> Literal -> Ptr CBool -> m CBool
assignmentIsFalse a b c = liftIO $ assignmentIsFalseFFI a b c

assignmentTruthValue :: MonadIO m => Ptr Assignment -> Literal -> Ptr TruthValue -> m CBool
assignmentTruthValue a b c = liftIO $ assignmentTruthValueFFI a b c

propagateControlThreadId :: MonadIO m => Ptr PropagateControl -> m Identifier
propagateControlThreadId a = liftIO $ propagateControlThreadIdFFI a

propagateControlAssignment :: MonadIO m => Ptr PropagateControl -> m (Ptr Assignment)
propagateControlAssignment a = liftIO $ propagateControlAssignmentFFI a

propagateControlAddLiteral :: MonadIO m => Ptr PropagateControl -> Ptr Literal -> m CBool
propagateControlAddLiteral a b = liftIO $ propagateControlAddLiteralFFI a b

propagateControlAddWatch :: MonadIO m => Ptr PropagateControl -> Literal -> m CBool
propagateControlAddWatch a b = liftIO $ propagateControlAddWatchFFI a b

propagateControlHasWatch :: MonadIO m => Ptr PropagateControl -> Literal -> m CBool
propagateControlHasWatch a b = liftIO $ propagateControlHasWatchFFI a b

propagateControlRemoveWatch :: MonadIO m => Ptr PropagateControl -> Literal -> m ()
propagateControlRemoveWatch a b = liftIO $ propagateControlRemoveWatchFFI a b

propagateControlAddClause :: MonadIO m => Ptr PropagateControl -> Ptr Literal -> CSize -> ClauseType -> Ptr CBool -> m CBool
propagateControlAddClause a b c d e = liftIO $ propagateControlAddClauseFFI a b c d e

propagateControlPropagate :: MonadIO m => Ptr PropagateControl -> Ptr CBool -> m CBool
propagateControlPropagate a b = liftIO $ propagateControlPropagateFFI a b
