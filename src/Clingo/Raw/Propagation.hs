module Clingo.Raw.Propagation
(
)
where

foreign import ccall "clingo.h clingo_propagate_init_solver_literal" propagateInitSolverLiteralFFI ::
foreign import ccall "clingo.h clingo_propagate_init_add_watch" propagateInitAddWatchFFI ::
foreign import ccall "clingo.h clingo_propagate_init_symbolic_atoms" propagateInitSymbolicAtomsFFI ::
foreign import ccall "clingo.h clingo_propagate_init_theory_atoms" propagateInitTheoryAtomsFFI ::
foreign import ccall "clingo.h clingo_propagate_init_number_of_threads" propagateInitNumberOfThreadsFFI ::
foreign import ccall "clingo.h clingo_assignment_decision_level" assignmentDecisionLevelFFI ::
foreign import ccall "clingo.h clingo_assignment_has_conflict" assignmentHasConflictFFI ::
foreign import ccall "clingo.h clingo_assignment_has_literal" assignmentHasLiteralFFI ::
foreign import ccall "clingo.h clingo_assignment_level" assignmentLevelFFI ::
foreign import ccall "clingo.h clingo_assignment_decision" assignmentDecisionFFI ::
foreign import ccall "clingo.h clingo_assignment_is_fixed" assignmentIsFixedFFI ::
foreign import ccall "clingo.h clingo_assignment_is_true" assignmentIsTrueFFI ::
foreign import ccall "clingo.h clingo_assignment_is_false" assignmentIsFalseFFI ::
foreign import ccall "clingo.h clingo_assignment_truth_value" assignmentTruthValueFFI ::
foreign import ccall "clingo.h clingo_propagate_control_thread_id" propagateControlThreadIdFFI ::
foreign import ccall "clingo.h clingo_propagate_control_assignment" propagateControlAssignmentFFI ::
foreign import ccall "clingo.h clingo_propagate_control_add_literal" propagateControlAddLiteralFFI ::
foreign import ccall "clingo.h clingo_propagate_control_add_watch" propagateControlAddWatchFFI ::
foreign import ccall "clingo.h clingo_propagate_control_has_watch" propagateControlHasWatchFFI ::
foreign import ccall "clingo.h clingo_propagate_control_remove_watch" propagateControlRemoveWatchFFI ::
foreign import ccall "clingo.h clingo_propagate_control_add_clause" propagateControlAddClauseFFI ::
foreign import ccall "clingo.h clingo_propagate_control_propagate" propagateControlPropagateFFI ::
