module Clingo.Raw.ProgramBuilding
(
)
where

foreign import ccall "clingo.h backend_rule" backendRuleFFI ::
foreign import ccall "clingo.h backend_weight_rule" backendWeightRuleFFI ::
foreign import ccall "clingo.h backend_minimize" backendMinimizeFFI ::
foreign import ccall "clingo.h backend_project" backendProjectFFI ::
foreign import ccall "clingo.h backend_external" backendExternalFFI ::
foreign import ccall "clingo.h backend_assume" backendAssumeFFI ::
foreign import ccall "clingo.h backend_heuristic" backendHeuristicFFI ::
foreign import ccall "clingo.h backend_acyc_edge" backendAcycEdgeFFI ::
foreign import ccall "clingo.h backend_add_atom" backendAddAtomFFI ::
foreign import ccall "clingo.h backend_builder_begin" backendBuilderBeginFFI ::
foreign import ccall "clingo.h backend_builder_add" backendBuilderAddFFI ::
foreign import ccall "clingo.h backend_builder_end" backendBuilderEndFFI ::
