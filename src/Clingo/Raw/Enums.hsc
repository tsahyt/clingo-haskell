{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Raw.Enums
(
    ClingoError,
    pattern ErrorSuccess,
    pattern ErrorRuntime,
    pattern ErrorLogic,
    pattern ErrorBadAlloc,
    pattern ErrorUnknown,

    ClingoWarning,
    pattern WarnOpUndefined,
    pattern WarnRTError,
    pattern WarnAtomUndefined,
    pattern WarnFileIncluded,
    pattern WarnVariableUnbounded,
    pattern WarnGlobalVariable,
    pattern WarnOther,

    TruthValue,
    pattern TruthFree,
    pattern TruthFalse,
    pattern TruthTrue,

    SymbolType,
    pattern SymInfimum,
    pattern SymNumber,
    pattern SymString,
    pattern SymFunction,
    pattern SymSupremum,

    ModelType,
    pattern StableModel,
    pattern BraveConsequences,
    pattern CautiousConsequences,

    ShowFlag,
    pattern ShowCSP,
    pattern ShowShown,
    pattern ShowAtoms,
    pattern ShowTerms,
    pattern ShowExtra,
    pattern ShowAll,
    pattern ShowComplement,

    SolveResult,
    pattern ResultSatisfiable,
    pattern ResultUnsatisfiable,
    pattern ResultExhausted,
    pattern ResultInterrupted,

    SolveMode,
    pattern SolveModeAsync,
    pattern SolveModeYield,

    SolveEvent,
    pattern SolveEventModel,
    pattern SolveEventFinish,

    TheoryTermType,
    pattern TheoryTuple,
    pattern TheoryList,
    pattern TheorySet,
    pattern TheoryFunction,
    pattern TheoryNumber,
    pattern TheorySymbol,

    ClauseType,
    pattern ClauseLearnt,
    pattern ClauseStatic,
    pattern ClauseVolatile,
    pattern ClauseVolatileStatic,

    HeuristicType,
    pattern HeuristicLevel,
    pattern HeuristicSign,
    pattern HeuristicFactor,
    pattern HeuristicInit,
    pattern HeuristicTrue,
    pattern HeuristicFalse,

    ExternalType,
    pattern ExternalFree,
    pattern ExternalTrue,
    pattern ExternalFalse,
    pattern ExternalRelease,

    ConfigurationType,
    pattern ConfigValue,
    pattern ConfigArray,
    pattern ConfigMap,

    StatisticsType,
    pattern StatsEmpty,
    pattern StatsValue,
    pattern StatsArray,
    pattern StatsMap
)
where

import Data.Int
import Data.Word

#include <clingo.h>

type ClingoError = (#type clingo_error_t)

pattern ErrorSuccess = #{const clingo_error_success}
pattern ErrorRuntime = #{const clingo_error_runtime}
pattern ErrorLogic = #{const clingo_error_logic}
pattern ErrorBadAlloc = #{const clingo_error_bad_alloc}
pattern ErrorUnknown = #{const clingo_error_unknown}

type ClingoWarning = (#type clingo_warning_t)

pattern WarnOpUndefined = #{const clingo_warning_operation_undefined}
pattern WarnRTError = #{const clingo_warning_runtime_error}
pattern WarnAtomUndefined = #{const clingo_warning_atom_undefined}
pattern WarnFileIncluded = #{const clingo_warning_file_included}
pattern WarnVariableUnbounded = #{const clingo_warning_variable_unbounded}
pattern WarnGlobalVariable = #{const clingo_warning_global_variable}
pattern WarnOther = #{const clingo_warning_other}

type TruthValue = (#type clingo_truth_value_t)

pattern TruthFree = #{const clingo_truth_value_free}
pattern TruthFalse = #{const clingo_truth_value_false}
pattern TruthTrue = #{const clingo_truth_value_true}

type SymbolType = (#type clingo_symbol_type_t)

pattern SymInfimum = #{const clingo_symbol_type_infimum}
pattern SymNumber = #{const clingo_symbol_type_number}
pattern SymString = #{const clingo_symbol_type_string}
pattern SymFunction = #{const clingo_symbol_type_function}
pattern SymSupremum = #{const clingo_symbol_type_supremum}

type ModelType = (#type clingo_model_type_t)

pattern StableModel = #{const clingo_model_type_stable_model}
pattern BraveConsequences = #{const clingo_model_type_brave_consequences}
pattern CautiousConsequences = #{const clingo_model_type_cautious_consequences}

type ShowFlag = (#type clingo_show_type_bitset_t)

pattern ShowCSP = #{const clingo_show_type_csp}
pattern ShowShown = #{const clingo_show_type_shown}
pattern ShowAtoms = #{const clingo_show_type_atoms}
pattern ShowTerms = #{const clingo_show_type_terms}
pattern ShowExtra = #{const clingo_show_type_extra}
pattern ShowAll = #{const clingo_show_type_all}
pattern ShowComplement = #{const clingo_show_type_complement}

type SolveResult = (#type clingo_solve_result_bitset_t)

pattern ResultSatisfiable = #{const clingo_solve_result_satisfiable}
pattern ResultUnsatisfiable = #{const clingo_solve_result_unsatisfiable}
pattern ResultExhausted = #{const clingo_solve_result_exhausted}
pattern ResultInterrupted = #{const clingo_solve_result_interrupted}

type SolveMode = (#type clingo_solve_mode_bitset_t)

pattern SolveModeAsync = #{const clingo_solve_mode_async}
pattern SolveModeYield = #{const clingo_solve_mode_yield}

type SolveEvent = (#type clingo_solve_event_type_t)
pattern SolveEventModel = #{const clingo_solve_event_type_model}
pattern SolveEventFinish = #{const clingo_solve_event_type_finish}

type TheoryTermType = (#type clingo_theory_term_type_t)

pattern TheoryTuple = #{const clingo_theory_term_type_tuple}
pattern TheoryList = #{const clingo_theory_term_type_list}
pattern TheorySet = #{const clingo_theory_term_type_set}
pattern TheoryFunction = #{const clingo_theory_term_type_function}
pattern TheoryNumber = #{const clingo_theory_term_type_number}
pattern TheorySymbol = #{const clingo_theory_term_type_symbol}

type ClauseType = (#type clingo_clause_type_t)

pattern ClauseLearnt = #{const clingo_clause_type_learnt}
pattern ClauseStatic = #{const clingo_clause_type_static}
pattern ClauseVolatile = #{const clingo_clause_type_volatile}
pattern ClauseVolatileStatic = #{const clingo_clause_type_volatile_static}

type HeuristicType = (#type clingo_heuristic_type_t)

pattern HeuristicLevel = #{const clingo_heuristic_type_level}
pattern HeuristicSign = #{const clingo_heuristic_type_sign}
pattern HeuristicFactor = #{const clingo_heuristic_type_factor}
pattern HeuristicInit = #{const clingo_heuristic_type_init}
pattern HeuristicTrue = #{const clingo_heuristic_type_true}
pattern HeuristicFalse = #{const clingo_heuristic_type_false}

type ExternalType = (#type clingo_external_type_t)

pattern ExternalFree = #{const clingo_external_type_free}
pattern ExternalTrue = #{const clingo_external_type_true}
pattern ExternalFalse = #{const clingo_external_type_false}
pattern ExternalRelease = #{const clingo_external_type_release}

type ConfigurationType = (#type clingo_configuration_type_bitset_t)

pattern ConfigValue = #{const clingo_configuration_type_value}
pattern ConfigArray = #{const clingo_configuration_type_array}
pattern ConfigMap = #{const clingo_configuration_type_map}

type StatisticsType = (#type clingo_statistics_type_t)

pattern StatsEmpty = #{const clingo_statistics_type_empty}
pattern StatsValue = #{const clingo_statistics_type_value}
pattern StatsArray = #{const clingo_statistics_type_array}
pattern StatsMap = #{const clingo_statistics_type_map}
