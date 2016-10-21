{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Raw.Enums
(
    combineFlags,

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

    TheoryTerm,
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

import Data.Flags
import Data.Int
import Data.Word

#include <clingo.h>

combineFlags :: Flags a => [a] -> a
combineFlags = foldl (.+.) noFlags

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

newtype SymbolType = MkSymbolType (#type clingo_symbol_type_t)
    deriving (Show, Eq)

pattern SymInfimum = MkSymbolType #{const clingo_symbol_type_infimum}
pattern SymNumber = MkSymbolType #{const clingo_symbol_type_number}
pattern SymString = MkSymbolType #{const clingo_symbol_type_string}
pattern SymFunction = MkSymbolType #{const clingo_symbol_type_function}
pattern SymSupremum = MkSymbolType #{const clingo_symbol_type_supremum}

newtype ModelType = MkModelType (#type clingo_model_type_t)
    deriving (Show, Eq)

pattern StableModel = MkModelType #{const clingo_model_type_stable_model}
pattern BraveConsequences = MkModelType #{const clingo_model_type_brave_consequences}
pattern CautiousConsequences = MkModelType #{const clingo_model_type_cautious_consequences}

newtype ShowFlag = MkShowFlag (#type clingo_show_type_bitset_t)
    deriving (Show, Eq, Flags)

pattern ShowCSP = MkShowFlag #{const clingo_show_type_csp}
pattern ShowShown = MkShowFlag #{const clingo_show_type_shown}
pattern ShowAtoms = MkShowFlag #{const clingo_show_type_atoms}
pattern ShowTerms = MkShowFlag #{const clingo_show_type_terms}
pattern ShowExtra = MkShowFlag #{const clingo_show_type_extra}
pattern ShowAll = MkShowFlag #{const clingo_show_type_all}
pattern ShowComplement = MkShowFlag #{const clingo_show_type_complement}

newtype SolveResult = MkSolveResult (#type clingo_solve_result_bitset_t)
    deriving (Show, Eq, Flags)

pattern ResultSatisfiable = MkSolveResult #{const clingo_solve_result_satisfiable}
pattern ResultUnsatisfiable = MkSolveResult #{const clingo_solve_result_unsatisfiable}
pattern ResultExhausted = MkSolveResult #{const clingo_solve_result_exhausted}
pattern ResultInterrupted = MkSolveResult #{const clingo_solve_result_interrupted}

newtype TheoryTerm = MkTheoryTerm (#type clingo_theory_term_type_t)
    deriving (Show, Eq)

pattern TheoryTuple = MkTheoryTerm #{const clingo_theory_term_type_tuple}
pattern TheoryList = MkTheoryTerm #{const clingo_theory_term_type_list}
pattern TheorySet = MkTheoryTerm #{const clingo_theory_term_type_set}
pattern TheoryFunction = MkTheoryTerm #{const clingo_theory_term_type_function}
pattern TheoryNumber = MkTheoryTerm #{const clingo_theory_term_type_number}
pattern TheorySymbol = MkTheoryTerm #{const clingo_theory_term_type_symbol}

newtype ClauseType = MkClauseType (#type clingo_clause_type_t)
    deriving (Show, Eq)

pattern ClauseLearnt = MkClauseType #{const clingo_clause_type_learnt}
pattern ClauseStatic = MkClauseType #{const clingo_clause_type_static}
pattern ClauseVolatile = MkClauseType #{const clingo_clause_type_volatile}
pattern ClauseVolatileStatic = MkClauseType #{const clingo_clause_type_volatile_static}

newtype HeuristicType = MkHeuristicType (#type clingo_heuristic_type_t)
    deriving (Show, Eq)

pattern HeuristicLevel = MkHeuristicType #{const clingo_heuristic_type_level}
pattern HeuristicSign = MkHeuristicType #{const clingo_heuristic_type_sign}
pattern HeuristicFactor = MkHeuristicType #{const clingo_heuristic_type_factor}
pattern HeuristicInit = MkHeuristicType #{const clingo_heuristic_type_init}
pattern HeuristicTrue = MkHeuristicType #{const clingo_heuristic_type_true}
pattern HeuristicFalse = MkHeuristicType #{const clingo_heuristic_type_false}

newtype ExternalType = MkExternalType (#type clingo_external_type_t)
    deriving (Show, Eq)

pattern ExternalFree = MkExternalType #{const clingo_external_type_free}
pattern ExternalTrue = MkExternalType #{const clingo_external_type_true}
pattern ExternalFalse = MkExternalType #{const clingo_external_type_false}
pattern ExternalRelease = MkExternalType #{const clingo_external_type_release}

newtype ConfigurationType = MkConfigType (#type clingo_configuration_type_bitset_t)
    deriving (Show, Eq, Flags)

pattern ConfigValue = MkConfigType #{const clingo_configuration_type_value}
pattern ConfigArray = MkConfigType #{const clingo_configuration_type_array}
pattern ConfigMap = MkConfigType #{const clingo_configuration_type_map}

newtype StatisticsType = MkStatisticsType (#type clingo_statistics_type_t)
    deriving (Show, Eq)

pattern StatsEmpty = MkStatisticsType #{const clingo_statistics_type_empty}
pattern StatsValue = MkStatisticsType #{const clingo_statistics_type_value}
pattern StatsArray = MkStatisticsType #{const clingo_statistics_type_array}
pattern StatsMap = MkStatisticsType #{const clingo_statistics_type_map}
