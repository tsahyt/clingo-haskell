{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Clingo.Raw.AST
(
    -- * Enumerations
    AstComparisonOperator,

    pattern AstComparisonOperatorGreaterThan,
    pattern AstComparisonOperatorLessThan,
    pattern AstComparisonOperatorLessEqual,
    pattern AstComparisonOperatorGreaterEqual,
    pattern AstComparisonOperatorNotEqual,
    pattern AstComparisonOperatorEqual,

    AstSign,

    pattern AstSignNone,
    pattern AstSignNegation,
    pattern AstSignDoubleNegation,

    AstTermType,

    pattern AstTermTypeSymbol,
    pattern AstTermTypeVariable,
    pattern AstTermTypeUnaryOperation,
    pattern AstTermTypeBinaryOperation,
    pattern AstTermTypeInterval,
    pattern AstTermTypeFunction,
    pattern AstTermTypeExternalFunction,
    pattern AstTermTypePool,

    AstUnaryOperator,

    pattern AstUnaryOperatorMinus,
    pattern AstUnaryOperatorNegation,
    pattern AstUnaryOperatorAbsolute,

    AstBinaryOperator,

    pattern AstBinaryOperatorXor,
    pattern AstBinaryOperatorOr,
    pattern AstBinaryOperatorAnd,
    pattern AstBinaryOperatorPlus,
    pattern AstBinaryOperatorMinus,
    pattern AstBinaryOperatorMultiplication,
    pattern AstBinaryOperatorDivision,
    pattern AstBinaryOperatorModulo,

    AstLiteralType,

    pattern AstLiteralTypeBoolean,
    pattern AstLiteralTypeSymbolic,
    pattern AstLiteralTypeComparison,
    pattern AstLiteralTypeCsp,

    AstAggregateFunction,

    pattern AstAggregateFunctionCount,
    pattern AstAggregateFunctionSum,
    pattern AstAggregateFunctionSump,
    pattern AstAggregateFunctionMin,
    pattern AstAggregateFunctionMax,

    AstTheoryTermType,

    pattern AstTheoryTermTypeSymbol,
    pattern AstTheoryTermTypeVariable,
    pattern AstTheoryTermTypeTuple,
    pattern AstTheoryTermTypeList,
    pattern AstTheoryTermTypeSet,
    pattern AstTheoryTermTypeFunction,
    pattern AstTheoryTermTypeUnparsedTerm,

    AstHeadLiteralType,

    pattern AstHeadLiteralTypeLiteral,
    pattern AstHeadLiteralTypeDisjunction,
    pattern AstHeadLiteralTypeAggregate,
    pattern AstHeadLiteralTypeHeadAggregate,
    pattern AstHeadLiteralTypeTheoryAtom,

    AstBodyLiteralType,

    pattern AstBodyLiteralTypeLiteral,
    pattern AstBodyLiteralTypeConditional,
    pattern AstBodyLiteralTypeAggregate,
    pattern AstBodyLiteralTypeBodyAggregate,
    pattern AstBodyLiteralTypeTheoryAtom,
    pattern AstBodyLiteralTypeDisjoint,

    AstTheoryOperatorType,

    pattern AstTheoryOperatorTypeUnary,
    pattern AstTheoryOperatorTypeBinaryLeft,
    pattern AstTheoryOperatorTypeBinaryRight,

    AstTheoryAtomDefType,

    pattern AstTheoryAtomDefinitionTypeHead,
    pattern AstTheoryAtomDefinitionTypeBody,
    pattern AstTheoryAtomDefinitionTypeAny,
    pattern AstTheoryAtomDefinitionTypeDirective,

    AstScriptType,

    pattern AstScriptTypeLua,
    pattern AstScriptTypePython,

    AstStatementType,

    pattern AstStatementTypeRule,
    pattern AstStatementTypeConst,
    pattern AstStatementTypeShowSignature,
    pattern AstStatementTypeShowTerm,
    pattern AstStatementTypeMinimize,
    pattern AstStatementTypeScript,
    pattern AstStatementTypeProgram,
    pattern AstStatementTypeExternal,
    pattern AstStatementTypeEdge,
    pattern AstStatementTypeHeuristic,
    pattern AstStatementTypeProjectAtom,
    pattern AstStatementTypeProjectAtomSignature,
    pattern AstStatementTypeTheoryDefinition,

    -- * Syntax Tree Definitions
    AstUnaryOperation (..),
    AstBinaryOperation (..),
    AstInterval (..),
    AstFunction (..),
    AstPool (..),
    AstTerm (..),
    AstCspProductTerm (..),
    AstCspSumTerm (..),
    AstCspGuard (..),
    AstCspLiteral (..),
    AstId (..),
    AstComparison (..),
    AstLiteral (..),
    AstAggregateGuard (..),
    AstConditionalLiteral (..),
    AstAggregate (..),
    AstBodyAggregateElement (..),
    AstBodyAggregate (..),
    AstHeadAggregateElement (..),
    AstHeadAggregate (..),
    AstDisjunction (..),
    AstDisjointElement (..),
    AstDisjoint (..),
    AstTheoryTermArray (..),
    AstTheoryFunction (..),
    AstTheoryUnparsedTermElement (..),
    AstTheoryUnparsedTerm (..),
    AstTheoryTerm (..),
    AstTheoryAtomElement (..),
    AstTheoryGuard (..),
    AstTheoryAtom (..),
    AstHeadLiteral (..),
    AstBodyLiteral (..),
    AstTheoryOperatorDefinition (..),
    AstTheoryTermDefinition (..),
    AstTheoryGuardDefinition (..),
    AstTheoryAtomDefinition (..),
    AstTheoryDefinition (..),
    AstRule (..),
    AstDefinition (..),
    AstShowSignature (..),
    AstShowTerm (..),
    AstMinimize (..),
    AstScript (..),
    AstProgram (..),
    AstExternal (..),
    AstEdge (..),
    AstHeuristic (..),
    AstProject (..),
    AstStatement (..),

    -- * Functions
    CallbackAST,
    mkCallbackAst,
    parseProgram,
)
where

import Control.Monad.IO.Class

import Foreign
import Foreign.C

import Clingo.Raw.Types

#include <clingo.h>

type AstComparisonOperator = #type clingo_ast_comparison_operator_t

pattern AstComparisonOperatorGreaterThan = #{const clingo_ast_comparison_operator_greater_than}
pattern AstComparisonOperatorLessThan = #{const clingo_ast_comparison_operator_less_than}
pattern AstComparisonOperatorLessEqual = #{const clingo_ast_comparison_operator_less_equal}
pattern AstComparisonOperatorGreaterEqual = #{const clingo_ast_comparison_operator_greater_equal}
pattern AstComparisonOperatorNotEqual = #{const clingo_ast_comparison_operator_not_equal}
pattern AstComparisonOperatorEqual = #{const clingo_ast_comparison_operator_equal}

type AstSign = #type clingo_ast_sign_t

pattern AstSignNone = #{const clingo_ast_sign_none}
pattern AstSignNegation = #{const clingo_ast_sign_negation}
pattern AstSignDoubleNegation = #{const clingo_ast_sign_double_negation}

type AstTermType = #type clingo_ast_term_type_t

pattern AstTermTypeSymbol = #{const clingo_ast_term_type_symbol}
pattern AstTermTypeVariable = #{const clingo_ast_term_type_variable}
pattern AstTermTypeUnaryOperation = #{const clingo_ast_term_type_unary_operation}
pattern AstTermTypeBinaryOperation = #{const clingo_ast_term_type_binary_operation}
pattern AstTermTypeInterval = #{const clingo_ast_term_type_interval}
pattern AstTermTypeFunction = #{const clingo_ast_term_type_function}
pattern AstTermTypeExternalFunction = #{const clingo_ast_term_type_external_function}
pattern AstTermTypePool = #{const clingo_ast_term_type_pool}

type AstUnaryOperator = #type clingo_ast_unary_operator_t

pattern AstUnaryOperatorMinus = #{const clingo_ast_unary_operator_minus}
pattern AstUnaryOperatorNegation = #{const clingo_ast_unary_operator_negation}
pattern AstUnaryOperatorAbsolute = #{const clingo_ast_unary_operator_absolute}

type AstBinaryOperator = #type clingo_ast_binary_operator_t

pattern AstBinaryOperatorXor = #{const clingo_ast_binary_operator_xor}
pattern AstBinaryOperatorOr = #{const clingo_ast_binary_operator_or}
pattern AstBinaryOperatorAnd = #{const clingo_ast_binary_operator_and}
pattern AstBinaryOperatorPlus = #{const clingo_ast_binary_operator_plus}
pattern AstBinaryOperatorMinus = #{const clingo_ast_binary_operator_minus}
pattern AstBinaryOperatorMultiplication = #{const clingo_ast_binary_operator_multiplication}
pattern AstBinaryOperatorDivision = #{const clingo_ast_binary_operator_division}
pattern AstBinaryOperatorModulo = #{const clingo_ast_binary_operator_modulo}

type AstLiteralType = #type clingo_ast_literal_type_t

pattern AstLiteralTypeBoolean = #{const clingo_ast_literal_type_boolean}
pattern AstLiteralTypeSymbolic = #{const clingo_ast_literal_type_symbolic}
pattern AstLiteralTypeComparison = #{const clingo_ast_literal_type_comparison}
pattern AstLiteralTypeCsp = #{const clingo_ast_literal_type_csp}

type AstAggregateFunction = #type clingo_ast_aggregate_function_t

pattern AstAggregateFunctionCount = #{const clingo_ast_aggregate_function_count}
pattern AstAggregateFunctionSum = #{const clingo_ast_aggregate_function_sum}
pattern AstAggregateFunctionSump = #{const clingo_ast_aggregate_function_sump}
pattern AstAggregateFunctionMin = #{const clingo_ast_aggregate_function_min}
pattern AstAggregateFunctionMax = #{const clingo_ast_aggregate_function_max}

type AstTheoryTermType = #type clingo_ast_theory_term_type_t

pattern AstTheoryTermTypeSymbol = #{const clingo_ast_theory_term_type_symbol}
pattern AstTheoryTermTypeVariable = #{const clingo_ast_theory_term_type_variable}
pattern AstTheoryTermTypeTuple = #{const clingo_ast_theory_term_type_tuple}
pattern AstTheoryTermTypeList = #{const clingo_ast_theory_term_type_list}
pattern AstTheoryTermTypeSet = #{const clingo_ast_theory_term_type_set}
pattern AstTheoryTermTypeFunction = #{const clingo_ast_theory_term_type_function}
pattern AstTheoryTermTypeUnparsedTerm = #{const clingo_ast_theory_term_type_unparsed_term}

type AstHeadLiteralType = #type clingo_ast_head_literal_type_t

pattern AstHeadLiteralTypeLiteral = #{const clingo_ast_head_literal_type_literal}
pattern AstHeadLiteralTypeDisjunction = #{const clingo_ast_head_literal_type_disjunction}
pattern AstHeadLiteralTypeAggregate = #{const clingo_ast_head_literal_type_aggregate}
pattern AstHeadLiteralTypeHeadAggregate = #{const clingo_ast_head_literal_type_head_aggregate}
pattern AstHeadLiteralTypeTheoryAtom = #{const clingo_ast_head_literal_type_theory_atom}

type AstBodyLiteralType = #type clingo_ast_body_literal_type_t

pattern AstBodyLiteralTypeLiteral = #{const clingo_ast_body_literal_type_literal}
pattern AstBodyLiteralTypeConditional = #{const clingo_ast_body_literal_type_conditional}
pattern AstBodyLiteralTypeAggregate = #{const clingo_ast_body_literal_type_aggregate}
pattern AstBodyLiteralTypeBodyAggregate = #{const clingo_ast_body_literal_type_body_aggregate}
pattern AstBodyLiteralTypeTheoryAtom = #{const clingo_ast_body_literal_type_theory_atom}
pattern AstBodyLiteralTypeDisjoint = #{const clingo_ast_body_literal_type_disjoint}

type AstTheoryOperatorType = #type clingo_ast_theory_operator_type_t

pattern AstTheoryOperatorTypeUnary = #{const clingo_ast_theory_operator_type_unary}
pattern AstTheoryOperatorTypeBinaryLeft = #{const clingo_ast_theory_operator_type_binary_left}
pattern AstTheoryOperatorTypeBinaryRight = #{const clingo_ast_theory_operator_type_binary_right}

type AstTheoryAtomDefType = #type clingo_ast_theory_atom_definition_type_t

pattern AstTheoryAtomDefinitionTypeHead = #{const clingo_ast_theory_atom_definition_type_head}
pattern AstTheoryAtomDefinitionTypeBody = #{const clingo_ast_theory_atom_definition_type_body}
pattern AstTheoryAtomDefinitionTypeAny = #{const clingo_ast_theory_atom_definition_type_any}
pattern AstTheoryAtomDefinitionTypeDirective = #{const clingo_ast_theory_atom_definition_type_directive}

type AstScriptType = #type clingo_ast_script_type_t

pattern AstScriptTypeLua = #{const clingo_ast_script_type_lua}
pattern AstScriptTypePython = #{const clingo_ast_script_type_python}

type AstStatementType = #type clingo_ast_statement_type_t

pattern AstStatementTypeRule = #{const clingo_ast_statement_type_rule}
pattern AstStatementTypeConst = #{const clingo_ast_statement_type_const}
pattern AstStatementTypeShowSignature = #{const clingo_ast_statement_type_show_signature}
pattern AstStatementTypeShowTerm = #{const clingo_ast_statement_type_show_term}
pattern AstStatementTypeMinimize = #{const clingo_ast_statement_type_minimize}
pattern AstStatementTypeScript = #{const clingo_ast_statement_type_script}
pattern AstStatementTypeProgram = #{const clingo_ast_statement_type_program}
pattern AstStatementTypeExternal = #{const clingo_ast_statement_type_external}
pattern AstStatementTypeEdge = #{const clingo_ast_statement_type_edge}
pattern AstStatementTypeHeuristic = #{const clingo_ast_statement_type_heuristic}
pattern AstStatementTypeProjectAtom = #{const clingo_ast_statement_type_project_atom}
pattern AstStatementTypeProjectAtomSignature = #{const clingo_ast_statement_type_project_atom_signature}
pattern AstStatementTypeTheoryDefinition = #{const clingo_ast_statement_type_theory_definition}

type CallbackAST a = Ptr AstStatement -> Ptr a -> IO CBool 

foreign import ccall "wrapper" mkCallbackAst ::
    CallbackAST a -> IO (FunPtr (CallbackAST a))

data AstUnaryOperation = AstUnaryOperation AstUnaryOperator AstTerm
    deriving (Eq, Show)

instance Storable AstUnaryOperation where
    sizeOf _ = #{size clingo_ast_unary_operation_t}
    alignment = sizeOf
    peek p = AstUnaryOperation 
        <$> (#{peek clingo_ast_unary_operation_t, unary_operator} p)
        <*> (#{peek clingo_ast_unary_operation_t, argument} p)
    poke p (AstUnaryOperation a b) = do
        (#poke clingo_ast_unary_operation_t, unary_operator) p a
        (#poke clingo_ast_unary_operation_t, argument) p b
   
data AstBinaryOperation = AstBinaryOperation AstBinaryOperator AstTerm AstTerm
    deriving (Eq, Show)

instance Storable AstBinaryOperation where
    sizeOf _ = #{size clingo_ast_binary_operation_t}
    alignment = sizeOf
    peek p = AstBinaryOperation 
        <$> (#{peek clingo_ast_binary_operation_t, binary_operator} p)
        <*> (#{peek clingo_ast_binary_operation_t, left} p)
        <*> (#{peek clingo_ast_binary_operation_t, right} p)
    poke p (AstBinaryOperation a b c) = do
        (#poke clingo_ast_binary_operation_t, binary_operator) p a
        (#poke clingo_ast_binary_operation_t, left) p b
        (#poke clingo_ast_binary_operation_t, right) p c

data AstInterval = AstInterval AstTerm AstTerm
    deriving (Eq, Show)

instance Storable AstInterval where
    sizeOf _ = #{size clingo_ast_interval_t}
    alignment = sizeOf
    peek p = AstInterval 
        <$> (#{peek clingo_ast_interval_t, left} p)
        <*> (#{peek clingo_ast_interval_t, right} p)
    poke p (AstInterval a b) = do
        (#poke clingo_ast_interval_t, left) p a
        (#poke clingo_ast_interval_t, right) p b

data AstFunction = AstFunction CString (Ptr AstTerm) CSize
    deriving (Eq, Show)

instance Storable AstFunction where
    sizeOf _ = #{size clingo_ast_function_t}
    alignment = sizeOf
    peek p = AstFunction 
        <$> (#{peek clingo_ast_function_t, name} p)
        <*> (#{peek clingo_ast_function_t, arguments} p)
        <*> (#{peek clingo_ast_function_t, size} p)
    poke p (AstFunction a b c) = do
        (#poke clingo_ast_function_t, name) p a
        (#poke clingo_ast_function_t, arguments) p b
        (#poke clingo_ast_function_t, size) p c

data AstPool = AstPool (Ptr AstTerm) CSize
    deriving (Eq, Show)

instance Storable AstPool where
    sizeOf _ = #{size clingo_ast_pool_t}
    alignment = sizeOf
    peek p = AstPool 
        <$> (#{peek clingo_ast_pool_t, arguments} p)
        <*> (#{peek clingo_ast_pool_t, size} p)
    poke p (AstPool a b) = do
        (#poke clingo_ast_pool_t, arguments) p a
        (#poke clingo_ast_pool_t, size) p b

data AstTerm = AstTermSymbol Location Symbol
             | AstTermVariable Location CString
             | AstTermUOp Location (Ptr AstUnaryOperation)
             | AstTermBOp Location (Ptr AstBinaryOperation)
             | AstTermInterval Location (Ptr AstInterval)
             | AstTermFunction Location (Ptr AstFunction)
             | AstTermExtFunction Location (Ptr AstFunction)
             | AstTermPool Location (Ptr AstPool)
    deriving (Eq, Show)

instance Storable AstTerm where
    sizeOf _ = #{size clingo_ast_term_t}
    alignment = sizeOf
    peek p = do
        loc <- (#{peek clingo_ast_term_t, location} p)
        typ :: AstTermType <- (#{peek clingo_ast_term_t, type} p)
        case typ of
            AstTermTypeSymbol -> do
                payload <- (#{peek clingo_ast_term_t, symbol} p)
                pure $! AstTermSymbol loc payload
            AstTermTypeVariable -> do
                payload <- (#{peek clingo_ast_term_t, variable} p)
                pure $! AstTermVariable loc payload
            AstTermTypeUnaryOperation -> do
                payload <- (#{peek clingo_ast_term_t, unary_operation} p)
                pure $! AstTermUOp loc payload
            AstTermTypeBinaryOperation -> do
                payload <- (#{peek clingo_ast_term_t, binary_operation} p)
                pure $! AstTermBOp loc payload
            AstTermTypeInterval -> do
                payload <- (#{peek clingo_ast_term_t, interval} p)
                pure $! AstTermInterval loc payload
            AstTermTypeFunction -> do
                payload <- (#{peek clingo_ast_term_t, function} p)
                pure $! AstTermFunction loc payload
            AstTermTypeExternalFunction -> do
                payload <- (#{peek clingo_ast_term_t, external_function} p)
                pure $! AstTermExtFunction loc payload
            AstTermTypePool -> do
                payload <- (#{peek clingo_ast_term_t, pool} p)
                pure $! AstTermPool loc payload
            _ -> error "Malformed struct clingo_ast_term_t"
    poke p d = case d of
        AstTermSymbol l x -> do
            (#poke clingo_ast_term_t, location) p l
            (#poke clingo_ast_term_t, type) p (AstTermTypeSymbol :: AstTermType)
            (#poke clingo_ast_term_t, symbol) p x
        AstTermVariable l x -> do
            (#poke clingo_ast_term_t, location) p l
            (#poke clingo_ast_term_t, type) p (AstTermTypeVariable :: AstTermType)
            (#poke clingo_ast_term_t, variable) p x
        AstTermUOp l x -> do
            (#poke clingo_ast_term_t, location) p l
            (#poke clingo_ast_term_t, type) p (AstTermTypeUnaryOperation :: AstTermType)
            (#poke clingo_ast_term_t, unary_operation) p x
        AstTermBOp l x -> do
            (#poke clingo_ast_term_t, location) p l
            (#poke clingo_ast_term_t, type) p (AstTermTypeBinaryOperation :: AstTermType)
            (#poke clingo_ast_term_t, binary_operation) p x
        AstTermInterval l x -> do
            (#poke clingo_ast_term_t, location) p l
            (#poke clingo_ast_term_t, type) p (AstTermTypeInterval :: AstTermType)
            (#poke clingo_ast_term_t, interval) p x
        AstTermFunction l x -> do
            (#poke clingo_ast_term_t, location) p l
            (#poke clingo_ast_term_t, type) p (AstTermTypeFunction :: AstTermType)
            (#poke clingo_ast_term_t, function) p x
        AstTermExtFunction l x -> do
            (#poke clingo_ast_term_t, location) p l
            (#poke clingo_ast_term_t, type) p (AstTermTypeExternalFunction :: AstTermType)
            (#poke clingo_ast_term_t, external_function) p x
        AstTermPool l x -> do
            (#poke clingo_ast_term_t, location) p l
            (#poke clingo_ast_term_t, type) p (AstTermTypePool :: AstTermType)
            (#poke clingo_ast_term_t, pool) p x

data AstCspProductTerm = AstCspProductTerm Location AstTerm (Ptr AstTerm)
    deriving (Eq, Show)

instance Storable AstCspProductTerm where
    sizeOf _ = #{size clingo_ast_csp_product_term_t}
    alignment = sizeOf
    peek p = AstCspProductTerm 
        <$> (#{peek clingo_ast_csp_product_term_t, location} p)
        <*> (#{peek clingo_ast_csp_product_term_t, coefficient} p)
        <*> (#{peek clingo_ast_csp_product_term_t, variable} p)
    poke p (AstCspProductTerm a b c) = do
        (#poke clingo_ast_csp_product_term_t, location) p a
        (#poke clingo_ast_csp_product_term_t, coefficient) p b
        (#poke clingo_ast_csp_product_term_t, variable) p c

data AstCspSumTerm = AstCspSumTerm Location (Ptr AstCspProductTerm) CSize
    deriving (Eq, Show)

instance Storable AstCspSumTerm where
    sizeOf _ = #{size clingo_ast_csp_sum_term_t}
    alignment = sizeOf
    peek p = AstCspSumTerm 
        <$> (#{peek clingo_ast_csp_sum_term_t, location} p)
        <*> (#{peek clingo_ast_csp_sum_term_t, terms} p)
        <*> (#{peek clingo_ast_csp_sum_term_t, size} p)
    poke p (AstCspSumTerm a b c) = do
        (#poke clingo_ast_csp_sum_term_t, location) p a
        (#poke clingo_ast_csp_sum_term_t, terms) p b
        (#poke clingo_ast_csp_sum_term_t, size) p c
    
data AstCspGuard = AstCspGuard AstComparisonOperator AstCspSumTerm
    deriving (Eq, Show)

instance Storable AstCspGuard where
    sizeOf _ = #{size clingo_ast_csp_guard_t}
    alignment = sizeOf
    peek p = AstCspGuard 
        <$> (#{peek clingo_ast_csp_guard_t, comparison} p)
        <*> (#{peek clingo_ast_csp_guard_t, term} p)
    poke p (AstCspGuard a b) = do
        (#poke clingo_ast_csp_guard_t, comparison) p a
        (#poke clingo_ast_csp_guard_t, term) p b

data AstCspLiteral = AstCspLiteral AstCspSumTerm (Ptr AstCspGuard) CSize
    deriving (Eq, Show)

instance Storable AstCspLiteral where
    sizeOf _ = #{size clingo_ast_csp_literal_t}
    alignment = sizeOf
    peek p = AstCspLiteral 
        <$> (#{peek clingo_ast_csp_literal_t, term} p)
        <*> (#{peek clingo_ast_csp_literal_t, guards} p)
        <*> (#{peek clingo_ast_csp_literal_t, size} p)
    poke p (AstCspLiteral a b c) = do
        (#poke clingo_ast_csp_literal_t, term) p a
        (#poke clingo_ast_csp_literal_t, guards) p b
        (#poke clingo_ast_csp_literal_t, size) p c
    
data AstId = AstId Location CString
    deriving (Eq, Show)

instance Storable AstId where
    sizeOf _ = #{size clingo_ast_id_t}
    alignment = sizeOf
    peek p = AstId 
        <$> (#{peek clingo_ast_id_t, location} p)
        <*> (#{peek clingo_ast_id_t, id} p)
    poke p (AstId a b) = do
        (#poke clingo_ast_id_t, location) p a
        (#poke clingo_ast_id_t, id) p b

data AstComparison = AstComparison AstComparisonOperator AstTerm AstTerm
    deriving (Eq, Show)

instance Storable AstComparison where
    sizeOf _ = #{size clingo_ast_comparison_t}
    alignment = sizeOf
    peek p = AstComparison 
        <$> (#{peek clingo_ast_comparison_t, comparison} p)
        <*> (#{peek clingo_ast_comparison_t, left} p)
        <*> (#{peek clingo_ast_comparison_t, right} p)
    poke p (AstComparison a b c) = do
        (#poke clingo_ast_comparison_t, comparison) p a
        (#poke clingo_ast_comparison_t, left) p b
        (#poke clingo_ast_comparison_t, right) p c

data AstLiteral = AstLiteralBool Location AstSign CBool
                | AstLiteralTerm Location AstSign (Ptr AstTerm)
                | AstLiteralComp Location AstSign (Ptr AstComparison)
                | AstLiteralCSPL Location AstSign (Ptr AstCspLiteral)
    deriving (Eq, Show)

instance Storable AstLiteral where
    sizeOf _ = #{size clingo_ast_literal_t}
    alignment = sizeOf
    peek p = do
        loc <- (#{peek clingo_ast_literal_t, location} p)
        sign <- (#{peek clingo_ast_literal_t, sign} p)
        typ :: AstLiteralType <- (#{peek clingo_ast_literal_t, type} p)
        case typ of
            AstLiteralTypeBoolean -> do
                payload <- (#{peek clingo_ast_literal_t, boolean} p)
                pure $! AstLiteralBool loc sign payload
            AstLiteralTypeSymbolic -> do
                payload <- (#{peek clingo_ast_literal_t, boolean} p)
                pure $! AstLiteralTerm loc sign payload
            AstLiteralTypeComparison -> do
                payload <- (#{peek clingo_ast_literal_t, boolean} p)
                pure $! AstLiteralComp loc sign payload
            AstLiteralTypeCsp -> do
                payload <- (#{peek clingo_ast_literal_t, boolean} p)
                pure $! AstLiteralCSPL loc sign payload
            _ -> error "Malformed struct clingo_ast_literal_t"
    poke p d = case d of
        AstLiteralBool l s x -> do
            (#poke clingo_ast_literal_t, location) p l
            (#poke clingo_ast_literal_t, sign) p s
            (#poke clingo_ast_literal_t, type) p (AstLiteralTypeBoolean :: AstLiteralType)
            (#poke clingo_ast_literal_t, boolean) p x
        AstLiteralTerm l s x -> do
            (#poke clingo_ast_literal_t, location) p l
            (#poke clingo_ast_literal_t, sign) p s
            (#poke clingo_ast_literal_t, type) p (AstLiteralTypeSymbolic :: AstLiteralType)
            (#poke clingo_ast_literal_t, symbol) p x
        AstLiteralComp l s x -> do
            (#poke clingo_ast_literal_t, location) p l
            (#poke clingo_ast_literal_t, sign) p s
            (#poke clingo_ast_literal_t, type) p (AstLiteralTypeComparison:: AstLiteralType)
            (#poke clingo_ast_literal_t, comparison) p x
        AstLiteralCSPL l s x -> do
            (#poke clingo_ast_literal_t, location) p l
            (#poke clingo_ast_literal_t, sign) p s
            (#poke clingo_ast_literal_t, type) p (AstLiteralTypeCsp :: AstLiteralType)
            (#poke clingo_ast_literal_t, csp_literal) p x

data AstAggregateGuard = AstAggregateGuard AstComparisonOperator AstTerm
    deriving (Eq, Show)

instance Storable AstAggregateGuard where
    sizeOf _ = #{size clingo_ast_aggregate_guard_t}
    alignment = sizeOf
    peek p = AstAggregateGuard 
        <$> (#{peek clingo_ast_aggregate_guard_t, comparison} p)
        <*> (#{peek clingo_ast_aggregate_guard_t, term} p)
    poke p (AstAggregateGuard a b) = do
        (#poke clingo_ast_aggregate_guard_t, comparison) p a
        (#poke clingo_ast_aggregate_guard_t, term) p b

data AstConditionalLiteral = AstConditionalLiteral AstLiteral 
                             (Ptr AstLiteral) CSize
    deriving (Eq, Show)

instance Storable AstConditionalLiteral where
    sizeOf _ = #{size clingo_ast_conditional_literal_t}
    alignment = sizeOf
    peek p = AstConditionalLiteral 
        <$> (#{peek clingo_ast_conditional_literal_t, literal} p)
        <*> (#{peek clingo_ast_conditional_literal_t, condition} p)
        <*> (#{peek clingo_ast_conditional_literal_t, size} p)
    poke p (AstConditionalLiteral a b c) = do
        (#poke clingo_ast_conditional_literal_t, literal) p a
        (#poke clingo_ast_conditional_literal_t, condition) p b
        (#poke clingo_ast_conditional_literal_t, size) p c

data AstAggregate = AstAggregate (Ptr AstConditionalLiteral) CSize
                    (Ptr AstAggregateGuard) (Ptr AstAggregateGuard)
    deriving (Eq, Show)

instance Storable AstAggregate where
    sizeOf _ = #{size clingo_ast_aggregate_t}
    alignment = sizeOf
    peek p = AstAggregate 
        <$> (#{peek clingo_ast_aggregate_t, elements} p)
        <*> (#{peek clingo_ast_aggregate_t, size} p)
        <*> (#{peek clingo_ast_aggregate_t, left_guard} p)
        <*> (#{peek clingo_ast_aggregate_t, right_guard} p)
    poke p (AstAggregate a b c d) = do
        (#poke clingo_ast_aggregate_t, elements) p a
        (#poke clingo_ast_aggregate_t, size) p b
        (#poke clingo_ast_aggregate_t, left_guard) p c
        (#poke clingo_ast_aggregate_t, right_guard) p d
    
data AstBodyAggregateElement = AstBodyAggregateElement (Ptr AstTerm) CSize 
                               (Ptr AstLiteral) CSize
    deriving (Eq, Show)

instance Storable AstBodyAggregateElement where
    sizeOf _ = #{size clingo_ast_body_aggregate_element_t}
    alignment = sizeOf
    peek p = AstBodyAggregateElement 
        <$> (#{peek clingo_ast_body_aggregate_element_t, tuple} p)
        <*> (#{peek clingo_ast_body_aggregate_element_t, tuple_size} p)
        <*> (#{peek clingo_ast_body_aggregate_element_t, condition} p)
        <*> (#{peek clingo_ast_body_aggregate_element_t, condition_size} p)
    poke p (AstBodyAggregateElement a b c d) = do
        (#poke clingo_ast_body_aggregate_element_t, tuple) p a
        (#poke clingo_ast_body_aggregate_element_t, tuple_size) p b
        (#poke clingo_ast_body_aggregate_element_t, condition) p c
        (#poke clingo_ast_body_aggregate_element_t, condition_size) p d
    
data AstBodyAggregate = AstBodyAggregate AstAggregateFunction
                        (Ptr AstBodyAggregateElement) CSize
                        (Ptr AstAggregateGuard) (Ptr AstAggregateGuard)
    deriving (Eq, Show)

instance Storable AstBodyAggregate where
    sizeOf _ = #{size clingo_ast_body_aggregate_t}
    alignment = sizeOf
    peek p = AstBodyAggregate 
        <$> (#{peek clingo_ast_body_aggregate_t, function} p)
        <*> (#{peek clingo_ast_body_aggregate_t, elements} p)
        <*> (#{peek clingo_ast_body_aggregate_t, size} p)
        <*> (#{peek clingo_ast_body_aggregate_t, left_guard} p)
        <*> (#{peek clingo_ast_body_aggregate_t, right_guard} p)
    poke p (AstBodyAggregate a b c d e) = do
        (#poke clingo_ast_body_aggregate_t, function) p a
        (#poke clingo_ast_body_aggregate_t, elements) p b
        (#poke clingo_ast_body_aggregate_t, size) p c
        (#poke clingo_ast_body_aggregate_t, left_guard) p d
        (#poke clingo_ast_body_aggregate_t, right_guard) p e

data AstHeadAggregateElement = AstHeadAggregateElement (Ptr AstTerm) CSize
                               AstConditionalLiteral
    deriving (Eq, Show)

instance Storable AstHeadAggregateElement where
    sizeOf _ = #{size clingo_ast_head_aggregate_element_t}
    alignment = sizeOf
    peek p = AstHeadAggregateElement 
        <$> (#{peek clingo_ast_head_aggregate_element_t, tuple} p)
        <*> (#{peek clingo_ast_head_aggregate_element_t, tuple_size} p)
        <*> (#{peek clingo_ast_head_aggregate_element_t, conditional_literal} p)
    poke p (AstHeadAggregateElement a b c) = do
        (#poke clingo_ast_head_aggregate_element_t, tuple) p a
        (#poke clingo_ast_head_aggregate_element_t, tuple_size) p b
        (#poke clingo_ast_head_aggregate_element_t, conditional_literal) p c
    
data AstHeadAggregate = AstHeadAggregate AstAggregateFunction
                        (Ptr AstHeadAggregateElement) CSize
                        (Ptr AstAggregateGuard) (Ptr AstAggregateGuard)
    deriving (Eq, Show)

instance Storable AstHeadAggregate where
    sizeOf _ = #{size clingo_ast_aggregate_t}
    alignment = sizeOf
    peek p = AstHeadAggregate 
        <$> (#{peek clingo_ast_head_aggregate_t, function} p)
        <*> (#{peek clingo_ast_head_aggregate_t, elements} p)
        <*> (#{peek clingo_ast_head_aggregate_t, size} p)
        <*> (#{peek clingo_ast_head_aggregate_t, left_guard} p)
        <*> (#{peek clingo_ast_head_aggregate_t, right_guard} p)
    poke p (AstHeadAggregate a b c d e) = do
        (#poke clingo_ast_head_aggregate_t, function) p a
        (#poke clingo_ast_head_aggregate_t, elements) p b
        (#poke clingo_ast_head_aggregate_t, size) p c
        (#poke clingo_ast_head_aggregate_t, left_guard) p d
        (#poke clingo_ast_head_aggregate_t, right_guard) p e
    
data AstDisjunction = AstDisjunction (Ptr AstConditionalLiteral) CSize
    deriving (Eq, Show)

instance Storable AstDisjunction where
    sizeOf _ = #{size clingo_ast_disjunction_t}
    alignment = sizeOf
    peek p = AstDisjunction 
        <$> (#{peek clingo_ast_disjunction_t, elements} p)
        <*> (#{peek clingo_ast_disjunction_t, size} p)
    poke p (AstDisjunction a b) = do
        (#poke clingo_ast_disjunction_t, elements) p a
        (#poke clingo_ast_disjunction_t, size) p b
    
data AstDisjointElement = AstDisjointElement Location (Ptr AstTerm) CSize
                          AstCspSumTerm (Ptr AstLiteral) CSize
    deriving (Eq, Show)

instance Storable AstDisjointElement where
    sizeOf _ = #{size clingo_ast_disjoint_element_t}
    alignment = sizeOf
    peek p = AstDisjointElement 
        <$> (#{peek clingo_ast_disjoint_element_t, location} p)
        <*> (#{peek clingo_ast_disjoint_element_t, tuple} p)
        <*> (#{peek clingo_ast_disjoint_element_t, tuple_size} p)
        <*> (#{peek clingo_ast_disjoint_element_t, term} p)
        <*> (#{peek clingo_ast_disjoint_element_t, condition} p)
        <*> (#{peek clingo_ast_disjoint_element_t, condition_size} p)
    poke p (AstDisjointElement a b c d e f) = do
        (#poke clingo_ast_disjoint_element_t, location) p a
        (#poke clingo_ast_disjoint_element_t, tuple) p b
        (#poke clingo_ast_disjoint_element_t, tuple_size) p c
        (#poke clingo_ast_disjoint_element_t, term) p d
        (#poke clingo_ast_disjoint_element_t, condition) p e
        (#poke clingo_ast_disjoint_element_t, condition_size) p f
    
data AstDisjoint = AstDisjoint (Ptr AstDisjointElement) CSize
    deriving (Eq, Show)

instance Storable AstDisjoint where
    sizeOf _ = #{size clingo_ast_disjoint_t}
    alignment = sizeOf
    peek p = AstDisjoint 
        <$> (#{peek clingo_ast_disjoint_t, elements} p)
        <*> (#{peek clingo_ast_disjoint_t, size} p)
    poke p (AstDisjoint a b) = do
        (#poke clingo_ast_disjoint_t, elements) p a
        (#poke clingo_ast_disjoint_t, size) p b
    
data AstTheoryTermArray = AstTheoryTermArray (Ptr AstTheoryTerm) CSize
    deriving (Eq, Show)

instance Storable AstTheoryTermArray where
    sizeOf _ = #{size clingo_ast_theory_term_array_t}
    alignment = sizeOf
    peek p = AstTheoryTermArray 
        <$> (#{peek clingo_ast_theory_term_array_t, terms} p)
        <*> (#{peek clingo_ast_theory_term_array_t, size} p)
    poke p (AstTheoryTermArray a b) = do
        (#poke clingo_ast_theory_term_array_t, terms) p a
        (#poke clingo_ast_theory_term_array_t, size) p b

data AstTheoryFunction = AstTheoryFunction CString (Ptr AstTheoryTerm) CSize
    deriving (Eq, Show)

instance Storable AstTheoryFunction where
    sizeOf _ = #{size clingo_ast_theory_function_t}
    alignment = sizeOf
    peek p = AstTheoryFunction 
        <$> (#{peek clingo_ast_theory_function_t, name} p)
        <*> (#{peek clingo_ast_theory_function_t, arguments} p)
        <*> (#{peek clingo_ast_theory_function_t, size} p)
    poke p (AstTheoryFunction a b c) = do
        (#poke clingo_ast_theory_function_t, name) p a
        (#poke clingo_ast_theory_function_t, arguments) p b
        (#poke clingo_ast_theory_function_t, size) p c
    
data AstTheoryUnparsedTermElement = AstTheoryUnparsedTermElement 
                                    (Ptr CString) CSize AstTheoryTerm
    deriving (Eq, Show)

instance Storable AstTheoryUnparsedTermElement where
    sizeOf _ = #{size clingo_ast_theory_unparsed_term_element_t}
    alignment = sizeOf
    peek p = AstTheoryUnparsedTermElement 
        <$> (#{peek clingo_ast_theory_unparsed_term_element_t, operators} p)
        <*> (#{peek clingo_ast_theory_unparsed_term_element_t, size} p)
        <*> (#{peek clingo_ast_theory_unparsed_term_element_t, term} p)
    poke p (AstTheoryUnparsedTermElement a b c) = do
        (#poke clingo_ast_theory_unparsed_term_element_t, operators) p a
        (#poke clingo_ast_theory_unparsed_term_element_t, size) p b
        (#poke clingo_ast_theory_unparsed_term_element_t, term) p c

data AstTheoryUnparsedTerm = AstTheoryUnparsedTerm 
                             (Ptr AstTheoryUnparsedTermElement) CSize
    deriving (Eq, Show)

instance Storable AstTheoryUnparsedTerm where
    sizeOf _ = #{size clingo_ast_theory_unparsed_term_t}
    alignment = sizeOf
    peek p = AstTheoryUnparsedTerm 
        <$> (#{peek clingo_ast_theory_unparsed_term_t, elements} p)
        <*> (#{peek clingo_ast_theory_unparsed_term_t, size} p)
    poke p (AstTheoryUnparsedTerm a b) = do
        (#poke clingo_ast_theory_unparsed_term_t, elements) p a
        (#poke clingo_ast_theory_unparsed_term_t, size) p b
    
data AstTheoryTerm = AstTheoryTermSymbol Location Symbol
                   | AstTheoryTermVariable Location CString
                   | AstTheoryTermTuple Location (Ptr AstTheoryTermArray)
                   | AstTheoryTermList Location (Ptr AstTheoryTermArray)
                   | AstTheoryTermSet Location (Ptr AstTheoryTermArray)
                   | AstTheoryTermFunction Location (Ptr AstTheoryFunction)
                   | AstTheoryTermUnparsed Location (Ptr AstTheoryUnparsedTerm)
    deriving (Eq, Show)

instance Storable AstTheoryTerm where
    sizeOf _ = #{size clingo_ast_theory_term_t}
    alignment = sizeOf
    peek p = do
        loc <- (#{peek clingo_ast_theory_term_t, location} p)
        typ :: AstTheoryTermType <- (#{peek clingo_ast_theory_term_t, type} p)
        case typ of
            AstTheoryTermTypeSymbol -> do
                payload <- (#{peek clingo_ast_theory_term_t, symbol} p)
                pure $! AstTheoryTermSymbol loc payload
            AstTheoryTermTypeVariable -> do
                payload <- (#{peek clingo_ast_theory_term_t, variable} p)
                pure $! AstTheoryTermVariable loc payload
            AstTheoryTermTypeTuple -> do
                payload <- (#{peek clingo_ast_theory_term_t, tuple} p)
                pure $! AstTheoryTermTuple loc payload
            AstTheoryTermTypeList -> do
                payload <- (#{peek clingo_ast_theory_term_t, list} p)
                pure $! AstTheoryTermList loc payload
            AstTheoryTermTypeSet -> do
                payload <- (#{peek clingo_ast_theory_term_t, set} p)
                pure $! AstTheoryTermSet loc payload
            AstTheoryTermTypeFunction -> do
                payload <- (#{peek clingo_ast_theory_term_t, function} p)
                pure $! AstTheoryTermFunction loc payload
            AstTheoryTermTypeUnparsedTerm -> do
                payload <- (#{peek clingo_ast_theory_term_t, unparsed_term} p)
                pure $! AstTheoryTermUnparsed loc payload
            _ -> error "Malformed struct clingo_ast_theory_term_t"
    poke p d = case d of
        AstTheoryTermSymbol l x -> do
            (#poke clingo_ast_theory_term_t, location) p l
            (#poke clingo_ast_theory_term_t, type) p (AstTheoryTermTypeSymbol :: AstTheoryTermType)
            (#poke clingo_ast_theory_term_t, symbol) p x
        AstTheoryTermVariable l x -> do
            (#poke clingo_ast_theory_term_t, location) p l
            (#poke clingo_ast_theory_term_t, type) p (AstTheoryTermTypeVariable :: AstTheoryTermType)
            (#poke clingo_ast_theory_term_t, variable) p x
        AstTheoryTermTuple l x -> do
            (#poke clingo_ast_theory_term_t, location) p l
            (#poke clingo_ast_theory_term_t, type) p (AstTheoryTermTypeTuple :: AstTheoryTermType)
            (#poke clingo_ast_theory_term_t, tuple) p x
        AstTheoryTermList l x -> do
            (#poke clingo_ast_theory_term_t, location) p l
            (#poke clingo_ast_theory_term_t, type) p (AstTheoryTermTypeList :: AstTheoryTermType)
            (#poke clingo_ast_theory_term_t, list) p x
        AstTheoryTermSet l x -> do
            (#poke clingo_ast_theory_term_t, location) p l
            (#poke clingo_ast_theory_term_t, type) p (AstTheoryTermTypeSet :: AstTheoryTermType)
            (#poke clingo_ast_theory_term_t, set) p x
        AstTheoryTermFunction l x -> do
            (#poke clingo_ast_theory_term_t, location) p l
            (#poke clingo_ast_theory_term_t, type) p (AstTheoryTermTypeFunction :: AstTheoryTermType)
            (#poke clingo_ast_theory_term_t, function) p x
        AstTheoryTermUnparsed l x -> do
            (#poke clingo_ast_theory_term_t, location) p l
            (#poke clingo_ast_theory_term_t, type) p (AstTheoryTermTypeUnparsedTerm :: AstTheoryTermType)
            (#poke clingo_ast_theory_term_t, unparsed_term) p x

data AstTheoryAtomElement = AstTheoryAtomElement (Ptr AstTheoryTerm) CSize
                            (Ptr AstLiteral) CSize
    deriving (Eq, Show)

instance Storable AstTheoryAtomElement where
    sizeOf _ = #{size clingo_ast_theory_atom_element_t}
    alignment = sizeOf
    peek p = AstTheoryAtomElement 
        <$> (#{peek clingo_ast_theory_atom_element_t, tuple} p)
        <*> (#{peek clingo_ast_theory_atom_element_t, tuple_size} p)
        <*> (#{peek clingo_ast_theory_atom_element_t, condition} p)
        <*> (#{peek clingo_ast_theory_atom_element_t, condition_size} p)
    poke p (AstTheoryAtomElement a b c d) = do
        (#poke clingo_ast_theory_atom_element_t, tuple) p a
        (#poke clingo_ast_theory_atom_element_t, tuple_size) p b
        (#poke clingo_ast_theory_atom_element_t, condition) p c
        (#poke clingo_ast_theory_atom_element_t, condition_size) p d
    
data AstTheoryGuard = AstTheoryGuard CString AstTheoryTerm
    deriving (Eq, Show)

instance Storable AstTheoryGuard where
    sizeOf _ = #{size clingo_ast_theory_guard_t}
    alignment = sizeOf
    peek p = AstTheoryGuard 
        <$> (#{peek clingo_ast_theory_guard_t, operator_name} p)
        <*> (#{peek clingo_ast_theory_guard_t, term} p)
    poke p (AstTheoryGuard a b) = do
        (#poke clingo_ast_theory_guard_t, operator_name) p a
        (#poke clingo_ast_theory_guard_t, term) p b

data AstTheoryAtom = AstTheoryAtom AstTerm (Ptr AstTheoryAtomElement) CSize
                     AstTheoryGuard
    deriving (Eq, Show)

instance Storable AstTheoryAtom where
    sizeOf _ = #{size clingo_ast_theory_atom_t}
    alignment = sizeOf
    peek p = AstTheoryAtom 
        <$> (#{peek clingo_ast_theory_atom_t, term} p)
        <*> (#{peek clingo_ast_theory_atom_t, elements} p)
        <*> (#{peek clingo_ast_theory_atom_t, size} p)
        <*> (#{peek clingo_ast_theory_atom_t, guard} p)
    poke p (AstTheoryAtom a b c d) = do
        (#poke clingo_ast_theory_atom_t, term) p a
        (#poke clingo_ast_theory_atom_t, elements) p b
        (#poke clingo_ast_theory_atom_t, size) p c
        (#poke clingo_ast_theory_atom_t, guard) p d

data AstHeadLiteral = AstHeadLiteral Location (Ptr AstLiteral)
                    | AstHeadDisjunction Location (Ptr AstDisjunction)
                    | AstHeadLitAggregate Location (Ptr AstAggregate)
                    | AstHeadTheoryAtom Location (Ptr AstTheoryAtom)
    deriving (Eq, Show)

instance Storable AstHeadLiteral where
    sizeOf _ = #{size clingo_ast_head_literal_t}
    alignment = sizeOf
    peek p = do
        loc <- (#{peek clingo_ast_head_literal_t, location} p)
        typ :: AstHeadLiteralType <- (#{peek clingo_ast_head_literal_t, type} p)
        case typ of
            AstHeadLiteralTypeLiteral -> do
                payload <- (#{peek clingo_ast_head_literal_t, literal} p)
                pure $! AstHeadLiteral loc payload
            AstHeadLiteralTypeDisjunction -> do
                payload <- (#{peek clingo_ast_head_literal_t, disjunction} p)
                pure $! AstHeadDisjunction loc payload
            AstHeadLiteralTypeAggregate -> do
                payload <- (#{peek clingo_ast_head_literal_t, aggregate} p)
                pure $! AstHeadLitAggregate loc payload
            AstHeadLiteralTypeHeadAggregate -> do
                payload <- (#{peek clingo_ast_head_literal_t, head_aggregate} p)
                pure $! AstHeadLiteral loc payload
            AstHeadLiteralTypeTheoryAtom -> do
                payload <- (#{peek clingo_ast_head_literal_t, theory_atom} p)
                pure $! AstHeadTheoryAtom loc payload
            _ -> error "Malformed struct clingo_ast_head_literal_t"
    poke p d = case d of
        AstHeadLiteral l x -> do
            (#{poke clingo_ast_head_literal_t, location}) p l
            (#{poke clingo_ast_head_literal_t, type}) p (AstHeadLiteralTypeLiteral :: AstHeadLiteralType)
            (#{poke clingo_ast_head_literal_t, literal}) p x
        AstHeadDisjunction l x -> do
            (#{poke clingo_ast_head_literal_t, location}) p l
            (#{poke clingo_ast_head_literal_t, type}) p (AstHeadLiteralTypeLiteral :: AstHeadLiteralType)
            (#{poke clingo_ast_head_literal_t, disjunction}) p x
        AstHeadLitAggregate l x -> do
            (#{poke clingo_ast_head_literal_t, location}) p l
            (#{poke clingo_ast_head_literal_t, type}) p (AstHeadLiteralTypeLiteral :: AstHeadLiteralType)
            (#{poke clingo_ast_head_literal_t, aggregate}) p x
        AstHeadTheoryAtom l x -> do
            (#{poke clingo_ast_head_literal_t, location}) p l
            (#{poke clingo_ast_head_literal_t, type}) p (AstHeadLiteralTypeLiteral :: AstHeadLiteralType)
            (#{poke clingo_ast_head_literal_t, theory_atom}) p x

data AstBodyLiteral 
    = AstBodyLiteral Location AstSign (Ptr AstLiteral)
    | AstBodyConditional Location (Ptr AstConditionalLiteral)
    | AstBodyLitAggregate Location AstSign (Ptr AstAggregate)
    | AstBodyBodyAggregate Location AstSign (Ptr AstBodyAggregate)
    | AstBodyTheoryAtom Location AstSign (Ptr AstTheoryAtom)
    | AstBodyDisjoint Location AstSign (Ptr AstDisjoint)
    deriving (Eq, Show)

instance Storable AstBodyLiteral where
    sizeOf _ = #{size clingo_ast_body_literal_t}
    alignment = sizeOf
    peek p = do
        loc <- (#{peek clingo_ast_body_literal_t, location} p)
        sign <- (#{peek clingo_ast_body_literal_t, sign} p)
        typ :: AstBodyLiteralType <- (#{peek clingo_ast_body_literal_t, type} p)
        case typ of
            AstBodyLiteralTypeLiteral -> do
                payload <- (#{peek clingo_ast_body_literal_t, literal} p)
                pure $! AstBodyLiteral loc sign payload
            AstBodyLiteralTypeConditional -> do
                payload <- (#{peek clingo_ast_body_literal_t, conditional} p)
                pure $! AstBodyConditional loc payload
            AstBodyLiteralTypeAggregate -> do
                payload <- (#{peek clingo_ast_body_literal_t, aggregate} p)
                pure $! AstBodyLitAggregate loc sign payload
            AstBodyLiteralTypeBodyAggregate -> do
                payload <- (#{peek clingo_ast_body_literal_t, body_aggregate} p)
                pure $! AstBodyBodyAggregate loc sign payload
            AstBodyLiteralTypeTheoryAtom -> do
                payload <- (#{peek clingo_ast_body_literal_t, theory_atom} p)
                pure $! AstBodyTheoryAtom loc sign payload
            AstBodyLiteralTypeDisjoint -> do
                payload <- (#{peek clingo_ast_body_literal_t, disjoint} p)
                pure $! AstBodyDisjoint loc sign payload
            _ -> error "Malformed struct clingo_ast_body_literal_t"
    poke p d = case d of
        AstBodyLiteral l s x -> do
            (#poke clingo_ast_body_literal_t, location) p l
            (#poke clingo_ast_body_literal_t, sign) p s
            (#poke clingo_ast_body_literal_t, type) p (AstBodyLiteralTypeLiteral :: AstBodyLiteralType)
            (#poke clingo_ast_body_literal_t, literal) p x
        AstBodyConditional l x -> do
            (#poke clingo_ast_body_literal_t, location) p l
            (#poke clingo_ast_body_literal_t, type) p (AstBodyLiteralTypeConditional :: AstBodyLiteralType)
            (#poke clingo_ast_body_literal_t, conditional) p x
        AstBodyLitAggregate l s x -> do
            (#poke clingo_ast_body_literal_t, location) p l
            (#poke clingo_ast_body_literal_t, sign) p s
            (#poke clingo_ast_body_literal_t, type) p (AstBodyLiteralTypeAggregate :: AstBodyLiteralType)
            (#poke clingo_ast_body_literal_t, aggregate) p x
        AstBodyBodyAggregate l s x -> do
            (#poke clingo_ast_body_literal_t, location) p l
            (#poke clingo_ast_body_literal_t, sign) p s
            (#poke clingo_ast_body_literal_t, type) p (AstBodyLiteralTypeBodyAggregate :: AstBodyLiteralType)
            (#poke clingo_ast_body_literal_t, body_aggregate) p x
        AstBodyTheoryAtom l s x -> do
            (#poke clingo_ast_body_literal_t, location) p l
            (#poke clingo_ast_body_literal_t, sign) p s
            (#poke clingo_ast_body_literal_t, type) p (AstBodyLiteralTypeTheoryAtom :: AstBodyLiteralType)
            (#poke clingo_ast_body_literal_t, theory_atom) p x
        AstBodyDisjoint l s x -> do
            (#poke clingo_ast_body_literal_t, location) p l
            (#poke clingo_ast_body_literal_t, sign) p s
            (#poke clingo_ast_body_literal_t, type) p (AstBodyLiteralTypeDisjoint :: AstBodyLiteralType)
            (#poke clingo_ast_body_literal_t, disjoint) p x

data AstTheoryOperatorDefinition = AstTheoryOperatorDefinition Location 
                                   CString CUInt AstTheoryOperatorType
    deriving (Eq, Show)

instance Storable AstTheoryOperatorDefinition where
    sizeOf _ = #{size clingo_ast_theory_operator_definition_t}
    alignment = sizeOf
    peek p = AstTheoryOperatorDefinition 
        <$> (#{peek clingo_ast_theory_operator_definition_t, location} p)
        <*> (#{peek clingo_ast_theory_operator_definition_t, name} p)
        <*> (#{peek clingo_ast_theory_operator_definition_t, priority} p)
        <*> (#{peek clingo_ast_theory_operator_definition_t, type} p)
    poke p (AstTheoryOperatorDefinition a b c d) = do
        (#poke clingo_ast_theory_operator_definition_t, location) p a
        (#poke clingo_ast_theory_operator_definition_t, name) p b
        (#poke clingo_ast_theory_operator_definition_t, priority) p c
        (#poke clingo_ast_theory_operator_definition_t, type) p d
    
data AstTheoryTermDefinition = AstTheoryTermDefinition Location CString
                               (Ptr AstTheoryOperatorDefinition) CSize
    deriving (Eq, Show)

instance Storable AstTheoryTermDefinition where
    sizeOf _ = #{size clingo_ast_theory_term_definition_t}
    alignment = sizeOf
    peek p = AstTheoryTermDefinition 
        <$> (#{peek clingo_ast_theory_term_definition_t, location} p)
        <*> (#{peek clingo_ast_theory_term_definition_t, name} p)
        <*> (#{peek clingo_ast_theory_term_definition_t, operators} p)
        <*> (#{peek clingo_ast_theory_term_definition_t, size} p)
    poke p (AstTheoryTermDefinition a b c d) = do
        (#poke clingo_ast_theory_term_definition_t, location) p a
        (#poke clingo_ast_theory_term_definition_t, name) p b
        (#poke clingo_ast_theory_term_definition_t, operators) p c
        (#poke clingo_ast_theory_term_definition_t, size) p d
    
data AstTheoryGuardDefinition = AstTheoryGuardDefinition CString (Ptr CString) 
                                CSize
    deriving (Eq, Show)

instance Storable AstTheoryGuardDefinition where
    sizeOf _ = #{size clingo_ast_theory_guard_definition_t}
    alignment = sizeOf
    peek p = AstTheoryGuardDefinition 
        <$> (#{peek clingo_ast_theory_guard_definition_t, term} p)
        <*> (#{peek clingo_ast_theory_guard_definition_t, operators} p)
        <*> (#{peek clingo_ast_theory_guard_definition_t, size} p)
    poke p (AstTheoryGuardDefinition a b c) = do
        (#poke clingo_ast_theory_guard_definition_t, term) p a
        (#poke clingo_ast_theory_guard_definition_t, operators) p b
        (#poke clingo_ast_theory_guard_definition_t, size) p c

data AstTheoryAtomDefinition = AstTheoryAtomDefinition Location 
                               AstTheoryAtomDefType CString CUInt CString
                               (Ptr AstTheoryGuardDefinition)
    deriving (Eq, Show)

instance Storable AstTheoryAtomDefinition where
    sizeOf _ = #{size clingo_ast_theory_atom_definition_t}
    alignment = sizeOf
    peek p = AstTheoryAtomDefinition 
        <$> (#{peek clingo_ast_theory_atom_definition_t, location} p)
        <*> (#{peek clingo_ast_theory_atom_definition_t, type} p)
        <*> (#{peek clingo_ast_theory_atom_definition_t, name} p)
        <*> (#{peek clingo_ast_theory_atom_definition_t, arity} p)
        <*> (#{peek clingo_ast_theory_atom_definition_t, elements} p)
        <*> (#{peek clingo_ast_theory_atom_definition_t, guard} p)
    poke p (AstTheoryAtomDefinition a b c d e f) = do
        (#poke clingo_ast_theory_atom_definition_t, location) p a
        (#poke clingo_ast_theory_atom_definition_t, type) p b
        (#poke clingo_ast_theory_atom_definition_t, name) p c
        (#poke clingo_ast_theory_atom_definition_t, arity) p d
        (#poke clingo_ast_theory_atom_definition_t, elements) p e
        (#poke clingo_ast_theory_atom_definition_t, guard) p f

data AstTheoryDefinition = AstTheoryDefinition CString 
                           (Ptr AstTheoryTermDefinition) CSize
                           (Ptr AstTheoryAtomDefinition) CSize
    deriving (Eq, Show)

instance Storable AstTheoryDefinition where
    sizeOf _ = #{size clingo_ast_theory_definition_t}
    alignment = sizeOf
    peek p = AstTheoryDefinition 
        <$> (#{peek clingo_ast_theory_definition_t, name} p)
        <*> (#{peek clingo_ast_theory_definition_t, terms} p)
        <*> (#{peek clingo_ast_theory_definition_t, terms_size} p)
        <*> (#{peek clingo_ast_theory_definition_t, atoms} p)
        <*> (#{peek clingo_ast_theory_definition_t, atoms_size} p)
    poke p (AstTheoryDefinition a b c d e) = do
        (#poke clingo_ast_theory_definition_t, name) p a
        (#poke clingo_ast_theory_definition_t, terms) p b
        (#poke clingo_ast_theory_definition_t, terms_size) p c
        (#poke clingo_ast_theory_definition_t, atoms) p d
        (#poke clingo_ast_theory_definition_t, atoms_size) p e
    
data AstRule = AstRule AstHeadLiteral (Ptr AstBodyLiteral) CSize
    deriving (Eq, Show)

instance Storable AstRule where
    sizeOf _ = #{size clingo_ast_rule_t}
    alignment = sizeOf
    peek p = AstRule 
        <$> (#{peek clingo_ast_rule_t, head} p)
        <*> (#{peek clingo_ast_rule_t, body} p)
        <*> (#{peek clingo_ast_rule_t, size} p)
    poke p (AstRule a b c) = do
        (#poke clingo_ast_rule_t, head) p a
        (#poke clingo_ast_rule_t, body) p b
        (#poke clingo_ast_rule_t, size) p c
    
data AstDefinition = AstDefinition CString AstTerm CBool
    deriving (Eq, Show)

instance Storable AstDefinition where
    sizeOf _ = #{size clingo_ast_definition_t}
    alignment = sizeOf
    peek p = AstDefinition 
        <$> (#{peek clingo_ast_definition_t, name} p)
        <*> (#{peek clingo_ast_definition_t, value} p)
        <*> (#{peek clingo_ast_definition_t, is_default} p)
    poke p (AstDefinition a b c) = do
        (#poke clingo_ast_definition_t, name) p a
        (#poke clingo_ast_definition_t, value) p b
        (#poke clingo_ast_definition_t, is_default) p c
    
data AstShowSignature = AstShowSignature Signature CBool
    deriving (Eq, Show)

instance Storable AstShowSignature where
    sizeOf _ = #{size clingo_ast_show_signature_t}
    alignment = sizeOf
    peek p = AstShowSignature 
        <$> (#{peek clingo_ast_show_signature_t, signature} p)
        <*> (#{peek clingo_ast_show_signature_t, csp} p)
    poke p (AstShowSignature a b) = do
        (#poke clingo_ast_show_signature_t, signature) p a
        (#poke clingo_ast_show_signature_t, csp) p b

data AstShowTerm = AstShowTerm AstTerm (Ptr AstBodyLiteral) CSize CBool
    deriving (Eq, Show)

instance Storable AstShowTerm where
    sizeOf _ = #{size clingo_ast_show_term_t}
    alignment = sizeOf
    peek p = AstShowTerm 
        <$> (#{peek clingo_ast_show_term_t, term} p)
        <*> (#{peek clingo_ast_show_term_t, body} p)
        <*> (#{peek clingo_ast_show_term_t, size} p)
        <*> (#{peek clingo_ast_show_term_t, csp} p)
    poke p (AstShowTerm a b c d) = do
        (#poke clingo_ast_show_term_t, term) p a
        (#poke clingo_ast_show_term_t, body) p b
        (#poke clingo_ast_show_term_t, size) p c
        (#poke clingo_ast_show_term_t, csp) p d
    
data AstMinimize = AstMinimize AstTerm AstTerm (Ptr AstTerm) CSize 
                   (Ptr AstBodyLiteral) CSize
    deriving (Eq, Show)

instance Storable AstMinimize where
    sizeOf _ = #{size clingo_ast_minimize_t}
    alignment = sizeOf
    peek p = AstMinimize 
        <$> (#{peek clingo_ast_minimize_t, weight} p)
        <*> (#{peek clingo_ast_minimize_t, priority} p)
        <*> (#{peek clingo_ast_minimize_t, tuple} p)
        <*> (#{peek clingo_ast_minimize_t, tuple_size} p)
        <*> (#{peek clingo_ast_minimize_t, body} p)
        <*> (#{peek clingo_ast_minimize_t, body_size} p)
    poke p (AstMinimize a b c d e f) = do
        (#poke clingo_ast_minimize_t, weight) p a
        (#poke clingo_ast_minimize_t, priority) p b
        (#poke clingo_ast_minimize_t, tuple) p c
        (#poke clingo_ast_minimize_t, tuple_size) p d
        (#poke clingo_ast_minimize_t, body) p e
        (#poke clingo_ast_minimize_t, body_size) p f

data AstScript = AstScript AstScriptType CString
    deriving (Eq, Show)

instance Storable AstScript where
    sizeOf _ = #{size clingo_ast_script_t}
    alignment = sizeOf
    peek p = AstScript 
        <$> (#{peek clingo_ast_script_t, type} p)
        <*> (#{peek clingo_ast_script_t, code} p)
    poke p (AstScript a b) = do
        (#poke clingo_ast_script_t, type) p a
        (#poke clingo_ast_script_t, code) p b

data AstProgram = AstProgram CString (Ptr AstId) CSize
    deriving (Eq, Show)

instance Storable AstProgram where
    sizeOf _ = #{size clingo_ast_program_t}
    alignment = sizeOf
    peek p = AstProgram 
        <$> (#{peek clingo_ast_program_t, name} p)
        <*> (#{peek clingo_ast_program_t, parameters} p)
        <*> (#{peek clingo_ast_program_t, size} p)
    poke p (AstProgram a b c) = do
        (#poke clingo_ast_program_t, name) p a
        (#poke clingo_ast_program_t, parameters) p b
        (#poke clingo_ast_program_t, size) p c
    
data AstExternal = AstExternal AstTerm (Ptr AstBodyLiteral) CSize
    deriving (Eq, Show)

instance Storable AstExternal where
    sizeOf _ = #{size clingo_ast_external_t}
    alignment = sizeOf
    peek p = AstExternal 
        <$> (#{peek clingo_ast_external_t, atom} p)
        <*> (#{peek clingo_ast_external_t, body} p)
        <*> (#{peek clingo_ast_external_t, size} p)
    poke p (AstExternal a b c) = do
        (#poke clingo_ast_external_t, atom) p a
        (#poke clingo_ast_external_t, body) p b
        (#poke clingo_ast_external_t, size) p c

data AstEdge = AstEdge AstTerm AstTerm (Ptr AstBodyLiteral) CSize
    deriving (Eq, Show)

instance Storable AstEdge where
    sizeOf _ = #{size clingo_ast_edge_t}
    alignment = sizeOf
    peek p = AstEdge 
        <$> (#{peek clingo_ast_edge_t, u} p)
        <*> (#{peek clingo_ast_edge_t, v} p)
        <*> (#{peek clingo_ast_edge_t, body} p)
        <*> (#{peek clingo_ast_edge_t, size} p)
    poke p (AstEdge a b c d) = do
        (#poke clingo_ast_edge_t, u) p a
        (#poke clingo_ast_edge_t, v) p b
        (#poke clingo_ast_edge_t, body) p c
        (#poke clingo_ast_edge_t, size) p d
    
data AstHeuristic = AstHeuristic AstTerm (Ptr AstBodyLiteral) CSize 
                    AstTerm AstTerm AstTerm
    deriving (Eq, Show)

instance Storable AstHeuristic where
    sizeOf _ = #{size clingo_ast_heuristic_t}
    alignment = sizeOf
    peek p = AstHeuristic 
        <$> (#{peek clingo_ast_heuristic_t, atom} p)
        <*> (#{peek clingo_ast_heuristic_t, body} p)
        <*> (#{peek clingo_ast_heuristic_t, size} p)
        <*> (#{peek clingo_ast_heuristic_t, bias} p)
        <*> (#{peek clingo_ast_heuristic_t, priority} p)
        <*> (#{peek clingo_ast_heuristic_t, modifier} p)
    poke p (AstHeuristic a b c d e f) = do
        (#poke clingo_ast_heuristic_t, atom) p a
        (#poke clingo_ast_heuristic_t, body) p b
        (#poke clingo_ast_heuristic_t, size) p c
        (#poke clingo_ast_heuristic_t, bias) p d
        (#poke clingo_ast_heuristic_t, priority) p e
        (#poke clingo_ast_heuristic_t, modifier) p f

data AstProject = AstProject AstTerm (Ptr AstBodyLiteral) CSize
    deriving (Eq, Show)

instance Storable AstProject where
    sizeOf _ = #{size clingo_ast_project_t}
    alignment = sizeOf
    peek p = AstProject
        <$> (#{peek clingo_ast_project_t, atom} p)
        <*> (#{peek clingo_ast_project_t, body} p)
        <*> (#{peek clingo_ast_project_t, size} p)
    poke p (AstProject a b c) = do
        (#poke clingo_ast_project_t, atom) p a
        (#poke clingo_ast_project_t, body) p b
        (#poke clingo_ast_project_t, size) p c
    
data AstStatement = AstStmtRule Location (Ptr AstRule)
                  | AstStmtDefinition Location (Ptr AstDefinition)
                  | AstStmtShowSignature Location (Ptr AstShowSignature)
                  | AstStmtShowTerm Location (Ptr AstShowTerm)
                  | AstStmtMinimize Location (Ptr AstMinimize)
                  | AstStmtScript Location (Ptr AstScript)
                  | AstStmtProgram Location (Ptr AstProgram)
                  | AstStmtExternal Location (Ptr AstExternal)
                  | AstStmtEdge Location (Ptr AstEdge)
                  | AstStmtHeuristic Location (Ptr AstHeuristic)
                  | AstStmtProject Location (Ptr AstProject)
                  | AstStmtSignature Location Signature
                  | AstStmtTheoryDefn Location (Ptr AstTheoryDefinition)
    deriving (Eq, Show)

instance Storable AstStatement where
    sizeOf _ = #{size clingo_ast_unary_operation_t}
    alignment = sizeOf
    peek p = do
        loc <- (#{peek clingo_ast_statement_t, location} p)
        typ :: AstStatementType <- (#{peek clingo_ast_statement_t, type} p)
        case typ of
            AstStatementTypeRule -> do
                payload <- (#{peek clingo_ast_statement_t, rule} p)
                pure $! AstStmtRule loc payload
            AstStatementTypeConst -> do
                payload <- (#{peek clingo_ast_statement_t, definition} p)
                pure $! AstStmtDefinition loc payload
            AstStatementTypeShowSignature -> do
                payload <- (#{peek clingo_ast_statement_t, show_signature} p)
                pure $! AstStmtShowSignature loc payload
            AstStatementTypeShowTerm -> do
                payload <- (#{peek clingo_ast_statement_t, show_term} p)
                pure $! AstStmtShowTerm loc payload
            AstStatementTypeMinimize -> do
                payload <- (#{peek clingo_ast_statement_t, minimize} p)
                pure $! AstStmtMinimize loc payload
            AstStatementTypeScript -> do
                payload <- (#{peek clingo_ast_statement_t, script} p)
                pure $! AstStmtScript loc payload
            AstStatementTypeProgram -> do
                payload <- (#{peek clingo_ast_statement_t, program} p)
                pure $! AstStmtProgram loc payload
            AstStatementTypeExternal -> do
                payload <- (#{peek clingo_ast_statement_t, external} p)
                pure $! AstStmtExternal loc payload
            AstStatementTypeEdge -> do
                payload <- (#{peek clingo_ast_statement_t, edge} p)
                pure $! AstStmtEdge loc payload
            AstStatementTypeHeuristic -> do
                payload <- (#{peek clingo_ast_statement_t, heuristic} p)
                pure $! AstStmtHeuristic loc payload
            AstStatementTypeProjectAtom -> do
                payload <- (#{peek clingo_ast_statement_t, project_atom} p)
                pure $! AstStmtProject loc payload
            AstStatementTypeProjectAtomSignature -> do
                payload <- (#{peek clingo_ast_statement_t, project_signature} p)
                pure $! AstStmtSignature loc payload
            AstStatementTypeTheoryDefinition -> do
                payload <- (#{peek clingo_ast_statement_t, theory_definition} p)
                pure $! AstStmtTheoryDefn loc payload
            _ -> error "Malformed struct clingo_ast_statement_t"
    poke p d = case d of
        AstStmtRule l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeRule :: AstStatementType)
            (#poke clingo_ast_statement_t, rule) p x
        AstStmtDefinition l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeConst :: AstStatementType)
            (#poke clingo_ast_statement_t, definition) p x
        AstStmtShowSignature l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeShowSignature :: AstStatementType)
            (#poke clingo_ast_statement_t, show_signature) p x
        AstStmtShowTerm l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeShowTerm :: AstStatementType)
            (#poke clingo_ast_statement_t, show_term) p x
        AstStmtMinimize l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeMinimize :: AstStatementType)
            (#poke clingo_ast_statement_t, minimize) p x
        AstStmtScript l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeScript :: AstStatementType)
            (#poke clingo_ast_statement_t, script) p x
        AstStmtProgram l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeProgram :: AstStatementType)
            (#poke clingo_ast_statement_t, program) p x
        AstStmtExternal l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeExternal :: AstStatementType)
            (#poke clingo_ast_statement_t, external) p x
        AstStmtEdge l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeEdge :: AstStatementType)
            (#poke clingo_ast_statement_t, edge) p x
        AstStmtHeuristic l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeHeuristic :: AstStatementType)
            (#poke clingo_ast_statement_t, heuristic) p x
        AstStmtProject l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeProjectAtom :: AstStatementType)
            (#poke clingo_ast_statement_t, project_atom) p x
        AstStmtSignature l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeProjectAtomSignature :: AstStatementType)
            (#poke clingo_ast_statement_t, project_signature) p x
        AstStmtTheoryDefn l x -> do
            (#poke clingo_ast_statement_t, location) p l
            (#poke clingo_ast_statement_t, type) p (AstStatementTypeTheoryDefinition :: AstStatementType)
            (#poke clingo_ast_statement_t, theory_definition) p x

foreign import ccall "clingo.h clingo_parse_program" parseProgramFFI ::
    CString -> FunPtr (CallbackAST a) -> Ptr a -> FunPtr (Logger b) -> Ptr b 
            -> CUInt -> IO CBool

parseProgram :: MonadIO m
             => CString -> FunPtr (CallbackAST a) -> Ptr a -> FunPtr (Logger b) 
             -> Ptr b -> CUInt -> m CBool
parseProgram a b c d e f = liftIO $ parseProgramFFI a b c d e f
