{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
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

data AstUnaryOperation = AstUnaryOperation AstUnaryOperator AstTerm
    deriving (Eq, Show)

data AstBinaryOperation = AstBinaryOperation AstBinaryOperator AstTerm AstTerm
    deriving (Eq, Show)

data AstInterval = AstInterval AstTerm AstTerm
    deriving (Eq, Show)

data AstFunction = AstFunction CString (Ptr AstTerm) CSize
    deriving (Eq, Show)

data AstPool = AstPool (Ptr AstTerm) CSize
    deriving (Eq, Show)

data AstTerm = AstTermSymbol Location Symbol
             | AstTermVariable Location CString
             | AstTermUOp Location AstUnaryOperation
             | AstTermBOp Location AstBinaryOperation
             | AstTermInterval Location AstInterval
             | AstTermFunction Location AstFunction
             | AstTermPool Location AstPool
    deriving (Eq, Show)

data AstCspProductTerm = AstCspProductTerm Location AstTerm (Ptr AstTerm)
    deriving (Eq, Show)

data AstCspSumTerm = AstCspSumTerm Location (Ptr AstCspProductTerm) CSize
    deriving (Eq, Show)

data AstCspGuard = AstCspGuard AstComparisonOperator AstCspSumTerm
    deriving (Eq, Show)

data AstCspLiteral = AstCspLiteral AstCspSumTerm (Ptr AstCspGuard) CSize
    deriving (Eq, Show)

data AstId = AstId Location CString
    deriving (Eq, Show)

data AstComparison = AstComparison AstComparisonOperator AstTerm AstTerm
    deriving (Eq, Show)

data AstLiteral = AstLiteralBool Location AstSign CBool
                | AstLiteralTerm Location AstSign AstTerm
                | AstLiteralComp Location AstSign AstComparison
                | AstLiteralCSPL Location AstSign AstCspLiteral
    deriving (Eq, Show)

data AstAggregateGuard = AstAggregateGuard AstComparisonOperator AstTerm
    deriving (Eq, Show)

data AstConditionalLiteral = AstConditionalLiteral AstLiteral 
                             (Ptr AstLiteral) CSize
    deriving (Eq, Show)

data AstAggregate = AstAggregate (Ptr AstConditionalLiteral) CSize
                    (Ptr AstAggregateGuard) (Ptr AstAggregateGuard)
    deriving (Eq, Show)

data AstBodyAggregateElement = AstBodyAggregateElement AstTerm CSize 
                               (Ptr AstLiteral) CSize
    deriving (Eq, Show)

data AstBodyAggregate = AstBodyAggregate AstAggregateFunction
                        (Ptr AstBodyAggregateElement) CSize
                        (Ptr AstAggregateGuard) (Ptr AstAggregateGuard)
    deriving (Eq, Show)

data AstHeadAggregateElement = AstHeadAggregateElement (Ptr AstTerm) CSize
                               AstConditionalLiteral
    deriving (Eq, Show)

data AstHeadAggregate = AstHeadAggregate AstAggregateFunction
                        (Ptr AstHeadAggregateElement) CSize
                        (Ptr AstAggregateGuard) (Ptr AstAggregateGuard)
    deriving (Eq, Show)

data AstDisjunction = AstDisjunction (Ptr AstConditionalLiteral) CSize
    deriving (Eq, Show)

data AstDisjointElement = AstDisjointElement Location (Ptr AstTerm) CSize
                          AstCspSumTerm (Ptr AstLiteral) CSize
    deriving (Eq, Show)

data AstDisjoint = AstDisjoint (Ptr AstDisjointElement) CSize
    deriving (Eq, Show)

data AstTheoryTermArray = AstTheoryTermArray (Ptr AstTheoryTerm) CSize
    deriving (Eq, Show)

data AstTheoryFunction = AstTheoryFunction CString (Ptr AstTheoryTerm) CSize
    deriving (Eq, Show)

data AstTheoryUnparsedTermElement = AstTheoryUnparsedTermElement 
                                    (Ptr CString) CSize AstTheoryTerm
    deriving (Eq, Show)

data AstTheoryUnparsedTerm = AstTheoryUnparsedTerm 
                             (Ptr AstTheoryUnparsedTermElement) CSize
    deriving (Eq, Show)

data AstTheoryTerm = AstTheoryTermSymbol Location Symbol
                   | AstTheoryTermVariable Location CString
                   | AstTheoryTermTuple Location (Ptr AstTheoryTermArray)
                   | AstTheoryTermList Location (Ptr AstTheoryTermArray)
                   | AstTheoryTermSet Location (Ptr AstTheoryTermArray)
                   | AstTheoryTermFunction Location (Ptr AstTheoryFunction)
                   | AstTheoryTermUnparsed Location (Ptr AstTheoryUnparsedTerm)
    deriving (Eq, Show)

data AstTheoryAtomElement = AstTheoryAtomElement (Ptr AstTheoryTerm) CSize
                            (Ptr AstLiteral) CSize
    deriving (Eq, Show)

data AstTheoryGuard = AstTheoryGuard CString AstTheoryTerm
    deriving (Eq, Show)

data AstTheoryAtom = AstTheoryAtom AstTerm (Ptr AstTheoryAtomElement) CSize
                     AstTheoryGuard
    deriving (Eq, Show)

data AstHeadLiteral = AstHeadLiteral Location (Ptr AstLiteral)
                    | AstHeadDisjunction Location (Ptr AstDisjunction)
                    | AstHeadLitAggregate Location (Ptr AstAggregate)
                    | AstHeadTheoryAtom Location (Ptr AstTheoryAtom)
    deriving (Eq, Show)

data AstBodyLiteral 
    = AstBodyLiteral Location AstSign (Ptr AstLiteral)
    | AstBodyConditional Location AstSign (Ptr AstConditionalLiteral)
    | AstBodyLitAggregate Location AstSign (Ptr AstBodyAggregate)
    | AstBodyTheoryAtom Location AstSign (Ptr AstTheoryAtom)
    | AstBodyDisjoint Location AstSign (Ptr AstDisjoint)
    deriving (Eq, Show)

data AstTheoryOperatorDefinition = AstTheoryOperatorDefinition Location 
                                   CString CUInt AstTheoryOperatorType
    deriving (Eq, Show)

data AstTheoryTermDefinition = AstTheoryTermDefinition Location CString
                               (Ptr AstTheoryOperatorDefinition) CSize
    deriving (Eq, Show)

data AstTheoryGuardDefinition = AstTheoryGuardDefinition CString (Ptr CString) 
                                CSize
    deriving (Eq, Show)

data AstTheoryAtomDefinition = AstTheoryAtomDefinition Location 
                               AstTheoryAtomDefType CString CUInt CString
                               (Ptr AstTheoryGuardDefinition)
    deriving (Eq, Show)

data AstTheoryDefinition = AstTheoryDefinition CString 
                           (Ptr AstTheoryTermDefinition) CSize
                           (Ptr AstTheoryAtomDefinition) CSize
    deriving (Eq, Show)

data AstRule = AstRule AstHeadLiteral (Ptr AstBodyLiteral) CSize
    deriving (Eq, Show)

data AstDefinition = AstDefinition CString AstTerm CBool
    deriving (Eq, Show)

data AstShowSignature = AstShowSignature Signature CBool
    deriving (Eq, Show)

data AstShowTerm = AstShowTerm AstTerm (Ptr AstBodyLiteral) CSize CBool
    deriving (Eq, Show)

data AstMinimize = AstMinimize AstTerm AstTerm (Ptr AstTerm) CSize 
                   (Ptr AstBodyLiteral) CSize
    deriving (Eq, Show)

data AstScript = AstScript AstScriptType CString
    deriving (Eq, Show)

data AstProgram = AstProgram CString (Ptr AstId) CSize
    deriving (Eq, Show)

data AstExternal = AstExternal AstTerm (Ptr AstBodyLiteral) CSize
    deriving (Eq, Show)

data AstEdge = AstEdge AstTerm AstTerm (Ptr AstBodyLiteral) CSize
    deriving (Eq, Show)

data AstHeuristic = AstHeuristic AstTerm (Ptr AstBodyLiteral) CSize 
                    AstTerm AstTerm AstTerm
    deriving (Eq, Show)

data AstProject = AstProject AstTerm (Ptr AstBodyLiteral) CSize
    deriving (Eq, Show)

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
                  | AstStmtProject (Ptr AstProject)
                  | AstStmtSignature Signature
                  | AstStmtTheoryDefn (Ptr AstTheoryDefinition)
    deriving (Eq, Show)

foreign import ccall "clingo.h clingo_parse_program" parseProgramFFI ::
    CString -> FunPtr (CallbackAST a) -> Ptr a -> FunPtr (Logger b) -> Ptr b 
            -> CUInt -> IO CBool

parseProgram :: MonadIO m
             => CString -> FunPtr (CallbackAST a) -> Ptr a -> FunPtr (Logger b) 
             -> Ptr b -> CUInt -> m CBool
parseProgram a b c d e f = liftIO $ parseProgramFFI a b c d e f
