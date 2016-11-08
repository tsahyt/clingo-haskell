module Clingo.AST
(
)
where

import Data.Text (Text, unpack)
import Foreign hiding (Pool)
import Foreign.C

import Clingo.Internal.Types (Location, rawLocation, fromRawLocation)
import Clingo.Raw.AST

import System.IO.Unsafe

-- TODOs
data Sign = Positive | Negative
data Signature
data Symbol

data UnaryOperation = UnaryOperation UnaryOperator Term

rawUnaryOperation :: UnaryOperation -> AstUnaryOperation
rawUnaryOperation (UnaryOperation o t) =
    AstUnaryOperation (rawUnaryOperator o) (rawTerm t)

fromRawUnaryOperation :: UnaryOperation -> AstUnaryOperation
fromRawUnaryOperation = undefined

data UnaryOperator = UnaryMinus | Negation | Absolute

rawUnaryOperator :: UnaryOperator -> AstUnaryOperator
rawUnaryOperator o = case o of
    UnaryMinus -> AstUnaryOperatorMinus
    Negation -> AstUnaryOperatorNegation
    Absolute -> AstUnaryOperatorAbsolute

fromRawUnaryOperator :: UnaryOperator -> AstUnaryOperator
fromRawUnaryOperator = undefined

data BinaryOperation = BinaryOperation BinaryOperator Term Term

rawBinaryOperation :: BinaryOperation -> AstBinaryOperation
rawBinaryOperation (BinaryOperation o l r) =
    AstBinaryOperation (rawBinaryOperator o) (rawTerm l) (rawTerm r)

fromRawBinaryOperation :: BinaryOperation -> AstBinaryOperation
fromRawBinaryOperation = undefined

data BinaryOperator = Xor | Or | And | Plus | Minus | Mult | Div | Mod

rawBinaryOperator :: BinaryOperator -> AstBinaryOperator
rawBinaryOperator o = case o of
    Xor -> AstBinaryOperatorXor
    Or -> AstBinaryOperatorOr
    And -> AstBinaryOperatorAnd
    Plus -> AstBinaryOperatorPlus
    Minus -> AstBinaryOperatorMinus
    Mult -> AstBinaryOperatorMultiplication
    Div -> AstBinaryOperatorDivision
    Mod -> AstBinaryOperatorModulo

fromRawBinaryOperator :: BinaryOperator -> AstBinaryOperator
fromRawBinaryOperator = undefined

data Interval = Interval Term Term

rawInterval :: Interval -> AstInterval
rawInterval (Interval a b) = AstInterval (rawTerm a) (rawTerm b)

fromRawInterval :: Interval -> AstInterval
fromRawInterval = undefined

data Function = Function Text [Term]

rawFunction :: Function -> AstFunction
rawFunction (Function n ts) = unsafePerformIO $ do
    n'  <- newCString (unpack n)
    ts' <- newArray (map rawTerm ts)
    return $ AstFunction n' ts' (fromIntegral . length $ ts)

fromRawFunction :: Function -> AstFunction
fromRawFunction = undefined

data Pool = Pool [Term]

rawPool :: Pool -> AstPool
rawPool (Pool ts) = unsafePerformIO $ do
    ts' <- newArray (map rawTerm ts)
    return $ AstPool ts' (fromIntegral . length $ ts)

fromRawPool :: Pool -> AstPool
fromRawPool = undefined

data Term 
    = TermSymbol Location Symbol
    | TermVariable Location Text
    | TermUOp Location UnaryOperation
    | TermBOp Location BinaryOperation
    | TermInterval Location Interval
    | TermFunction Location Function
    | TermExtFunction Location Function
    | TermPool Location Pool

rawTerm :: Term -> AstTerm
rawTerm (TermSymbol l s) = AstTermSymbol (rawLocation l) undefined
rawTerm (TermVariable l n) = AstTermVariable (rawLocation l) undefined
rawTerm (TermUOp l u) = AstTermUOp (rawLocation l) (rawUnaryOperation u)
rawTerm (TermBOp l u) = AstTermBOp (rawLocation l) (rawBinaryOperation u)
rawTerm (TermInterval l i) = AstTermInterval (rawLocation l) (rawInterval i)
rawTerm (TermFunction l f) = AstTermFunction (rawLocation l) (rawFunction f)
rawTerm (TermExtFunction l f) = 
    AstTermExtFunction (rawLocation l) (rawFunction f)
rawTerm (TermPool l p) = AstTermPool (rawLocation l) (rawPool p)

fromRawTerm :: Term -> AstTerm
fromRawTerm = undefined

data CspProductTerm = CspProductTerm Location Term (Maybe Term)

data CspSumTerm = CspSumTerm Location [CspProductTerm]

data CspGuard = CspGuard ComparisonOperator CspSumTerm

data ComparisonOperator = GreaterThan | LessThan | LessEqual | GreaterEqual
                        | NotEqual | Equal

data CspLiteral = CspLiteral CspSumTerm [CspGuard]

data Identifier = Identifier Location Text

data Comparison = Comparison ComparisonOperator Term Term

data Literal
    = LiteralBool Location Sign Bool
    | LiteralTerm Location Sign Term
    | LiteralComp Location Sign Comparison
    | LiteralCSPL Location Sign CspLiteral

data AggregateGuard = AggregateGuard ComparisonOperator Term

data ConditionalLiteral = ConditionalLiteral Literal [Literal]

data Aggregate = Aggregate [ConditionalLiteral] 
                           (Maybe AggregateGuard) 
                           (Maybe AggregateGuard)

data BodyAggregateElement = BodyAggregateElement [Term] [Literal]

data BodyAggregate = BodyAggregate AggregateFunction [BodyAggregateElement] 
                                   (Maybe AggregateGuard) 
                                   (Maybe AggregateGuard)

data AggregateFunction = Count | Sum | Sump | Min | Max

data HeadAggregateElement = HeadAggregateElement [Term] ConditionalLiteral

data HeadAggregate = HeadAggregate [HeadAggregateElement]
                                   (Maybe AggregateGuard)
                                   (Maybe AggregateGuard)

data Disjunction = Disjunction [ConditionalLiteral]

data DisjointElement = DisjointElement Location [Term] CspSumTerm [Literal]

data Disjoint = Disjoint [DisjointElement]

data TheoryTermArray = TheoryTermArray [TheoryTerm]

data TheoryFunction = TheoryFunction Text [TheoryTerm]

data TheoryUnparsedTermElement = TheoryUnparsedTermElement [Text] TheoryTerm

data TheoryUnparsedTerm = TheoryUnparsedTerm [TheoryUnparsedTermElement]

data TheoryTerm
    = TheoryTermSymbol Location Symbol
    | TheoryTermVariable Location Text
    | TheoryTermTuple Location TheoryTermArray
    | TheoryTermList Location TheoryTermArray
    | TheoryTermSet Location TheoryTermArray
    | TheoryTermFunction Location TheoryFunction
    | TheoryTermUnparsed Location TheoryUnparsedTerm

data TheoryAtomElement = TheoryAtomElement [TheoryTerm] [Literal]

data TheoryGuard = TheoryGuard Text TheoryTerm

data TheoryAtom = TheoryAtom Term [TheoryAtomElement] TheoryGuard

data HeadLiteral
    = HeadLiteral Location Literal
    | HeadDisjunction Location Disjunction
    | HeadLitAggregate Location Aggregate
    | HeadTheoryAtom Location TheoryAtom

data BodyLiteral
    = BodyLiteral Location Sign Literal
    | BodyConditional Location ConditionalLiteral
    | BodyLitAggregate Location Sign Aggregate
    | BodyTheoryAtom Location Sign TheoryAtom
    | BodyDisjoint Location Sign Disjoint

data TheoryOperatorDefinition = 
    TheoryOperatorDefinition Location Text Int TheoryOperatorType

data TheoryOperatorType = Unary | BinLeft | BinRight

data TheoryTermDefinition =
    TheoryTermDefinition Location Text [TheoryOperatorDefinition]

data TheoryGuardDefinition =
    TheoryGuardDefinition Text [Text]

data TheoryAtomDefinition =
    TheoryAtomDefinition Location TheoryAtomDefinitionType 
                         Text Int Text TheoryGuardDefinition

data TheoryAtomDefinitionType = Head | Body | Any | Directive

data TheoryDefinition =
    TheoryDefinition Text [TheoryTermDefinition] [TheoryAtomDefinition]

data Rule = Rule HeadLiteral [BodyLiteral]

data Definition = Definition Text Term Bool

data ShowSignature = ShowSignature Signature Bool

data ShowTerm = ShowTerm Term [BodyLiteral] Bool

data Minimize = Minimize Term Term [Term] [BodyLiteral]

data Script = Script ScriptType Text

data ScriptType = Lua | Python

data Program = Program Text [Identifier]

data External = External Term [BodyLiteral]

data Edge = Edge Term Term [BodyLiteral]

data Heuristic = Heuristic Term [BodyLiteral] Term Term Term

data Project = Project Term [BodyLiteral]

data Statement
    = StmtRule Location Rule
    | StmtDefinition Location Definition
    | StmtShowSignature Location ShowSignature
    | StmtShowTerm Location ShowTerm
    | StmtMinimize Location Minimize
    | StmtScript Location Script
    | StmtProgram Location Program
    | StmtExternal Location External
    | StmtEdge Location Edge
    | StmtHeuristic Location Heuristic
    | StmtProject Location Project
    | StmtSignature Location Signature
    | StmtTheoryDefinition Location TheoryDefinition
