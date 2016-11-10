module Clingo.AST where

import Control.Monad
import Data.Text (Text, unpack)
import Numeric.Natural
import Foreign hiding (Pool, freePool)
import Foreign.C

import Clingo.Internal.Types (Location, rawLocation, freeRawLocation, 
                              fromRawLocation)
import Clingo.Raw.AST

freeArray :: Storable a => Ptr a -> CSize -> (a -> IO ()) -> IO ()
freeArray p n f = unless (p == nullPtr) $ do
    p' <- peekArray (fromIntegral n) p
    mapM_ f p'
    free p

freeIndirection :: Storable a => Ptr a -> (a -> IO ()) -> IO ()
freeIndirection p f = unless (p == nullPtr) $ do
    p' <- peek p
    f p'
    free p

-- TODOs
data Sign = NoSign | NegationSign | DoubleNegationSign

rawSign :: Sign -> AstSign
rawSign s = case s of
    NoSign -> AstSignNone
    NegationSign -> AstSignNegation
    DoubleNegationSign -> AstSignDoubleNegation

fromRawSign :: AstSign -> Sign
fromRawSign s = case s of
    AstSignNone -> NoSign
    AstSignNegation -> NegationSign
    AstSignDoubleNegation -> DoubleNegationSign
    _ -> error "Invalid clingo_ast_sign_t"

data Signature
data Symbol

data UnaryOperation = UnaryOperation UnaryOperator Term

rawUnaryOperation :: UnaryOperation -> IO AstUnaryOperation
rawUnaryOperation (UnaryOperation o t) = 
    AstUnaryOperation <$> pure (rawUnaryOperator o) <*> rawTerm t

freeUnaryOperation :: AstUnaryOperation -> IO ()
freeUnaryOperation (AstUnaryOperation _ t) = freeTerm t

fromRawUnaryOperation :: AstUnaryOperation -> IO UnaryOperation
fromRawUnaryOperation = undefined

data UnaryOperator = UnaryMinus | Negation | Absolute

rawUnaryOperator :: UnaryOperator -> AstUnaryOperator
rawUnaryOperator o = case o of
    UnaryMinus -> AstUnaryOperatorMinus
    Negation -> AstUnaryOperatorNegation
    Absolute -> AstUnaryOperatorAbsolute

fromRawUnaryOperator :: AstUnaryOperator -> UnaryOperator
fromRawUnaryOperator o = case o of
    AstUnaryOperatorMinus -> UnaryMinus
    AstUnaryOperatorNegation -> Negation
    AstUnaryOperatorAbsolute -> Absolute
    _ -> error "Invalid clingo_ast_unary_operator_t"

data BinaryOperation = BinaryOperation BinaryOperator Term Term

rawBinaryOperation :: BinaryOperation -> IO AstBinaryOperation
rawBinaryOperation (BinaryOperation o l r) =
    AstBinaryOperation <$> pure (rawBinaryOperator o) 
                       <*> rawTerm l <*> rawTerm r

freeBinaryOperation :: AstBinaryOperation -> IO ()
freeBinaryOperation (AstBinaryOperation _ a b) = freeTerm a >> freeTerm b

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

fromRawBinaryOperator :: AstBinaryOperator -> BinaryOperator
fromRawBinaryOperator o = case o of
    AstBinaryOperatorXor -> Xor 
    AstBinaryOperatorOr -> Or 
    AstBinaryOperatorAnd -> And 
    AstBinaryOperatorPlus -> Plus 
    AstBinaryOperatorMinus -> Minus 
    AstBinaryOperatorMultiplication -> Mult 
    AstBinaryOperatorDivision -> Div 
    AstBinaryOperatorModulo -> Mod 
    _ -> error "Invalid clingo_ast_binary_operator_t"

data Interval = Interval Term Term

rawInterval :: Interval -> IO AstInterval
rawInterval (Interval a b) = AstInterval <$> rawTerm a <*> rawTerm b

freeInterval :: AstInterval -> IO ()
freeInterval (AstInterval a b) = freeTerm a >> freeTerm b

fromRawInterval :: Interval -> AstInterval
fromRawInterval = undefined

data Function = Function Text [Term]

rawFunction :: Function -> IO AstFunction
rawFunction (Function n ts) = do
    n'  <- newCString (unpack n)
    ts' <- newArray =<< mapM rawTerm ts
    return $ AstFunction n' ts' (fromIntegral . length $ ts)

freeFunction :: AstFunction -> IO ()
freeFunction (AstFunction s ts n) = do
    free s 
    freeArray ts n freeTerm

fromRawFunction :: Function -> AstFunction
fromRawFunction = undefined

data Pool = Pool [Term]

rawPool :: Pool -> IO AstPool
rawPool (Pool ts) = do
    ts' <- newArray =<< mapM rawTerm ts
    return $ AstPool ts' (fromIntegral . length $ ts)

freePool :: AstPool -> IO ()
freePool (AstPool ts n) = freeArray ts n freeTerm

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

rawTerm :: Term -> IO AstTerm
rawTerm (TermSymbol l s) = AstTermSymbol <$> rawLocation l <*> undefined
rawTerm (TermVariable l n) = AstTermVariable <$> rawLocation l <*> undefined
rawTerm (TermUOp l u) = AstTermUOp <$> rawLocation l <*> rawUnaryOperation u
rawTerm (TermBOp l u) = AstTermBOp <$> rawLocation l <*> rawBinaryOperation u
rawTerm (TermInterval l i) = AstTermInterval <$> rawLocation l <*> rawInterval i
rawTerm (TermFunction l f) = AstTermFunction <$> rawLocation l <*> rawFunction f
rawTerm (TermExtFunction l f) =
    AstTermExtFunction <$> rawLocation l <*> rawFunction f
rawTerm (TermPool l p) = AstTermPool <$> rawLocation l <*> rawPool p

freeTerm :: AstTerm -> IO ()
freeTerm (AstTermSymbol l _) = freeRawLocation l
freeTerm (AstTermVariable l _) = freeRawLocation l
freeTerm (AstTermUOp l o) = freeRawLocation l >> freeUnaryOperation o
freeTerm (AstTermBOp l o) = freeRawLocation l >> freeBinaryOperation o
freeTerm (AstTermInterval l i) = freeRawLocation l >> freeInterval i
freeTerm (AstTermFunction l f) = freeRawLocation l >> freeFunction f
freeTerm (AstTermExtFunction l f) = freeRawLocation l >> freeFunction f
freeTerm (AstTermPool l p) = freeRawLocation l >> freePool p

fromRawTerm :: Term -> AstTerm
fromRawTerm = undefined

data CspProductTerm = CspProductTerm Location Term (Maybe Term)

rawCspProductTerm :: CspProductTerm -> IO AstCspProductTerm
rawCspProductTerm (CspProductTerm l t m) = AstCspProductTerm
    <$> rawLocation l
    <*> rawTerm t
    <*> maybe (return nullPtr) (new <=< rawTerm) m

freeCspProductTerm :: AstCspProductTerm -> IO ()
freeCspProductTerm (AstCspProductTerm l t p) = do
    freeRawLocation l
    freeTerm t
    freeIndirection p freeTerm

fromRawCspProductTerm :: CspProductTerm -> AstCspProductTerm
fromRawCspProductTerm = undefined

data CspSumTerm = CspSumTerm Location [CspProductTerm]

rawCspSumTerm :: CspSumTerm -> IO AstCspSumTerm
rawCspSumTerm (CspSumTerm l ts) = do
    l' <- rawLocation l
    ts' <- newArray =<< mapM rawCspProductTerm ts
    return $ AstCspSumTerm l' ts' (fromIntegral . length $ ts)

freeCspSumTerm :: AstCspSumTerm -> IO ()
freeCspSumTerm (AstCspSumTerm l ts n) = do
    freeRawLocation l
    freeArray ts n freeCspProductTerm

fromRawCspSumTerm :: CspSumTerm -> AstCspSumTerm
fromRawCspSumTerm = undefined

data CspGuard = CspGuard ComparisonOperator CspSumTerm

rawCspGuard :: CspGuard -> IO AstCspGuard
rawCspGuard (CspGuard o t) = AstCspGuard
    <$> pure (rawComparisonOperator o) <*> rawCspSumTerm t

freeCspGuard :: AstCspGuard -> IO ()
freeCspGuard (AstCspGuard _ t) = freeCspSumTerm t

fromRawCspGuard :: CspGuard -> AstCspGuard
fromRawCspGuard = undefined

data ComparisonOperator = GreaterThan | LessThan | LessEqual | GreaterEqual
                        | NotEqual | Equal

rawComparisonOperator :: ComparisonOperator -> AstComparisonOperator
rawComparisonOperator o = case o of
    GreaterThan -> AstComparisonOperatorGreaterThan
    LessThan -> AstComparisonOperatorLessThan
    LessEqual -> AstComparisonOperatorLessEqual
    GreaterEqual -> AstComparisonOperatorGreaterEqual
    NotEqual -> AstComparisonOperatorNotEqual
    Equal -> AstComparisonOperatorEqual

fromRawComparisonOperator :: ComparisonOperator -> AstComparisonOperator
fromRawComparisonOperator = undefined

data CspLiteral = CspLiteral CspSumTerm [CspGuard]

rawCspLiteral :: CspLiteral -> IO AstCspLiteral
rawCspLiteral (CspLiteral t gs) = do
    gs' <- newArray =<< mapM rawCspGuard gs
    t'  <- rawCspSumTerm t
    return $ AstCspLiteral t' gs' (fromIntegral . length $ gs)

freeCspLiteral :: AstCspLiteral -> IO ()
freeCspLiteral (AstCspLiteral t p n) = do
    freeCspSumTerm t
    freeArray p n freeCspGuard

fromRawCspLiteral :: CspLiteral -> AstCspLiteral
fromRawCspLiteral = undefined

data Identifier = Identifier Location Text

rawIdentifier :: Identifier -> IO AstId
rawIdentifier (Identifier l t) = do
    l' <- rawLocation l
    t' <- newCString (unpack t)
    return $ AstId l' t'

freeIdentifier :: AstId -> IO ()
freeIdentifier (AstId l t) = freeRawLocation l >> free t

fromRawIdentifier :: Identifier -> AstId
fromRawIdentifier = undefined

data Comparison = Comparison ComparisonOperator Term Term

rawComparison :: Comparison -> IO AstComparison
rawComparison (Comparison o a b) = AstComparison
    <$> pure (rawComparisonOperator o) <*> rawTerm a <*> rawTerm b

freeComparison :: AstComparison -> IO ()
freeComparison (AstComparison _ a b) = freeTerm a >> freeTerm b

fromRawComparison :: Comparison -> AstComparison
fromRawComparison = undefined

data Literal
    = LiteralBool Location Sign Bool
    | LiteralTerm Location Sign Term
    | LiteralComp Location Sign Comparison
    | LiteralCSPL Location Sign CspLiteral

rawLiteral :: Literal -> IO AstLiteral
rawLiteral (LiteralBool l s b) = AstLiteralBool 
    <$> rawLocation l <*> pure (rawSign s) <*> pure (fromBool b)
rawLiteral (LiteralTerm l s t) = AstLiteralTerm
    <$> rawLocation l <*> pure (rawSign s) <*> rawTerm t
rawLiteral (LiteralComp l s c) = AstLiteralComp
    <$> rawLocation l <*> pure (rawSign s) <*> rawComparison c
rawLiteral (LiteralCSPL l s x) = AstLiteralCSPL
    <$> rawLocation l <*> pure (rawSign s) <*> rawCspLiteral x

freeLiteral :: AstLiteral -> IO ()
freeLiteral (AstLiteralBool l _ _) = freeRawLocation l
freeLiteral (AstLiteralTerm l _ t) = freeRawLocation l >> freeTerm t
freeLiteral (AstLiteralComp l _ c) = freeRawLocation l >> freeComparison c
freeLiteral (AstLiteralCSPL l _ x) = freeRawLocation l >> freeCspLiteral x

fromRawLiteral :: Literal -> AstLiteral
fromRawLiteral = undefined

data AggregateGuard = AggregateGuard ComparisonOperator Term

rawAggregateGuard :: AggregateGuard -> IO AstAggregateGuard
rawAggregateGuard (AggregateGuard o t) = AstAggregateGuard
    <$> pure (rawComparisonOperator o) <*> rawTerm t

rawAggregateGuardM :: Maybe AggregateGuard -> IO (Ptr AstAggregateGuard)
rawAggregateGuardM Nothing = return nullPtr
rawAggregateGuardM (Just g) = new =<< rawAggregateGuard g

freeAggregateGuard :: AstAggregateGuard -> IO ()
freeAggregateGuard (AstAggregateGuard _ t) = freeTerm t

fromRawAggregateGuard :: AggregateGuard -> AstAggregateGuard
fromRawAggregateGuard = undefined

data ConditionalLiteral = ConditionalLiteral Literal [Literal]

rawConditionalLiteral :: ConditionalLiteral -> IO AstConditionalLiteral
rawConditionalLiteral (ConditionalLiteral l ls) = do
    l' <- rawLiteral l
    ls' <- newArray =<< mapM rawLiteral ls
    return $ AstConditionalLiteral l' ls' (fromIntegral . length $ ls)

freeConditionalLiteral :: AstConditionalLiteral -> IO ()
freeConditionalLiteral (AstConditionalLiteral l ls n) = do
    freeLiteral l
    freeArray ls n freeLiteral

fromRawConditionalLiteral :: ConditionalLiteral -> AstConditionalLiteral
fromRawConditionalLiteral = undefined

data Aggregate = Aggregate [ConditionalLiteral] 
                           (Maybe AggregateGuard) 
                           (Maybe AggregateGuard)

rawAggregate :: Aggregate -> IO AstAggregate
rawAggregate (Aggregate ls a b) = do
    ls' <- newArray =<< mapM rawConditionalLiteral ls
    a'  <- rawAggregateGuardM a
    b'  <- rawAggregateGuardM b
    return $ AstAggregate ls' (fromIntegral . length $ ls) a' b'

freeAggregate :: AstAggregate -> IO ()
freeAggregate (AstAggregate ls n a b) = do
    freeIndirection a freeAggregateGuard
    freeIndirection b freeAggregateGuard
    freeArray ls n freeConditionalLiteral

fromRawAggregate :: Aggregate -> AstAggregate
fromRawAggregate = undefined

data BodyAggregateElement = BodyAggregateElement [Term] [Literal]

rawBodyAggregateElement :: BodyAggregateElement -> IO AstBodyAggregateElement
rawBodyAggregateElement (BodyAggregateElement ts ls) = do
    ts' <- newArray =<< mapM rawTerm ts
    ls' <- newArray =<< mapM rawLiteral ls
    return $ AstBodyAggregateElement ts' (fromIntegral . length $ ts) 
                                     ls' (fromIntegral . length $ ls)

freeBodyAggregateElement :: AstBodyAggregateElement -> IO ()
freeBodyAggregateElement (AstBodyAggregateElement ts nt ls nl) = do
    freeArray ts nt freeTerm
    freeArray ls nl freeLiteral

fromRawBodyAggregateElement :: BodyAggregateElement -> AstBodyAggregateElement
fromRawBodyAggregateElement = undefined

data BodyAggregate = BodyAggregate AggregateFunction [BodyAggregateElement] 
                                   (Maybe AggregateGuard) 
                                   (Maybe AggregateGuard)

rawBodyAggregate :: BodyAggregate -> IO AstBodyAggregate
rawBodyAggregate (BodyAggregate f es a b) = AstBodyAggregate
    <$> pure (rawAggregateFunction f)
    <*> (newArray =<< mapM rawBodyAggregateElement es)
    <*> pure (fromIntegral . length $ es)
    <*> rawAggregateGuardM a
    <*> rawAggregateGuardM b

freeBodyAggregate :: AstBodyAggregate -> IO ()
freeBodyAggregate (AstBodyAggregate _ es n a b) = do
    freeArray es n freeBodyAggregateElement
    freeIndirection a freeAggregateGuard
    freeIndirection b freeAggregateGuard

fromRawBodyAggregate :: BodyAggregate -> AstBodyAggregate
fromRawBodyAggregate = undefined

data AggregateFunction = Count | Sum | Sump | Min | Max

rawAggregateFunction :: AggregateFunction -> AstAggregateFunction
rawAggregateFunction f = case f of
    Count -> AstAggregateFunctionCount
    Sum -> AstAggregateFunctionSum
    Sump -> AstAggregateFunctionSump
    Min -> AstAggregateFunctionMin
    Max -> AstAggregateFunctionMax

fromRawAggregateFunction :: AstAggregateFunction -> AggregateFunction
fromRawAggregateFunction f = case f of
    AstAggregateFunctionCount -> Count
    AstAggregateFunctionSum -> Sum
    AstAggregateFunctionSump -> Sump
    AstAggregateFunctionMin -> Min
    AstAggregateFunctionMax -> Max
    _ -> error "Invalid clingo_ast_aggregate_function_t"

data HeadAggregateElement = HeadAggregateElement [Term] ConditionalLiteral

rawHeadAggregateElement :: HeadAggregateElement -> IO AstHeadAggregateElement
rawHeadAggregateElement (HeadAggregateElement ts l) = AstHeadAggregateElement
    <$> (newArray =<< mapM rawTerm ts)
    <*> pure (fromIntegral . length $ ts)
    <*> rawConditionalLiteral l

freeHeadAggregateElement :: AstHeadAggregateElement -> IO ()
freeHeadAggregateElement (AstHeadAggregateElement p n l) = do
    freeArray p n freeTerm
    freeConditionalLiteral l

fromRawHeadAggregateElement :: HeadAggregateElement -> AstHeadAggregateElement
fromRawHeadAggregateElement = undefined

data HeadAggregate = HeadAggregate AggregateFunction
                                   [HeadAggregateElement]
                                   (Maybe AggregateGuard)
                                   (Maybe AggregateGuard)

rawHeadAggregate :: HeadAggregate -> IO AstHeadAggregate
rawHeadAggregate (HeadAggregate f es a b) = AstHeadAggregate
    <$> pure (rawAggregateFunction f)
    <*> (newArray =<< mapM rawHeadAggregateElement es)
    <*> pure (fromIntegral . length $ es)
    <*> rawAggregateGuardM a
    <*> rawAggregateGuardM b

freeHeadAggregate :: AstHeadAggregate -> IO ()
freeHeadAggregate (AstHeadAggregate _ es n a b) = do
    freeArray es n freeHeadAggregateElement
    freeIndirection a freeAggregateGuard
    freeIndirection b freeAggregateGuard

fromRawHeadAggregate :: HeadAggregate -> AstHeadAggregate
fromRawHeadAggregate = undefined

data Disjunction = Disjunction [ConditionalLiteral]

rawDisjunction :: Disjunction -> IO AstDisjunction
rawDisjunction (Disjunction ls) = AstDisjunction 
    <$> (newArray =<< mapM rawConditionalLiteral ls)
    <*> pure (fromIntegral . length $ ls)

freeDisjunction :: AstDisjunction -> IO ()
freeDisjunction (AstDisjunction ls n) = freeArray ls n freeConditionalLiteral

fromRawDisjunction :: Disjunction -> AstDisjunction
fromRawDisjunction = undefined

data DisjointElement = DisjointElement Location [Term] CspSumTerm [Literal]

rawDisjointElement :: DisjointElement -> IO AstDisjointElement
rawDisjointElement (DisjointElement l ts s ls) = AstDisjointElement
    <$> rawLocation l
    <*> (newArray =<< mapM rawTerm ts)
    <*> pure (fromIntegral . length $ ts)
    <*> rawCspSumTerm s
    <*> (newArray =<< mapM rawLiteral ls)
    <*> pure (fromIntegral . length $ ls)

freeDisjointElement :: AstDisjointElement -> IO ()
freeDisjointElement (AstDisjointElement l ts nt s ls nl) = do
    freeRawLocation l
    freeCspSumTerm s
    freeArray ts nt freeTerm
    freeArray ls nl freeLiteral

fromRawDisjointElement :: DisjointElement -> AstDisjointElement
fromRawDisjointElement = undefined

data Disjoint = Disjoint [DisjointElement]

rawDisjoint :: Disjoint -> IO AstDisjoint
rawDisjoint (Disjoint es) = AstDisjoint
    <$> (newArray =<< mapM rawDisjointElement es)
    <*> pure (fromIntegral . length $ es)

freeDisjoint :: AstDisjoint -> IO ()
freeDisjoint (AstDisjoint ls n) = freeArray ls n freeDisjointElement

fromRawDisjoint :: Disjoint -> AstDisjoint
fromRawDisjoint = undefined

data TheoryTermArray = TheoryTermArray [TheoryTerm]

rawTheoryTermArray :: TheoryTermArray -> IO AstTheoryTermArray
rawTheoryTermArray (TheoryTermArray ts) = AstTheoryTermArray
    <$> (newArray =<< mapM rawTheoryTerm ts)
    <*> pure (fromIntegral . length $ ts)

freeTheoryTermArray :: AstTheoryTermArray -> IO ()
freeTheoryTermArray (AstTheoryTermArray ts n) = freeArray ts n freeTheoryTerm

fromRawTheoryTermArray :: TheoryTermArray -> AstTheoryTermArray
fromRawTheoryTermArray = undefined

data TheoryFunction = TheoryFunction Text [TheoryTerm]

rawTheoryFunction :: TheoryFunction -> IO AstTheoryFunction
rawTheoryFunction (TheoryFunction t ts) = AstTheoryFunction
    <$> newCString (unpack t)
    <*> (newArray =<< mapM rawTheoryTerm ts)
    <*> pure (fromIntegral . length $ ts)

freeTheoryFunction :: AstTheoryFunction -> IO ()
freeTheoryFunction (AstTheoryFunction s p n) = do
    free s
    freeArray p n freeTheoryTerm

fromRawTheoryFunction :: TheoryFunction -> AstTheoryFunction
fromRawTheoryFunction = undefined

data TheoryUnparsedTermElement = TheoryUnparsedTermElement [Text] TheoryTerm

rawTheoryUnparsedTermElement :: TheoryUnparsedTermElement 
                             -> IO AstTheoryUnparsedTermElement
rawTheoryUnparsedTermElement (TheoryUnparsedTermElement ts t) =
    AstTheoryUnparsedTermElement
    <$> (newArray =<< mapM (newCString . unpack) ts)
    <*> pure (fromIntegral . length $ ts)
    <*> rawTheoryTerm t

freeTheoryUnparsedTermElement :: AstTheoryUnparsedTermElement -> IO ()
freeTheoryUnparsedTermElement (AstTheoryUnparsedTermElement ss n t) = do
    freeTheoryTerm t
    freeArray ss n free

fromRawTheoryUnparsedTermElement :: TheoryUnparsedTermElement 
                                 -> AstTheoryUnparsedTermElement
fromRawTheoryUnparsedTermElement = undefined

data TheoryUnparsedTerm = TheoryUnparsedTerm [TheoryUnparsedTermElement]

rawTheoryUnparsedTerm :: TheoryUnparsedTerm -> IO AstTheoryUnparsedTerm
rawTheoryUnparsedTerm (TheoryUnparsedTerm es) = AstTheoryUnparsedTerm
    <$> (newArray =<< mapM rawTheoryUnparsedTermElement es)
    <*> pure (fromIntegral . length $ es)

freeTheoryUnparsedTerm :: AstTheoryUnparsedTerm -> IO ()
freeTheoryUnparsedTerm (AstTheoryUnparsedTerm es n) = 
    freeArray es n freeTheoryUnparsedTermElement

fromRawTheoryUnparsedTerm :: TheoryUnparsedTerm -> AstTheoryUnparsedTerm
fromRawTheoryUnparsedTerm = undefined

data TheoryTerm
    = TheoryTermSymbol Location Symbol
    | TheoryTermVariable Location Text
    | TheoryTermTuple Location TheoryTermArray
    | TheoryTermList Location TheoryTermArray
    | TheoryTermSet Location TheoryTermArray
    | TheoryTermFunction Location TheoryFunction
    | TheoryTermUnparsed Location TheoryUnparsedTerm

rawTheoryTerm :: TheoryTerm -> IO AstTheoryTerm
rawTheoryTerm (TheoryTermSymbol l _) = 
    AstTheoryTermSymbol <$> rawLocation l <*> undefined
rawTheoryTerm (TheoryTermVariable l _) =
    AstTheoryTermVariable <$> rawLocation l <*> undefined
rawTheoryTerm (TheoryTermTuple l a) = 
    AstTheoryTermTuple <$> rawLocation l <*> (new =<< rawTheoryTermArray a)
rawTheoryTerm (TheoryTermList l a) = 
    AstTheoryTermList <$> rawLocation l <*> (new =<< rawTheoryTermArray a)
rawTheoryTerm (TheoryTermSet l a) = 
    AstTheoryTermSet <$> rawLocation l <*> (new =<< rawTheoryTermArray a)
rawTheoryTerm (TheoryTermFunction l f) = 
    AstTheoryTermFunction <$> rawLocation l <*> (new =<< rawTheoryFunction f)
rawTheoryTerm (TheoryTermUnparsed l t) = 
    AstTheoryTermUnparsed <$> rawLocation l 
                          <*> (new =<< rawTheoryUnparsedTerm t)

freeTheoryTerm :: AstTheoryTerm -> IO ()
freeTheoryTerm (AstTheoryTermSymbol l _) = freeRawLocation l
freeTheoryTerm (AstTheoryTermVariable l _) = freeRawLocation l
freeTheoryTerm (AstTheoryTermTuple l a) = do
    freeRawLocation l
    freeIndirection a freeTheoryTermArray
freeTheoryTerm (AstTheoryTermList l a) = do
    freeRawLocation l
    freeIndirection a freeTheoryTermArray
freeTheoryTerm (AstTheoryTermSet l a) = do
    freeRawLocation l
    freeIndirection a freeTheoryTermArray
freeTheoryTerm (AstTheoryTermFunction l f) = do
    freeRawLocation l 
    freeIndirection f freeTheoryFunction
freeTheoryTerm (AstTheoryTermUnparsed l t) = do
    freeRawLocation l 
    freeIndirection t freeTheoryUnparsedTerm

fromRawTheoryTerm :: TheoryTerm -> AstTheoryTerm
fromRawTheoryTerm = undefined

data TheoryAtomElement = TheoryAtomElement [TheoryTerm] [Literal]

rawTheoryAtomElement :: TheoryAtomElement -> IO AstTheoryAtomElement
rawTheoryAtomElement (TheoryAtomElement ts ls) = AstTheoryAtomElement
    <$> (newArray =<< mapM rawTheoryTerm ts)
    <*> pure (fromIntegral . length $ ts)
    <*> (newArray =<< mapM rawLiteral ls)
    <*> pure (fromIntegral . length $ ls)

freeTheoryAtomElement :: AstTheoryAtomElement -> IO ()
freeTheoryAtomElement (AstTheoryAtomElement ts nt ls nl) = do
    freeArray ts nt freeTheoryTerm
    freeArray ls nl freeLiteral

fromRawTheoryAtomElement :: TheoryAtomElement -> AstTheoryAtomElement
fromRawTheoryAtomElement = undefined

data TheoryGuard = TheoryGuard Text TheoryTerm

rawTheoryGuard :: TheoryGuard -> IO AstTheoryGuard
rawTheoryGuard (TheoryGuard s t) = AstTheoryGuard
    <$> newCString (unpack s)
    <*> rawTheoryTerm t

freeTheoryGuard :: AstTheoryGuard -> IO ()
freeTheoryGuard (AstTheoryGuard s t) = free s >> freeTheoryTerm t

fromRawTheoryGuard :: TheoryGuard -> AstTheoryGuard
fromRawTheoryGuard = undefined

data TheoryAtom = TheoryAtom Term [TheoryAtomElement] TheoryGuard

rawTheoryAtom :: TheoryAtom -> IO AstTheoryAtom
rawTheoryAtom (TheoryAtom t es g) = AstTheoryAtom
    <$> rawTerm t
    <*> (newArray =<< mapM rawTheoryAtomElement es)
    <*> pure (fromIntegral . length $ es)
    <*> rawTheoryGuard g

freeTheoryAtom :: AstTheoryAtom -> IO ()
freeTheoryAtom (AstTheoryAtom t es n g) = do
    freeTerm t
    freeTheoryGuard g
    freeArray es n freeTheoryAtomElement

fromRawTheoryAtom :: TheoryAtom -> AstTheoryAtom
fromRawTheoryAtom = undefined

data HeadLiteral
    = HeadLiteral Location Literal
    | HeadDisjunction Location Disjunction
    | HeadLitAggregate Location Aggregate
    | HeadTheoryAtom Location TheoryAtom

rawHeadLiteral :: HeadLiteral -> IO AstHeadLiteral
rawHeadLiteral (HeadLiteral l x) = AstHeadLiteral
    <$> rawLocation l <*> (new =<< rawLiteral x)
rawHeadLiteral (HeadDisjunction l d) = AstHeadDisjunction
    <$> rawLocation l <*> (new =<< rawDisjunction d)
rawHeadLiteral (HeadLitAggregate l d) = AstHeadLitAggregate
    <$> rawLocation l <*> (new =<< rawAggregate d)
rawHeadLiteral (HeadTheoryAtom l d) = AstHeadTheoryAtom
    <$> rawLocation l <*> (new =<< rawTheoryAtom d)

freeHeadLiteral :: AstHeadLiteral -> IO ()
freeHeadLiteral (AstHeadLiteral l x) = do
    freeRawLocation l
    freeIndirection x freeLiteral
freeHeadLiteral (AstHeadDisjunction l x) = do
    freeRawLocation l
    freeIndirection x freeDisjunction
freeHeadLiteral (AstHeadLitAggregate l x) = do
    freeRawLocation l
    freeIndirection x freeAggregate
freeHeadLiteral (AstHeadTheoryAtom l x) = do
    freeRawLocation l
    freeIndirection x freeTheoryAtom

fromRawHeadLiteral :: HeadLiteral -> AstHeadLiteral
fromRawHeadLiteral = undefined

data BodyLiteral
    = BodyLiteral Location Sign Literal
    | BodyConditional Location ConditionalLiteral
    | BodyLitAggregate Location Sign Aggregate
    | BodyTheoryAtom Location Sign TheoryAtom
    | BodyDisjoint Location Sign Disjoint

rawBodyLiteral :: BodyLiteral -> IO AstBodyLiteral
rawBodyLiteral (BodyLiteral l s x) = AstBodyLiteral
    <$> rawLocation l <*> pure (rawSign s)
    <*> (new =<< rawLiteral x)
rawBodyLiteral (BodyConditional l x) = AstBodyConditional
    <$> rawLocation l
    <*> (new =<< rawConditionalLiteral x)
rawBodyLiteral (BodyLitAggregate l s x) = AstBodyLitAggregate
    <$> rawLocation l <*> pure (rawSign s)
    <*> (new =<< rawAggregate x)
rawBodyLiteral (BodyTheoryAtom l s x) = AstBodyTheoryAtom
    <$> rawLocation l <*> pure (rawSign s)
    <*> (new =<< rawTheoryAtom x)
rawBodyLiteral (BodyDisjoint l s x) = AstBodyDisjoint
    <$> rawLocation l <*> pure (rawSign s)
    <*> (new =<< rawDisjoint x)

freeBodyLiteral :: AstBodyLiteral -> IO ()
freeBodyLiteral (AstBodyLiteral l _ x) = do
    freeRawLocation l
    freeIndirection x freeLiteral
freeBodyLiteral (AstBodyConditional l x) = do
    freeRawLocation l
    freeIndirection x freeConditionalLiteral
freeBodyLiteral (AstBodyLitAggregate l _ x) = do
    freeRawLocation l
    freeIndirection x freeAggregate
freeBodyLiteral (AstBodyTheoryAtom l _ x) = do
    freeRawLocation l
    freeIndirection x freeTheoryAtom
freeBodyLiteral (AstBodyDisjoint l _ x) = do
    freeRawLocation l
    freeIndirection x freeDisjoint

fromRawBodyLiteral :: BodyLiteral -> AstBodyLiteral
fromRawBodyLiteral = undefined

data TheoryOperatorDefinition = 
    TheoryOperatorDefinition Location Text Natural TheoryOperatorType

rawTheoryOperatorDefinition :: TheoryOperatorDefinition 
                            -> IO AstTheoryOperatorDefinition
rawTheoryOperatorDefinition (TheoryOperatorDefinition l s x t) = 
    AstTheoryOperatorDefinition
    <$> rawLocation l
    <*> newCString (unpack s)
    <*> pure (fromIntegral x)
    <*> pure (rawTheoryOperatorType t)

freeTheoryOperatorDefinition :: AstTheoryOperatorDefinition -> IO ()
freeTheoryOperatorDefinition (AstTheoryOperatorDefinition l s _ _) =
    freeRawLocation l >> free s

fromRawTheoryOperatorDefinition :: TheoryOperatorDefinition -> AstTheoryOperatorDefinition
fromRawTheoryOperatorDefinition = undefined

data TheoryOperatorType = Unary | BinLeft | BinRight

rawTheoryOperatorType :: TheoryOperatorType -> AstTheoryOperatorType
rawTheoryOperatorType t = case t of
    Unary -> AstTheoryOperatorTypeUnary
    BinLeft -> AstTheoryOperatorTypeBinaryLeft
    BinRight -> AstTheoryOperatorTypeBinaryRight

fromRawTheoryOperatorType :: AstTheoryOperatorType -> TheoryOperatorType
fromRawTheoryOperatorType t = case t of
    AstTheoryOperatorTypeUnary -> Unary 
    AstTheoryOperatorTypeBinaryLeft -> BinLeft 
    AstTheoryOperatorTypeBinaryRight -> BinRight 
    _ -> error "Invalid clingo_ast_theory_operator_type_t"

data TheoryTermDefinition =
    TheoryTermDefinition Location Text [TheoryOperatorDefinition]

rawTheoryTermDefinition :: TheoryTermDefinition -> IO AstTheoryTermDefinition
rawTheoryTermDefinition (TheoryTermDefinition l s xs) = AstTheoryTermDefinition
    <$> rawLocation l
    <*> newCString (unpack s)
    <*> (newArray =<< mapM rawTheoryOperatorDefinition xs)
    <*> pure (fromIntegral . length $ xs)

freeTheoryTermDefinition :: AstTheoryTermDefinition -> IO ()
freeTheoryTermDefinition (AstTheoryTermDefinition l s xs n) = do
    freeRawLocation l
    free s
    freeArray xs n freeTheoryOperatorDefinition

fromRawTheoryTermDefinition :: TheoryTermDefinition -> AstTheoryTermDefinition
fromRawTheoryTermDefinition = undefined

data TheoryGuardDefinition =
    TheoryGuardDefinition Text [Text]

rawTheoryGuardDefinition :: TheoryGuardDefinition -> IO AstTheoryGuardDefinition
rawTheoryGuardDefinition (TheoryGuardDefinition t ts) = AstTheoryGuardDefinition
    <$> newCString (unpack t)
    <*> (newArray =<< mapM (newCString . unpack) ts)
    <*> pure (fromIntegral . length $ ts)

freeTheoryGuardDefinition :: AstTheoryGuardDefinition -> IO ()
freeTheoryGuardDefinition (AstTheoryGuardDefinition s ss n) = do
    free s
    freeArray ss n free

fromRawTheoryGuardDefinition :: TheoryGuardDefinition -> AstTheoryGuardDefinition
fromRawTheoryGuardDefinition = undefined

data TheoryAtomDefinition =
    TheoryAtomDefinition Location TheoryAtomDefinitionType 
                         Text Int Text TheoryGuardDefinition

rawTheoryAtomDefinition :: TheoryAtomDefinition -> IO AstTheoryAtomDefinition
rawTheoryAtomDefinition (TheoryAtomDefinition l t a i b d) =
    AstTheoryAtomDefinition
    <$> rawLocation l
    <*> pure (rawTheoryAtomDefinitionType t)
    <*> newCString (unpack a)
    <*> pure (fromIntegral i)
    <*> newCString (unpack b)
    <*> (new =<< rawTheoryGuardDefinition d)

freeTheoryAtomDefinition :: AstTheoryAtomDefinition -> IO ()
freeTheoryAtomDefinition (AstTheoryAtomDefinition l _ a _ b p) = do
    freeRawLocation l
    free a
    free b
    freeIndirection p freeTheoryGuardDefinition

fromRawTheoryAtomDefinition :: TheoryAtomDefinition -> AstTheoryAtomDefinition
fromRawTheoryAtomDefinition = undefined

data TheoryAtomDefinitionType = Head | Body | Any | Directive

rawTheoryAtomDefinitionType :: TheoryAtomDefinitionType 
                            -> AstTheoryAtomDefType
rawTheoryAtomDefinitionType t = case t of
    Head -> AstTheoryAtomDefinitionTypeHead
    Body -> AstTheoryAtomDefinitionTypeBody
    Any -> AstTheoryAtomDefinitionTypeAny
    Directive -> AstTheoryAtomDefinitionTypeDirective

fromRawTheoryAtomDefinitionType :: AstTheoryAtomDefType 
                                -> TheoryAtomDefinitionType
fromRawTheoryAtomDefinitionType t = case t of
    AstTheoryAtomDefinitionTypeHead -> Head 
    AstTheoryAtomDefinitionTypeBody -> Body 
    AstTheoryAtomDefinitionTypeAny -> Any 
    AstTheoryAtomDefinitionTypeDirective -> Directive 
    _ -> error "Invalid clingo_ast_theory_atom_definition_type_t"

data TheoryDefinition =
    TheoryDefinition Text [TheoryTermDefinition] [TheoryAtomDefinition]

rawTheoryDefinition :: TheoryDefinition -> IO AstTheoryDefinition
rawTheoryDefinition (TheoryDefinition t ts as) = AstTheoryDefinition
    <$> newCString (unpack t)
    <*> (newArray =<< mapM rawTheoryTermDefinition ts)
    <*> pure (fromIntegral . length $ ts)
    <*> (newArray =<< mapM rawTheoryAtomDefinition as)
    <*> pure (fromIntegral . length $ as)

freeTheoryDefinition :: AstTheoryDefinition -> IO ()
freeTheoryDefinition (AstTheoryDefinition t ts nt as na) = do
    free t
    freeArray ts nt freeTheoryTermDefinition
    freeArray as na freeTheoryAtomDefinition

fromRawTheoryDefinition :: TheoryDefinition -> AstTheoryDefinition
fromRawTheoryDefinition = undefined

data Rule = Rule HeadLiteral [BodyLiteral]

rawRule :: Rule -> IO AstRule
rawRule (Rule h bs) = AstRule
    <$> rawHeadLiteral h <*> (newArray =<< mapM rawBodyLiteral bs)
                         <*> pure (fromIntegral . length $ bs)

freeRule :: AstRule -> IO ()
freeRule (AstRule h bs n) = do
    freeHeadLiteral h
    freeArray bs n freeBodyLiteral

fromRawRule :: Rule -> AstRule
fromRawRule = undefined

data Definition = Definition Text Term Bool

rawDefinition :: Definition -> IO AstDefinition
rawDefinition (Definition s t b) = AstDefinition
    <$> newCString (unpack s)
    <*> rawTerm t
    <*> pure (fromBool b)

freeDefinition :: AstDefinition -> IO ()
freeDefinition (AstDefinition s t _) = free s >> freeTerm t

fromRawDefinition :: Definition -> AstDefinition
fromRawDefinition = undefined

data ShowSignature = ShowSignature Signature Bool

rawShowSignature :: ShowSignature -> IO AstShowSignature
rawShowSignature (ShowSignature s b) = AstShowSignature
    <$> undefined
    <*> pure (fromBool b)

freeShowSignature :: AstShowSignature -> IO ()
freeShowSignature (AstShowSignature _ _) = return ()

fromRawShowSignature :: ShowSignature -> AstShowSignature
fromRawShowSignature = undefined

data ShowTerm = ShowTerm Term [BodyLiteral] Bool

rawShowTerm :: ShowTerm -> IO AstShowTerm
rawShowTerm (ShowTerm t ls b) = AstShowTerm
    <$> rawTerm t
    <*> (newArray =<< mapM rawBodyLiteral ls)
    <*> pure (fromIntegral . length $ ls)
    <*> pure (fromBool b)

freeShowTerm :: AstShowTerm -> IO ()
freeShowTerm (AstShowTerm t ls n _) = do
    freeTerm t
    freeArray ls n freeBodyLiteral

fromRawShowTerm :: ShowTerm -> AstShowTerm
fromRawShowTerm = undefined

data Minimize = Minimize Term Term [Term] [BodyLiteral]

rawMinimize :: Minimize -> IO AstMinimize
rawMinimize (Minimize a b ts ls) = AstMinimize
    <$> rawTerm a
    <*> rawTerm b
    <*> (newArray =<< mapM rawTerm ts)
    <*> pure (fromIntegral . length $ ts)
    <*> (newArray =<< mapM rawBodyLiteral ls)
    <*> pure (fromIntegral . length $ ls)

freeMinimize :: AstMinimize -> IO ()
freeMinimize (AstMinimize a b ts nt ls nl) = do
    freeTerm a
    freeTerm b
    freeArray ts nt freeTerm
    freeArray ls nl freeBodyLiteral

fromRawMinimize :: Minimize -> AstMinimize
fromRawMinimize = undefined

data Script = Script ScriptType Text

rawScript :: Script -> IO AstScript
rawScript (Script t s) = AstScript
    <$> pure (rawScriptType t)
    <*> newCString (unpack s)

freeScript :: AstScript -> IO ()
freeScript (AstScript _ s) = free s

fromRawScript :: Script -> AstScript
fromRawScript = undefined

data ScriptType = Lua | Python

rawScriptType :: ScriptType -> AstScriptType
rawScriptType t = case t of
    Lua -> AstScriptTypeLua
    Python -> AstScriptTypePython

fromRawScriptType :: AstScriptType -> ScriptType
fromRawScriptType t = case t of
    AstScriptTypeLua -> Lua 
    AstScriptTypePython -> Python 
    _ -> error "Invalid clingo_ast_script_type_t"

data Program = Program Text [Identifier]

rawProgram :: Program -> IO AstProgram
rawProgram (Program n is) = AstProgram
    <$> newCString (unpack n)
    <*> (newArray =<< mapM rawIdentifier is)
    <*> pure (fromIntegral . length $ is)

freeProgram :: AstProgram -> IO ()
freeProgram (AstProgram s xs n) = free s >> freeArray xs n freeIdentifier

fromRawProgram :: Program -> AstProgram
fromRawProgram = undefined

data External = External Term [BodyLiteral]

rawExternal :: External -> IO AstExternal
rawExternal (External t ls) = AstExternal
    <$> rawTerm t
    <*> (newArray =<< mapM rawBodyLiteral ls)
    <*> pure (fromIntegral . length $ ls)

freeExternal :: AstExternal -> IO ()
freeExternal (AstExternal t ls n) = freeTerm t >> freeArray ls n freeBodyLiteral

fromRawExternal :: External -> AstExternal
fromRawExternal = undefined

data Edge = Edge Term Term [BodyLiteral]

rawEdge :: Edge -> IO AstEdge
rawEdge (Edge a b ls) = AstEdge
    <$> rawTerm a
    <*> rawTerm b
    <*> (newArray =<< mapM rawBodyLiteral ls)
    <*> pure (fromIntegral . length $ ls)

freeEdge :: AstEdge -> IO ()
freeEdge (AstEdge a b ls n) = do
    freeTerm a
    freeTerm b
    freeArray ls n freeBodyLiteral

fromRawEdge :: Edge -> AstEdge
fromRawEdge = undefined

data Heuristic = Heuristic Term [BodyLiteral] Term Term Term

rawHeuristic :: Heuristic -> IO AstHeuristic
rawHeuristic (Heuristic a ls b c d) = AstHeuristic
    <$> rawTerm a
    <*> (newArray =<< mapM rawBodyLiteral ls)
    <*> pure (fromIntegral . length $ ls)
    <*> rawTerm b
    <*> rawTerm c
    <*> rawTerm d

freeHeuristic :: AstHeuristic -> IO ()
freeHeuristic (AstHeuristic a ls n b c d) = do
    freeTerm a
    freeTerm b
    freeTerm c
    freeTerm d
    freeArray ls n freeBodyLiteral

fromRawHeuristic :: Heuristic -> AstHeuristic
fromRawHeuristic = undefined

data Project = Project Term [BodyLiteral]

rawProject :: Project -> IO AstProject
rawProject (Project t ls) = AstProject
    <$> rawTerm t
    <*> (newArray =<< mapM rawBodyLiteral ls)
    <*> pure (fromIntegral . length $ ls)

freeProject :: AstProject -> IO ()
freeProject (AstProject t ls n) = do
    freeTerm t
    freeArray ls n freeBodyLiteral

fromRawProject :: Project -> AstProject
fromRawProject = undefined

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

rawStatement :: Statement -> IO AstStatement
rawStatement (StmtRule l x) = AstStmtRule
    <$> rawLocation l <*> (new =<< rawRule x)
rawStatement (StmtDefinition l x) = AstStmtDefinition
    <$> rawLocation l <*> (new =<< rawDefinition x)
rawStatement (StmtShowSignature l x) = AstStmtShowSignature
    <$> rawLocation l <*> (new =<< rawShowSignature x)
rawStatement (StmtShowTerm l x) = AstStmtShowTerm
    <$> rawLocation l <*> (new =<< rawShowTerm x)
rawStatement (StmtMinimize l x) = AstStmtMinimize
    <$> rawLocation l <*> (new =<< rawMinimize x)
rawStatement (StmtScript l x) = AstStmtScript
    <$> rawLocation l <*> (new =<< rawScript x)
rawStatement (StmtProgram l x) = AstStmtProgram
    <$> rawLocation l <*> (new =<< rawProgram x)
rawStatement (StmtExternal l x) = AstStmtExternal
    <$> rawLocation l <*> (new =<< rawExternal x)
rawStatement (StmtEdge l x) = AstStmtEdge
    <$> rawLocation l <*> (new =<< rawEdge x)
rawStatement (StmtHeuristic l x) = AstStmtHeuristic
    <$> rawLocation l <*> (new =<< rawHeuristic x)
rawStatement (StmtProject l x) = AstStmtProject
    <$> rawLocation l <*> (new =<< rawProject x)
rawStatement (StmtSignature l x) = AstStmtSignature
    <$> rawLocation l <*> undefined
rawStatement (StmtTheoryDefinition l x) = AstStmtTheoryDefn
    <$> rawLocation l <*> (new =<< rawTheoryDefinition x)

freeStatement :: AstStatement -> IO ()
freeStatement (AstStmtRule l x) = 
    freeRawLocation l >> freeIndirection x freeRule
freeStatement (AstStmtDefinition l x) = 
    freeRawLocation l >> freeIndirection x freeDefinition
freeStatement (AstStmtShowSignature l x) = 
    freeRawLocation l >> freeIndirection x freeShowSignature
freeStatement (AstStmtShowTerm l x) = 
    freeRawLocation l >> freeIndirection x freeShowTerm
freeStatement (AstStmtMinimize l x) = 
    freeRawLocation l >> freeIndirection x freeMinimize
freeStatement (AstStmtScript l x) = 
    freeRawLocation l >> freeIndirection x freeScript
freeStatement (AstStmtProgram l x) = 
    freeRawLocation l >> freeIndirection x freeProgram
freeStatement (AstStmtExternal l x) = 
    freeRawLocation l >> freeIndirection x freeExternal
freeStatement (AstStmtEdge l x) = 
    freeRawLocation l >> freeIndirection x freeEdge
freeStatement (AstStmtHeuristic l x) = 
    freeRawLocation l >> freeIndirection x freeHeuristic
freeStatement (AstStmtProject l x) = 
    freeRawLocation l >> freeIndirection x freeProject
freeStatement (AstStmtSignature l x) = 
    freeRawLocation l >> undefined
freeStatement (AstStmtTheoryDefn l x) =
    freeRawLocation l >> freeIndirection x freeTheoryDefinition

fromRawStatement :: Statement -> AstStatement
fromRawStatement = undefined
