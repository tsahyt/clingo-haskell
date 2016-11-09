module Clingo.AST
(
)
where

import Control.Monad
import Data.Text (Text, unpack)
import Foreign hiding (Pool, freePool)
import Foreign.C

import Clingo.Internal.Types (Location, rawLocation, freeRawLocation, 
                              fromRawLocation)
import Clingo.Raw.AST

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
    unless (ts == nullPtr) $ do
        ts' <- peekArray (fromIntegral n) ts
        mapM_ freeTerm ts'
        free ts

fromRawFunction :: Function -> AstFunction
fromRawFunction = undefined

data Pool = Pool [Term]

rawPool :: Pool -> IO AstPool
rawPool (Pool ts) = do
    ts' <- newArray =<< mapM rawTerm ts
    return $ AstPool ts' (fromIntegral . length $ ts)

freePool :: AstPool -> IO ()
freePool (AstPool ts n) =
    unless (ts == nullPtr) $ do
        ts' <- peekArray (fromIntegral n) ts
        mapM_ freeTerm ts'
        free ts

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
    unless (p == nullPtr) ((freeTerm =<< peek p) >> free p)

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
    unless (ts == nullPtr) $ do
        ts' <- peekArray (fromIntegral n) ts
        mapM_ freeCspProductTerm ts'
        free ts

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
    unless (p == nullPtr) $ do
        p' <- peekArray (fromIntegral n) p
        mapM_ freeCspGuard p'
        free p

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
    unless (ls == nullPtr) $ do
        ls' <- peekArray (fromIntegral n) ls
        mapM_ freeLiteral ls'
        free ls

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
    unless (a == nullPtr) ((freeAggregateGuard =<< peek a) >> free a)
    unless (b == nullPtr) ((freeAggregateGuard =<< peek b) >> free b)
    unless (ls == nullPtr) $ do
        ls' <- peekArray (fromIntegral n) ls
        mapM_ freeConditionalLiteral ls'
        free ls

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
    unless (ts == nullPtr) $ do
        ts' <- peekArray (fromIntegral nt) ts
        mapM_ freeTerm ts'
        free ts
    unless (ls == nullPtr) $ do
        ls' <- peekArray (fromIntegral nl) ls
        mapM_ freeLiteral ls'
        free ls

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
    unless (es == nullPtr) $ do
        es' <- peekArray (fromIntegral n) es
        mapM_ freeBodyAggregateElement es'
        free es
    unless (a == nullPtr) ((freeAggregateGuard =<< peek a) >> free a)
    unless (b == nullPtr) ((freeAggregateGuard =<< peek b) >> free b)

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
    unless (p == nullPtr) $ do
        p' <- peekArray (fromIntegral n) p
        mapM_ freeTerm p'
        free p
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
    unless (es == nullPtr) $ do
        es' <- peekArray (fromIntegral n) es
        mapM_ freeHeadAggregateElement es'
        free es
    unless (a == nullPtr) ((freeAggregateGuard =<< peek a) >> free a)
    unless (b == nullPtr) ((freeAggregateGuard =<< peek b) >> free b)

fromRawHeadAggregate :: HeadAggregate -> AstHeadAggregate
fromRawHeadAggregate = undefined

data Disjunction = Disjunction [ConditionalLiteral]

rawDisjunction :: Disjunction -> IO AstDisjunction
rawDisjunction (Disjunction ls) = AstDisjunction 
    <$> (newArray =<< mapM rawConditionalLiteral ls)
    <*> pure (fromIntegral . length $ ls)

freeDisjunction :: AstDisjunction -> IO ()
freeDisjunction (AstDisjunction ls n) =
    unless (ls == nullPtr) $ do
        ls' <- peekArray (fromIntegral n) ls
        mapM_ freeConditionalLiteral ls'
        free ls

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
    unless (ts == nullPtr) $ do
        ts' <- peekArray (fromIntegral nt) ts
        mapM_ freeTerm ts'
        free ts
    unless (ls == nullPtr) $ do
        ls' <- peekArray (fromIntegral nl) ls
        mapM_ freeLiteral ls'
        free ls

fromRawDisjointElement :: DisjointElement -> AstDisjointElement
fromRawDisjointElement = undefined

data Disjoint = Disjoint [DisjointElement]

rawDisjoint :: Disjoint -> IO AstDisjoint
rawDisjoint (Disjoint es) = AstDisjoint
    <$> (newArray =<< mapM rawDisjointElement es)
    <*> pure (fromIntegral . length $ es)

freeDisjoint :: AstDisjoint -> IO ()
freeDisjoint (AstDisjoint ls n) =
    unless (ls == nullPtr) $ do
        ls' <- peekArray (fromIntegral n) ls
        mapM_ freeDisjointElement ls'
        free ls

fromRawDisjoint :: Disjoint -> AstDisjoint
fromRawDisjoint = undefined

data TheoryTermArray = TheoryTermArray [TheoryTerm]

rawTheoryTermArray :: TheoryTermArray -> IO AstTheoryTermArray
rawTheoryTermArray (TheoryTermArray ts) = AstTheoryTermArray
    <$> (newArray =<< mapM rawTheoryTerm ts)
    <*> pure (fromIntegral . length $ ts)

freeTheoryTermArray :: AstTheoryTermArray -> IO ()
freeTheoryTermArray (AstTheoryTermArray ts n) =
    unless (ts == nullPtr) $ do
        ts' <- peekArray (fromIntegral n) ts
        mapM_ freeTheoryTerm ts'
        free ts

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
    unless (p == nullPtr) $ do
        p' <- peekArray (fromIntegral n) p
        mapM_ freeTheoryTerm p'
        free p

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
    unless (ss == nullPtr) $ do
        ss' <- peekArray (fromIntegral n) ss
        mapM_ free ss'
        free ss

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
    unless (es == nullPtr) $ do
        es' <- peekArray (fromIntegral n) es
        mapM_ freeTheoryUnparsedTermElement es'
        free es

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
    unless (a == nullPtr) ((freeTheoryTermArray =<< peek a) >> free a)
freeTheoryTerm (AstTheoryTermList l a) = do
    freeRawLocation l
    unless (a == nullPtr) ((freeTheoryTermArray =<< peek a) >> free a)
freeTheoryTerm (AstTheoryTermSet l a) = do
    freeRawLocation l
    unless (a == nullPtr) ((freeTheoryTermArray =<< peek a) >> free a)
freeTheoryTerm (AstTheoryTermFunction l f) = do
    freeRawLocation l 
    unless (f == nullPtr) ((freeTheoryFunction =<< peek f) >> free f)
freeTheoryTerm (AstTheoryTermUnparsed l t) = do
    freeRawLocation l 
    unless (t == nullPtr) ((freeTheoryUnparsedTerm =<< peek t) >> free t)

fromRawTheoryTerm :: TheoryTerm -> AstTheoryTerm
fromRawTheoryTerm = undefined

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
