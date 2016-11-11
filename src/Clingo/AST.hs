module Clingo.AST
(
    parseProgram,

    Sign (..),
    Signature,
    wrapSignature,
    Symbol,
    wrapSymbol,
    UnaryOperation (..),
    UnaryOperator (..),
    BinaryOperation (..),
    BinaryOperator (..),
    Interval (..),
    Function (..),
    Pool (..),
    Term (..),
    CspProductTerm (..),
    CspSumTerm (..),
    CspGuard (..),
    ComparisonOperator (..),
    CspLiteral (..),
    Identifier (..),
    Comparison (..),
    Literal (..),
    AggregateGuard (..),
    ConditionalLiteral (..),
    Aggregate (..),
    BodyAggregateElement (..),
    BodyAggregate (..),
    AggregateFunction (..),
    HeadAggregateElement (..),
    HeadAggregate (..),
    Disjunction (..),
    DisjointElement (..),
    Disjoint (..),
    TheoryTermArray (..),
    TheoryFunction (..),
    TheoryUnparsedTermElement (..),
    TheoryUnparsedTerm (..),
    TheoryTerm (..),
    TheoryAtomElement (..),
    TheoryGuard (..),
    TheoryAtom (..),
    HeadLiteral (..),
    BodyLiteral (..),
    TheoryOperatorDefinition (..),
    TheoryOperatorType (..),
    TheoryTermDefinition (..),
    TheoryGuardDefinition (..),
    TheoryAtomDefinition (..),
    TheoryAtomDefinitionType (..),
    TheoryDefinition (..),
    Rule (..),
    Definition (..),
    ShowSignature (..),
    ShowTerm (..),
    Minimize (..),
    Script (..),
    ScriptType (..),
    Program (..),
    External (..),
    Edge (..),
    Heuristic (..),
    Project (..),
    Statement (..)
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.Text (Text, unpack)
import Data.IORef
import Numeric.Natural

import Foreign hiding (Pool)
import Foreign.C

import Clingo.Internal.AST
import Clingo.Internal.Utils
import qualified Clingo.Internal.Types as T
import qualified Clingo.Raw as Raw

wrapSymbol :: T.Symbol s -> Symbol
wrapSymbol = Symbol

{-
 -withSymbol :: Symbol -> (T.Symbol s -> m a) -> m a
 -withSymbol x f = case x of Symbol x -> f x
 -}

wrapSignature :: T.Signature s -> Signature
wrapSignature = Signature

parseProgram :: (MonadIO m, MonadThrow m)
             => T.Clingo s
             -> Text
             -> Maybe (ClingoWarning -> Text -> IO ())
             -> Natural
             -> m [Statement]
parseProgram _ prog logger limit = do
    ref <- liftIO (newIORef [])
    marshall0 $
        withCString (unpack prog) $ \p -> do
            logCB <- maybe (pure nullFunPtr) T.wrapCBLogger logger
            astCB <- wrapCBAst (\s -> modifyIORef ref (s :))
            Raw.parseProgram p astCB nullPtr 
                               logCB nullPtr (fromIntegral limit)
    liftIO (readIORef ref)

wrapCBAst :: MonadIO m
          => (Statement -> IO ())
          -> m (FunPtr (Ptr Raw.AstStatement -> Ptr () -> IO Raw.CBool))
wrapCBAst f = liftIO $ Raw.mkCallbackAst go
    where go :: Ptr Raw.AstStatement -> Ptr () -> IO Raw.CBool
          go stmt _ = reraiseIO $ f =<< fromRawStatement =<< peek stmt
