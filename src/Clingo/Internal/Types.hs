{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Internal.Types
(
    Clingo (..),
    Signed (..),
    Symbol (..),
    Signature (..),
    SymbolicLiteral (..),
    rawSymLit,
    Literal (..),
    Atom (..),
    AsyncSolver (..),
    IterSolver (..),
    Model (..),
    Location (..),
    rawLocation,
    fromRawLocation,
    SolveResult (..),
    rawSolveResult,
    fromRawSolveResult,
    exhausted,
    wrapCBLogger,
    Statistics (..),
    ProgramBuilder (..),
    Configuration (..),
    Backend (..),
    SymbolicAtoms (..),
    TheoryAtoms (..),
    TruthValue (..),
    pattern TruthFree,
    pattern TruthFalse,
    pattern TruthTrue,
    IOPropagator (..),
    rawPropagator,
    PropagateCtrl (..),
    PropagateInit (..),
    AMVTree (..)
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Text (Text, pack, unpack)
import Data.Bits
import Foreign
import Foreign.C
import Foreign.Marshal.Utils
import qualified Clingo.Raw as Raw
import Clingo.Internal.Utils
import Numeric.Natural

import System.IO.Unsafe

newtype Clingo s = Clingo Raw.Control

newtype Symbol s = Symbol { rawSymbol :: Raw.Symbol }

instance Eq (Symbol s) where
    (Symbol a) == (Symbol b) = toBool (Raw.symbolIsEqualTo a b)

instance Ord (Symbol s) where
    (Symbol a) <= (Symbol b) = toBool (Raw.symbolIsLessThan a b)

class Signed a where
    positive :: a -> Bool
    positive = not . negative

    negative :: a -> Bool
    negative = not . positive

instance Signed Bool where
    positive x = x

data SymbolicLiteral s 
    = SLPositive (Symbol s)
    | SLNegative (Symbol s)

symLitSymbol :: SymbolicLiteral s -> Symbol s
symLitSymbol (SLPositive s) = s
symLitSymbol (SLNegative s) = s

symLitPositive :: SymbolicLiteral s -> Bool
symLitPositive (SLPositive _) = True
symLitPositive _ = False

instance Signed (SymbolicLiteral s) where
    positive = symLitPositive

rawSymLit :: SymbolicLiteral s -> Raw.SymbolicLiteral
rawSymLit sl = Raw.SymbolicLiteral
    { Raw.slitSymbol   = rawSymbol (symLitSymbol sl)
    , Raw.slitPositive = fromBool (symLitPositive sl) }

newtype Signature s = Signature { rawSignature :: Raw.Signature }

instance Eq (Signature s) where
    (Signature a) == (Signature b) = toBool (Raw.signatureIsEqualTo a b)

instance Ord (Signature s) where
    (Signature a) <= (Signature b) = toBool (Raw.signatureIsLessThan a b)

instance Signed (Signature s) where
    positive = toBool . Raw.signatureIsPositive . rawSignature
    negative = toBool . Raw.signatureIsNegative . rawSignature

newtype Literal s = Literal { rawLiteral :: Raw.Literal }

newtype Atom s = Atom { rawAtom :: Raw.Atom }

newtype AsyncSolver s = AsyncSolver Raw.AsyncSolver

newtype IterSolver s = IterSolver Raw.IterSolver

newtype Model s = Model Raw.Model

data Location = Location
    { locBeginFile :: FilePath
    , locEndFile   :: FilePath
    , locBeginLine :: Natural
    , locEndLine   :: Natural
    , locBeginCol  :: Natural
    , locEndCol    :: Natural }
    deriving (Eq, Show)

rawLocation :: Location -> Raw.Location
rawLocation = undefined

fromRawLocation :: Raw.Location -> Location
fromRawLocation = undefined

data SolveResult = Satisfiable Bool | Unsatisfiable Bool | Interrupted
    deriving (Eq, Show, Read)

exhausted :: SolveResult -> Bool
exhausted (Satisfiable b) = b
exhausted (Unsatisfiable b) = b
exhausted _ = False

rawSolveResult :: SolveResult -> Raw.SolveResult
rawSolveResult (Satisfiable e) =
    Raw.ResultSatisfiable .|. if e then Raw.ResultExhausted else zeroBits
rawSolveResult (Unsatisfiable e) =
    Raw.ResultUnsatisfiable .|. if e then Raw.ResultExhausted else zeroBits
rawSolveResult Interrupted = Raw.ResultInterrupted

fromRawSolveResult :: Raw.SolveResult -> SolveResult
fromRawSolveResult r
    | r == Raw.ResultSatisfiable .|. Raw.ResultExhausted = Satisfiable True
    | r == Raw.ResultSatisfiable = Satisfiable False
    | r == Raw.ResultUnsatisfiable .|. Raw.ResultExhausted = Unsatisfiable True
    | r == Raw.ResultUnsatisfiable = Unsatisfiable False
    | r == Raw.ResultInterrupted = Interrupted
    | otherwise = error "Malformed clingo_solve_result_bitset_t"

wrapCBLogger :: MonadIO m
             => (ClingoWarning -> Text -> IO ())
             -> m (FunPtr (Raw.Logger ()))
wrapCBLogger f = liftIO $ Raw.mkCallbackLogger go
    where go :: Raw.ClingoWarning -> CString -> Ptr () -> IO ()
          go w str _ = peekCString str >>= \cstr ->
                           f (ClingoWarning w) (pack cstr)

newtype Statistics s = Statistics Raw.Statistics

newtype ProgramBuilder s = ProgramBuilder Raw.ProgramBuilder

newtype Configuration s = Configuration Raw.Configuration

newtype Backend s = Backend Raw.Backend

newtype SymbolicAtoms s = SymbolicAtoms Raw.SymbolicAtoms

newtype TheoryAtoms s = TheoryAtoms Raw.TheoryAtoms

newtype TruthValue = TruthValue { rawTruthValue :: Raw.TruthValue }

pattern TruthFree = TruthValue Raw.TruthFree
pattern TruthFalse = TruthValue Raw.TruthFalse
pattern TruthTrue = TruthValue Raw.TruthTrue

data IOPropagator s = IOPropagator
    { propagatorInit      :: Maybe (PropagateInit s -> IO ())
    , propagatorPropagate :: Maybe (PropagateCtrl s -> [Literal s] -> IO ())
    , propagatorUndo      :: Maybe (PropagateCtrl s -> [Literal s] -> IO ())
    , propagatorCheck     :: Maybe (PropagateCtrl s -> IO ())
    }

rawPropagator :: MonadIO m => IOPropagator s -> m (Raw.Propagator ())
rawPropagator p = Raw.Propagator <$> wrapCBInit (propagatorInit p)
                                 <*> wrapCBProp (propagatorPropagate p)
                                 <*> wrapCBUndo (propagatorUndo p)
                                 <*> wrapCBCheck (propagatorCheck p)

wrapCBInit :: MonadIO m
           => Maybe (PropagateInit s -> IO ())
           -> m (FunPtr (Raw.CallbackPropagatorInit ()))
wrapCBInit Nothing  = pure nullFunPtr
wrapCBInit (Just f) = liftIO $ Raw.mkCallbackPropagatorInit go
    where go :: Raw.PropagateInit -> Ptr () -> IO Raw.CBool
          go c _ = reraiseIO $ f (PropagateInit c)

wrapCBProp :: MonadIO m
           => Maybe (PropagateCtrl s -> [Literal s] -> IO ())
           -> m (FunPtr (Raw.CallbackPropagatorPropagate ()))
wrapCBProp Nothing  = pure nullFunPtr
wrapCBProp (Just f) = liftIO $ Raw.mkCallbackPropagatorPropagate go
    where go :: Raw.PropagateControl -> Ptr Raw.Literal -> CSize -> Ptr () 
             -> IO Raw.CBool
          go c lits len _ = 
              reraiseIO $ do
                  ls <- map Literal <$> peekArray (fromIntegral len) lits
                  f (PropagateCtrl c) ls

wrapCBUndo :: MonadIO m
           => Maybe (PropagateCtrl s -> [Literal s] -> IO ())
           -> m (FunPtr (Raw.CallbackPropagatorUndo ()))
wrapCBUndo = wrapCBProp

wrapCBCheck :: MonadIO m
            => Maybe (PropagateCtrl s -> IO ())
            -> m (FunPtr (Raw.CallbackPropagatorCheck ()))
wrapCBCheck Nothing  = pure nullFunPtr
wrapCBCheck (Just f) = liftIO $ Raw.mkCallbackPropagatorCheck go
    where go :: Raw.PropagateControl -> Ptr () -> IO Raw.CBool
          go c _ = reraiseIO $ f (PropagateCtrl c)

newtype PropagateCtrl s = PropagateCtrl Raw.PropagateControl

newtype PropagateInit s = PropagateInit Raw.PropagateInit

class AMVTree t where
    atArray :: Int -> t v -> Maybe (t v)
    atMap   :: Text -> t v -> Maybe (t v)
    value   :: t v -> Maybe v
