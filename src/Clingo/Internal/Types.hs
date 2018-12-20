{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Internal.Types
(
    IOSym (..),
    ClingoSetting (..),
    ClingoT(..),
    runClingoT,
    mapClingoT,
    Clingo,
    runClingo,
    liftC,
    askC,
    mkClingo,
    freeClingo,
    Signed (..),
    Symbol (..),
    Signature (..),
    Literal (..),
    WeightedLiteral (..),
    rawWeightedLiteral,
    fromRawWeightedLiteral,
    ExternalType (..),
    rawExtT,
    fromRawExtT,
    HeuristicType (..),
    rawHeuT,
    fromRawHeuT,
    negateLiteral,
    AspifLiteral (..),
    Atom (..),
    Model (..),
    Location (..),
    rawLocation,
    freeRawLocation,
    fromRawLocation,
    SolveResult (..),
    rawSolveResult,
    fromRawSolveResult,
    SolveMode (..),
    fromRawSolveMode,
    pattern SolveModeAsync,
    pattern SolveModeYield,
    Solver (..),
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
    negateTruth,
    IOPropagator (..),
    rawPropagator,
    PropagateCtrl (..),
    PropagateInit (..),
    AMVTree (..)
)
where

import Control.DeepSeq
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Data.Text (Text, pack)
import Data.Bits
import Data.Hashable
import Foreign
import Foreign.C
import GHC.Generics
import qualified Clingo.Raw as Raw
import Clingo.Internal.Utils
import Numeric.Natural

-- | A monad that serves as witness that data registered with a running solver
-- still exists and can be used.
newtype IOSym s a = IOSym { iosym :: IO a }
    deriving (Functor, Applicative, Monad, MonadMask, MonadThrow
             , MonadCatch, MonadIO, MonadFix, MonadPlus, Alternative )

-- | The 'Clingo' monad provides a base monad for computations utilizing the
-- clingo answer set solver. It uses an additional type parameter to ensure that
-- values that are managed by the solver can not leave scope. 
newtype ClingoT m s a = Clingo { clingo :: ReaderT Raw.Control m a }
    deriving (Functor, Applicative, Monad, MonadMask, MonadThrow
             , MonadCatch, MonadIO, MonadFix, MonadPlus, Alternative)

mapClingoT :: (m a -> n b) -> ClingoT m s a -> ClingoT n s b
mapClingoT f (Clingo k) = Clingo (mapReaderT f k)

type Clingo s a = ClingoT IO s a

instance MonadState st m => MonadState st (ClingoT m s) where
    get = liftC get
    put = liftC . put
    state = liftC . state

instance MonadError e m => MonadError e (ClingoT m s) where
    throwError = liftC . throwError
    catchError (Clingo k) f = Clingo $ catchError k (clingo <$> f)

instance MonadReader r m => MonadReader r (ClingoT m s) where
    ask = liftC ask
    local f (Clingo (ReaderT k)) = Clingo (ReaderT $ \ctrl -> local f (k ctrl))

instance MonadWriter w m => MonadWriter w (ClingoT m s) where
    writer = liftC . writer
    listen (Clingo k) = Clingo (listen k)
    pass (Clingo k) = Clingo (pass k)

-- | Run a clingo computation from an explicit handle. The handle must be
-- cleaned up manually afterwards, or on failure!
runClingoT :: Raw.Control -> ClingoT m s a -> m a
runClingoT ctrl a = runReaderT (clingo a) ctrl

runClingo :: Raw.Control -> Clingo s a -> IO a
runClingo ctrl k = runClingoT ctrl k

-- | A version of 'lift' for 'ClingoT'. Due to the additional @s@ parameter,
-- which must occur after the @m@ to define 'MonadSymbol' and 'MonadModel'
-- instances, 'ClingoT' cannot implement 'MonadTrans'.
liftC :: Monad m => m a -> ClingoT m s a
liftC k = Clingo (lift k)
{-# INLINE liftC #-}

-- | Get the control handle from the 'Clingo' monad. Arbitrarily unsafe things
-- can be done with this!
askC :: Monad m => ClingoT m s Raw.Control
askC = Clingo ask

-- | Data type to encapsulate the settings for clingo.
data ClingoSetting = ClingoSetting
    { clingoArgs   :: [String]
    , clingoLogger :: Maybe (ClingoWarning -> Text -> IO ())
    , msgLimit     :: Natural }

-- | Wrapper function to create a raw handle. Arbitrarily unsafe things can be
-- done with this! Note that the handle must be freed after you are done with it!
mkClingo :: MonadIO m => ClingoSetting -> m Raw.Control
mkClingo settings = liftIO $ do
    let argc = length (clingoArgs settings)
    argv <- liftIO $ mapM newCString (clingoArgs settings)
    ctrl <- marshal1 $ \x ->
        withArray argv $ \argvArr -> do
            logCB <- maybe (pure nullFunPtr) wrapCBLogger 
                         (clingoLogger settings)
            let argv' = case clingoArgs settings of
                            [] -> nullPtr
                            _  -> argvArr
            Raw.controlNew argv' (fromIntegral argc)
                           logCB nullPtr (fromIntegral . msgLimit $ settings) x
    liftIO $ mapM_ free argv
    pure ctrl

freeClingo :: MonadIO m => Raw.Control -> m ()
freeClingo = Raw.controlFree

data Symbol s = Symbol 
    { rawSymbol :: Raw.Symbol 
    , symType   :: Raw.SymbolType
    , symHash   :: Integer
    , symNum    :: Maybe Integer
    , symName   :: Maybe Text
    , symString :: Maybe Text
    , symArgs   :: [Symbol s]
    , symPretty :: Text
    }
    deriving (Generic)

instance NFData (Symbol s)

instance Eq (Symbol s) where
    a == b = toBool (Raw.symbolIsEqualTo (rawSymbol a) (rawSymbol b))

instance Ord (Symbol s) where
    a <= b = toBool (Raw.symbolIsLessThan (rawSymbol a) (rawSymbol b))

instance Hashable (Symbol s) where
    hashWithSalt s sym = hashWithSalt s (symHash sym)

class Signed a where
    positive :: a -> Bool
    positive = not . negative

    negative :: a -> Bool
    negative = not . positive

instance Signed Bool where
    positive x = x

data Signature s = Signature 
    { rawSignature :: Raw.Signature
    , sigArity     :: Natural
    , sigName      :: Text 
    , sigHash      :: Integer
    }

instance Eq (Signature s) where
    a == b = toBool (Raw.signatureIsEqualTo (rawSignature a) (rawSignature b))

instance Ord (Signature s) where
    a <= b = toBool (Raw.signatureIsLessThan (rawSignature a) (rawSignature b))

instance Signed (Signature s) where
    positive = toBool . Raw.signatureIsPositive . rawSignature
    negative = toBool . Raw.signatureIsNegative . rawSignature

instance Hashable (Signature s) where
    hashWithSalt s sig = hashWithSalt s (sigHash sig)

newtype Literal s = Literal { rawLiteral :: Raw.Literal }
    deriving (Ord, Show, Eq, NFData, Generic)

instance Hashable (Literal s)

instance Signed (Literal s) where
    positive = (> 0) . rawLiteral
    negative = (< 0) . rawLiteral

data WeightedLiteral s = WeightedLiteral (Literal s) Integer
    deriving (Eq, Show, Ord, Generic)

instance Hashable (WeightedLiteral s)
instance NFData (WeightedLiteral s)

rawWeightedLiteral :: WeightedLiteral s -> Raw.WeightedLiteral
rawWeightedLiteral (WeightedLiteral l w) = 
    Raw.WeightedLiteral (rawLiteral l) (fromIntegral w)

fromRawWeightedLiteral :: Raw.WeightedLiteral -> WeightedLiteral s
fromRawWeightedLiteral (Raw.WeightedLiteral l w) = 
    WeightedLiteral (Literal l) (fromIntegral w)

data ExternalType = ExtFree | ExtTrue | ExtFalse | ExtRelease
    deriving (Show, Eq, Ord, Enum, Read, Generic)

instance Hashable ExternalType

rawExtT :: ExternalType -> Raw.ExternalType
rawExtT ExtFree = Raw.ExternalFree
rawExtT ExtTrue = Raw.ExternalTrue
rawExtT ExtFalse = Raw.ExternalFalse
rawExtT ExtRelease = Raw.ExternalRelease

fromRawExtT :: Raw.ExternalType -> ExternalType
fromRawExtT Raw.ExternalFree = ExtFree
fromRawExtT Raw.ExternalTrue = ExtTrue
fromRawExtT Raw.ExternalFalse = ExtFalse
fromRawExtT Raw.ExternalRelease = ExtRelease
fromRawExtT _ = error "unknown external_type"

data HeuristicType = HeuristicLevel | HeuristicSign | HeuristicFactor 
                   | HeuristicInit  | HeuristicTrue | HeuristicFalse
    deriving (Show, Eq, Ord, Enum, Read, Generic)

instance Hashable HeuristicType

rawHeuT :: HeuristicType -> Raw.HeuristicType
rawHeuT HeuristicLevel = Raw.HeuristicLevel
rawHeuT HeuristicSign = Raw.HeuristicSign
rawHeuT HeuristicFactor = Raw.HeuristicFactor
rawHeuT HeuristicInit = Raw.HeuristicInit
rawHeuT HeuristicTrue = Raw.HeuristicTrue
rawHeuT HeuristicFalse = Raw.HeuristicFalse

fromRawHeuT :: Raw.HeuristicType -> HeuristicType
fromRawHeuT Raw.HeuristicLevel = HeuristicLevel
fromRawHeuT Raw.HeuristicSign = HeuristicSign
fromRawHeuT Raw.HeuristicFactor = HeuristicFactor
fromRawHeuT Raw.HeuristicInit = HeuristicInit
fromRawHeuT Raw.HeuristicTrue = HeuristicTrue
fromRawHeuT Raw.HeuristicFalse = HeuristicFalse
fromRawHeuT _ = error "unknown heuristic_type"

negateLiteral :: Literal s -> Literal s
negateLiteral (Literal a) = Literal (negate a)

newtype AspifLiteral s = AspifLiteral { rawAspifLiteral :: Raw.Literal }
    deriving (Ord, Show, Eq, NFData, Generic)

instance Hashable (AspifLiteral s)

instance Signed (AspifLiteral s) where
    positive = (> 0) . rawAspifLiteral
    negative = (< 0) . rawAspifLiteral

newtype Atom s = Atom { rawAtom :: Raw.Atom }
    deriving (Show, Ord, Eq, NFData, Generic)

instance Hashable (Atom s)

newtype Model s = Model Raw.Model

data Location = Location
    { locBeginFile :: FilePath
    , locEndFile   :: FilePath
    , locBeginLine :: Natural
    , locEndLine   :: Natural
    , locBeginCol  :: Natural
    , locEndCol    :: Natural }
    deriving (Eq, Show, Ord)

rawLocation :: Location -> IO Raw.Location
rawLocation l = Raw.Location 
    <$> newCString (locBeginFile l)
    <*> newCString (locEndFile l)
    <*> pure (fromIntegral (locBeginLine l))
    <*> pure (fromIntegral (locEndLine l))
    <*> pure (fromIntegral (locBeginCol l))
    <*> pure (fromIntegral (locEndCol l))

freeRawLocation :: Raw.Location -> IO ()
freeRawLocation l = do
    free (Raw.locBeginFile l)
    free (Raw.locEndFile l)

fromRawLocation :: Raw.Location -> IO Location
fromRawLocation l = Location
    <$> peekCString (Raw.locBeginFile l)
    <*> peekCString (Raw.locEndFile l)
    <*> pure (fromIntegral . Raw.locBeginLine $ l)
    <*> pure (fromIntegral . Raw.locEndLine $ l)
    <*> pure (fromIntegral . Raw.locBeginCol $ l)
    <*> pure (fromIntegral . Raw.locEndCol $ l)

data SolveResult = Satisfiable Bool | Unsatisfiable Bool | Interrupted
    deriving (Eq, Show, Read, Generic)

instance NFData SolveResult
instance Hashable SolveResult

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

newtype SolveMode = SolveMode { rawSolveMode :: Raw.SolveMode }
    deriving (Eq, Show, Read, Ord)

fromRawSolveMode :: Raw.SolveMode -> SolveMode
fromRawSolveMode = SolveMode

newtype Solver s = Solver Raw.SolveHandle

pattern SolveModeAsync = SolveMode Raw.SolveModeAsync
pattern SolveModeYield = SolveMode Raw.SolveModeYield

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
    deriving (Eq, Show, Ord)

pattern TruthFree = TruthValue Raw.TruthFree
pattern TruthFalse = TruthValue Raw.TruthFalse
pattern TruthTrue = TruthValue Raw.TruthTrue

negateTruth :: TruthValue -> TruthValue
negateTruth TruthFree = TruthFree
negateTruth TruthTrue = TruthFalse
negateTruth TruthFalse = TruthTrue
negateTruth _ = error "negateTruth: Invalid TruthValue"

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
