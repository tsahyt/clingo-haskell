{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Internal.Types
(
    Clingo (..),
    Symbol (..),
    SymbolType (..),
    symbolType,
    pattern SymInfimum,
    pattern SymNumber,
    pattern SymString,
    pattern SymFunction,
    pattern SymSupremum,
    FunctionSymbol,
    functionSymbol,
    Part (..),
    rawPart,
    freeRawPart,
    SymbolicLiteral (..),
    rawSymLit,
    Literal (..),
    WeightedLiteral (..),
    rawWeightedLiteral,
    Atom (..),
    Signature (..),
    AsyncSolver (..),
    IterSolver (..),
    Model (..),
    ModelType (..),
    Location (..),
    rawLocation,
    fromRawLocation,
    SolveResult (..),
    rawSolveResult,
    fromRawSolveResult,
    exhausted,
    wrapCBLogger,
    Statistics (..),
    StatisticsType (..),
    ProgramBuilder (..),
    Configuration (..),
    ConfigurationType (..),
    Backend (..),
    SymbolicAtoms (..),
    TheoryAtoms (..),
    SymbolSelection (..),
    Assignment (..),
    selectAll,
    rawSymbolSelection,
    TruthValue (..),
    pattern TruthFree,
    pattern TruthFalse,
    pattern TruthTrue
)
where

import Control.Monad.IO.Class
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

newtype SymbolType = SymbolType Raw.SymbolType
    deriving Eq

pattern SymInfimum = SymbolType Raw.SymInfimum
pattern SymNumber = SymbolType Raw.SymNumber
pattern SymString = SymbolType Raw.SymString
pattern SymFunction = SymbolType Raw.SymFunction
pattern SymSupremum = SymbolType Raw.SymSupremum

symbolType :: Symbol s -> SymbolType
symbolType = SymbolType . Raw.symbolType . rawSymbol

newtype FunctionSymbol s = FuncSym { unFuncSym :: Symbol s }
    deriving (Eq, Ord)

functionSymbol :: Symbol s -> Maybe (FunctionSymbol s)
functionSymbol s = case symbolType s of
    SymFunction -> Just $ FuncSym s
    _ -> Nothing

class Signed a where
    positive :: a -> Bool
    positive = not . negative

    negative :: a -> Bool
    negative = not . positive

instance Signed Bool where
    positive x = x

instance Signed (FunctionSymbol s) where
    positive s = unsafePerformIO $ 
        toBool <$> marshall1 (Raw.symbolIsPositive . rawSymbol . unFuncSym $ s)
    negative s = unsafePerformIO $ 
        toBool <$> marshall1 (Raw.symbolIsNegative . rawSymbol . unFuncSym $ s)

data Part s = Part
    { partName   :: Text
    , partParams :: [Symbol s] }

rawPart :: Part s -> IO Raw.Part
rawPart p = Raw.Part <$> newCString (unpack (partName p))
                     <*> newArray (map rawSymbol . partParams $ p)
                     <*> pure (fromIntegral (length . partParams $ p))

freeRawPart :: Raw.Part -> IO ()
freeRawPart p = do
    free (Raw.partName p)
    free (Raw.partParams p)

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

newtype Literal s = Literal { rawLiteral :: Raw.Literal }

data WeightedLiteral s = WeightedLiteral (Literal s) Integer

rawWeightedLiteral :: WeightedLiteral s -> Raw.WeightedLiteral
rawWeightedLiteral (WeightedLiteral l w) = 
    Raw.WeightedLiteral (rawLiteral l) (fromIntegral w)

newtype Atom s = Atom { rawAtom :: Raw.Atom }

newtype Signature s = Signature { rawSignature :: Raw.Signature }

instance Eq (Signature s) where
    (Signature a) == (Signature b) = toBool (Raw.signatureIsEqualTo a b)

instance Ord (Signature s) where
    (Signature a) <= (Signature b) = toBool (Raw.signatureIsLessThan a b)

instance Signed (Signature s) where
    positive = toBool . Raw.signatureIsPositive . rawSignature
    negative = toBool . Raw.signatureIsNegative . rawSignature

newtype AsyncSolver s = AsyncSolver Raw.AsyncSolver

newtype IterSolver s = IterSolver Raw.IterSolver

newtype Model s = Model Raw.Model

newtype ModelType = ModelType Raw.ModelType

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

newtype StatisticsType = StatisticsType Raw.StatisticsType

newtype ProgramBuilder s = ProgramBuilder Raw.ProgramBuilder

newtype Configuration s = Configuration Raw.Configuration

newtype ConfigurationType = ConfigurationType Raw.ConfigurationType

newtype Backend s = Backend Raw.Backend

newtype SymbolicAtoms s = SymbolicAtoms Raw.SymbolicAtoms

newtype TheoryAtoms s = TheoryAtoms Raw.TheoryAtoms

newtype Assignment s = Assignment Raw.Assignment

data SymbolSelection = SymbolSelection 
    { selectCSP     :: Bool
    , selectShown   :: Bool
    , selectAtoms   :: Bool
    , selectTerms   :: Bool
    , selectExtra   :: Bool
    , useComplement :: Bool }
    deriving (Eq, Show, Ord)

selectAll :: SymbolSelection
selectAll = SymbolSelection True True True True True False

rawSymbolSelection :: SymbolSelection -> Raw.ShowFlag
rawSymbolSelection s = foldr ((.|.) . fst) zeroBits . filter snd $
    [ (Raw.ShowCSP, selectCSP s)
    , (Raw.ShowShown, selectShown s)
    , (Raw.ShowAtoms, selectAtoms s)
    , (Raw.ShowTerms, selectTerms s)
    , (Raw.ShowExtra, selectExtra s)
    , (Raw.ShowComplement, useComplement s) ]

newtype TruthValue = TruthValue { rawTruthValue :: Raw.TruthValue }

pattern TruthFree = TruthValue Raw.TruthFree
pattern TruthFalse = TruthValue Raw.TruthFalse
pattern TruthTrue = TruthValue Raw.TruthTrue
