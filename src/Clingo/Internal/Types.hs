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
    SymbolicLiteral (..),
    rawSymLit,
    Signature (..),
    AsyncSolver (..),
    IterSolver (..),
    Model (..)
)
where

import Data.Text (Text)
import Foreign.Marshal.Utils
import qualified Clingo.Raw as Raw
import Clingo.Internal.Utils

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

rawPart :: Part s -> Raw.Part
rawPart = undefined

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

newtype AsyncSolver s = AsyncSolver Raw.AsyncSolver

newtype IterSolver s = IterSolver Raw.IterSolver

newtype Model s = Model Raw.Model
