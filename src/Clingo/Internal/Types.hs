{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Internal.Types
(
    Clingo (..),
    Symbol (..),
    SymbolType (..),
    pattern SymInfimum,
    pattern SymNumber,
    pattern SymString,
    pattern SymFunction,
    pattern SymSupremum,
    Part (..),
    rawPart,
    SymbolicLiteral (..),
    rawSymLit
)
where

import Data.Text (Text)
import Foreign.Marshal.Utils
import qualified Clingo.Raw as Raw

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

rawSymLit :: SymbolicLiteral s -> Raw.SymbolicLiteral
rawSymLit sl = Raw.SymbolicLiteral
    { Raw.slitSymbol   = rawSymbol (symLitSymbol sl)
    , Raw.slitPositive = fromBool (symLitPositive sl) }
