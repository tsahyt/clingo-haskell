module Clingo.Internal.Types
(
    Clingo (..),
    Part (..),
    rawPart,
    SymbolicLiteral (..),
    rawSymLit
)
where

import Data.Text (Text)
import qualified Clingo.Raw as Raw

newtype Clingo s = Clingo Raw.Control

data Part = Part
    { partName   :: Text
    , partParams :: [()] }

rawPart :: Part -> Raw.Part
rawPart = undefined

data SymbolicLiteral = SymLit
    { symLitSymbol   :: ()
    , symLitPositive :: Bool }

rawSymLit :: SymbolicLiteral -> Raw.SymbolicLiteral
rawSymLit = undefined
