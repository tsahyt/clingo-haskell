module Clingo.Symbol
(
    Symbol,

    -- * Symbol creation
    createNumber,
    createSupremum,
    createInfimum,
    
    symbolHash
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.Text (Text)

import Clingo.Internal.Types
import Clingo.Internal.Utils
import qualified Clingo.Raw as Raw

{-
 -addString :: (MonadIO m, MonadThrow m) 
 -          => Clingo s -> Text -> m (InternalizedString s)
 -addString = undefined
 -}

createNumber :: (Integral a, MonadIO m) => Clingo s -> a -> m (Symbol s)
createNumber _ a = Symbol <$> 
    marshall1V (Raw.symbolCreateNumber (fromIntegral a))

createSupremum :: MonadIO m => Clingo s -> m (Symbol s)
createSupremum _ = Symbol <$> 
    marshall1V Raw.symbolCreateSupremum

createInfimum :: MonadIO m => Clingo s -> m (Symbol s)
createInfimum _ = Symbol <$> 
    marshall1V Raw.symbolCreateSupremum

symbolHash :: Symbol s -> Integer
symbolHash = fromIntegral . Raw.symbolHash . rawSymbol
