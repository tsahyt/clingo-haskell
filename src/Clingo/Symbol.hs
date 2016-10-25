module Clingo.Symbol
(
    Symbol,

    -- * Signature inspection
    signatureName,
    signatureArity,
    signatureHash,

    -- * Symbol creation
    createNumber,
    createSupremum,
    createInfimum,
    
    -- * Symbol inspection
    symbolHash,
    symbolType
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.Text (Text, pack)
import Numeric.Natural
import Foreign.C

import Clingo.Internal.Types
import Clingo.Internal.Utils
import qualified Clingo.Raw as Raw

import System.IO.Unsafe

{-
 -addString :: (MonadIO m, MonadThrow m) 
 -          => Clingo s -> Text -> m (InternalizedString s)
 -addString = undefined
 -}

signatureName :: Signature s -> Text
signatureName s = unsafePerformIO $
    pack <$> (peekCString . Raw.signatureName . rawSignature $ s)

signatureArity :: Signature s -> Natural
signatureArity = fromIntegral . Raw.signatureArity . rawSignature

signatureHash :: Signature s -> Integer
signatureHash = fromIntegral . Raw.symbolHash . rawSignature

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
