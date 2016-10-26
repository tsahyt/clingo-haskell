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
    createString,
    createFunction,
    createId,
    
    -- * Symbol inspection
    symbolHash,
    symbolType
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.Text (Text, pack, unpack)
import Numeric.Natural
import Foreign.C
import Foreign

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

createString :: (MonadIO m, MonadThrow m) => Clingo s -> Text -> m (Symbol s)
createString _ str = Symbol <$> marshall1 go
    where go = withCString (unpack str) . flip Raw.symbolCreateString

createFunction :: (MonadIO m, MonadThrow m) 
               => Clingo s
               -> Text
               -> [Symbol s]
               -> Bool
               -> m (Symbol s)
createFunction _ name args pos = Symbol <$> marshall1 go 
    where go x = withCString (unpack name) $ \cstr -> 
                     withArrayLen (map rawSymbol args) $ \len syms -> 
                         Raw.symbolCreateFunction cstr syms 
                             (fromIntegral len) (fromBool pos) x

createId :: (MonadIO m, MonadThrow m) 
         => Clingo s -> Text -> Bool -> m (Symbol s)
createId c t = createFunction c t []

symbolHash :: Symbol s -> Integer
symbolHash = fromIntegral . Raw.symbolHash . rawSymbol

symbolNumber :: (MonadIO m, MonadThrow m) => Symbol s -> m (Maybe Integer)
symbolNumber s = fmap fromIntegral <$> 
    marshall1RT (Raw.symbolNumber (rawSymbol s))

symbolName :: (MonadIO m, MonadThrow m) => Symbol s -> m (Maybe Text)
symbolName s = do
    x <- marshall1RT (Raw.symbolName (rawSymbol s))
    case x of
        Nothing   -> return Nothing
        Just cstr -> liftIO $ (Just . pack) <$> peekCString cstr

symbolString :: (MonadIO m, MonadThrow m) => Symbol s -> m (Maybe Text)
symbolString s = do
    x <- marshall1RT (Raw.symbolString (rawSymbol s))
    case x of
        Nothing   -> return Nothing
        Just cstr -> liftIO $ (Just . pack) <$> peekCString cstr

symbolArguments :: (MonadIO m, MonadThrow m) => Symbol s -> m [Symbol s]
symbolArguments s = map Symbol <$> 
    marshall1A (Raw.symbolArguments (rawSymbol s))

prettySymbol :: (MonadIO m, MonadThrow m) => Symbol s -> m Text
prettySymbol s = do
    len <- marshall1 (Raw.symbolSymbolToStringSize (rawSymbol s))
    str <- liftIO $ alloca $ \ptr -> do
        b <- Raw.symbolSymbolToString (rawSymbol s) ptr len
        x <- peekCString ptr
        checkAndThrow b
        return x
    return (pack str)
