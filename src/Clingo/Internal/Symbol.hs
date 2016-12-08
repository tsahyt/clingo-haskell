module Clingo.Internal.Symbol
(
    pureSymbol,
    symbolHash',
    symbolNumber',
    symbolName',
    symbolString',
    symbolArguments',
    prettySymbol',

    pureSignature,
    signatureArity',
    signatureHash',
    signatureName',

    createSignature',
    createNumber',
    createSupremum',
    createInfimum',
    createString',
    createFunction'
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

pureSymbol :: (MonadIO m, MonadThrow m) => Raw.Symbol -> m (Symbol s)
pureSymbol s = Symbol
    <$> pure s
    <*> pure (Raw.symbolType s)
    <*> pure (symbolHash' s)
    <*> symbolNumber' s
    <*> symbolName' s
    <*> symbolString' s
    <*> symbolArguments' s
    <*> prettySymbol' s

symbolHash' :: Raw.Symbol -> Integer
symbolHash' = fromIntegral . Raw.symbolHash

symbolNumber' :: (MonadIO m) => Raw.Symbol -> m (Maybe Integer)
symbolNumber' s = case Raw.symbolType s of
    Raw.SymNumber -> fmap fromIntegral <$> marshall1RT (Raw.symbolNumber s)
    _ -> return Nothing

symbolName' :: (MonadIO m) => Raw.Symbol -> m (Maybe Text)
symbolName' s = case Raw.symbolType s of
    Raw.SymFunction -> do
        x <- marshall1RT (Raw.symbolName s)
        case x of
            Nothing   -> return Nothing
            Just cstr -> liftIO $ (Just . pack) <$> peekCString cstr
    _ -> return Nothing

symbolString' :: (MonadIO m) => Raw.Symbol -> m (Maybe Text)
symbolString' s = case Raw.symbolType s of
    Raw.SymString -> do
        x <- marshall1RT (Raw.symbolString s)
        case x of
            Nothing   -> return Nothing
            Just cstr -> liftIO $ (Just . pack) <$> peekCString cstr
    _ -> return Nothing

symbolArguments' :: (MonadIO m, MonadThrow m) => Raw.Symbol -> m [Symbol s]
symbolArguments' s = case Raw.symbolType s of
    Raw.SymFunction -> mapM pureSymbol =<< marshall1A (Raw.symbolArguments s)
    _ -> return []

prettySymbol' :: (MonadIO m, MonadThrow m) => Raw.Symbol -> m Text
prettySymbol' s = do
    len <- marshall1 (Raw.symbolToStringSize s)
    str <- liftIO $ allocaArray (fromIntegral len) $ \ptr -> do
        b <- Raw.symbolToString s ptr len
        x <- peekCString ptr
        checkAndThrow b
        return x
    return (pack str)

pureSignature :: MonadIO m => Raw.Signature -> m (Signature s)
pureSignature s = Signature
    <$> pure s
    <*> pure (signatureArity' s)
    <*> signatureName' s
    <*> pure (signatureHash' s)

signatureName' :: MonadIO m => Raw.Signature -> m Text
signatureName' s = liftIO $
    pack <$> (peekCString . Raw.signatureName $ s)

signatureArity' :: Raw.Signature -> Natural
signatureArity' = fromIntegral . Raw.signatureArity

signatureHash' :: Raw.Signature -> Integer
signatureHash' = fromIntegral . Raw.symbolHash

createSignature' :: (MonadIO m, MonadThrow m)
                 => Text                     -- ^ Name
                 -> Natural                  -- ^ Arity
                 -> Bool                     -- ^ Positive
                 -> m (Signature s)
createSignature' name arity pos = pureSignature =<< marshall1 go
    where go x = withCString (unpack name) $ \cstr ->
                     Raw.signatureCreate cstr (fromIntegral arity) 
                                         (fromBool pos) x

createNumber' :: (MonadIO m, MonadThrow m, Integral a) => a -> m (Symbol s)
createNumber' a = pureSymbol =<< 
    marshall1V (Raw.symbolCreateNumber (fromIntegral a))

createSupremum' :: (MonadIO m, MonadThrow m) => m (Symbol s)
createSupremum' = pureSymbol =<< marshall1V Raw.symbolCreateSupremum

createInfimum' :: (MonadIO m, MonadThrow m) => m (Symbol s)
createInfimum' = pureSymbol =<< marshall1V Raw.symbolCreateInfimum

createString' :: (MonadIO m, MonadThrow m) => Text -> m (Symbol s)
createString' str = pureSymbol =<< marshall1 go
    where go = withCString (unpack str) . flip Raw.symbolCreateString

createFunction' :: (MonadIO m, MonadThrow m)
                => Text          -- ^ Function name
                -> [Symbol s]    -- ^ Arguments
                -> Bool          -- ^ Positive sign
                -> m (Symbol s)
createFunction' name args pos = pureSymbol =<< marshall1 go 
    where go x = withCString (unpack name) $ \cstr -> 
                     withArrayLen (map rawSymbol args) $ \len syms -> 
                         Raw.symbolCreateFunction cstr syms 
                             (fromIntegral len) (fromBool pos) x
