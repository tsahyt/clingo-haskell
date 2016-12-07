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
    signatureName'
)
where

import Control.Monad.IO.Class

import Data.Text (Text, pack, unpack)
import Numeric.Natural
import Foreign.C
import Foreign

import Clingo.Internal.Types
import Clingo.Internal.Utils
import qualified Clingo.Raw as Raw

import System.IO.Unsafe

pureSymbol :: Raw.Symbol -> Clingo s (Symbol s)
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

symbolNumber' :: Raw.Symbol -> Clingo s (Maybe Integer)
symbolNumber' s = fmap fromIntegral <$> 
    marshall1RT (Raw.symbolNumber s)

symbolName' :: Raw.Symbol -> Clingo s (Maybe Text)
symbolName' s = do
    x <- marshall1RT (Raw.symbolName s)
    case x of
        Nothing   -> return Nothing
        Just cstr -> liftIO $ (Just . pack) <$> peekCString cstr

symbolString' :: Raw.Symbol -> Clingo s (Maybe Text)
symbolString' s = do
    x <- marshall1RT (Raw.symbolString s)
    case x of
        Nothing   -> return Nothing
        Just cstr -> liftIO $ (Just . pack) <$> peekCString cstr

symbolArguments' :: Raw.Symbol -> Clingo s [Symbol s]
symbolArguments' s = mapM pureSymbol =<< marshall1A (Raw.symbolArguments s)

prettySymbol' :: Raw.Symbol -> Clingo s Text
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
