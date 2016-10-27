{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Symbol
(
    addString,
    parseTerm,

    -- * Signatures
    signatureCreate,
    signatureName,
    signatureArity,
    signatureIsPositive,
    signatureIsNegative,
    signatureIsEqualTo,
    signatureIsLessThan,
    signatureHash,

    -- * Symbol construction
    symbolCreateNumber,
    symbolCreateSupremum,
    symbolCreateInfimum,
    symbolCreateString,
    symbolCreateId,
    symbolCreateFunction,

    -- * Symbol inspection
    symbolNumber,
    symbolName,
    symbolString,
    symbolIsPositive,
    symbolIsNegative,
    symbolArguments,
    symbolType,
    symbolToStringSize,
    symbolToString,

    -- * Symbol comparison
    symbolIsEqualTo,
    symbolIsLessThan,
    symbolHash
)
where

import Control.Monad.IO.Class
import Foreign
import Foreign.C
import Clingo.Raw.Types
import Clingo.Raw.Enums

foreign import ccall "clingo.h clingo_add_string" addStringFFI ::
    CString -> Ptr CString -> IO CBool
foreign import ccall "clingo.h clingo_parse_term" parseTermFFI ::
    CString -> FunPtr (Logger a) -> Ptr a -> CUInt -> Ptr Symbol -> IO CBool
foreign import ccall "clingo.h clingo_signature_create" signatureCreateFFI ::
    CString -> Word32 -> CBool -> Ptr Signature -> IO CBool
foreign import ccall "clingo.h clingo_signature_name" signatureNameFFI ::
    Signature -> CString
foreign import ccall "clingo.h clingo_signature_arity" signatureArityFFI ::
    Signature -> Word32
foreign import ccall "clingo.h clingo_signature_is_positive" 
    signatureIsPositiveFFI :: Signature -> CBool
foreign import ccall "clingo.h clingo_signature_is_negative" 
    signatureIsNegativeFFI :: Signature -> CBool
foreign import ccall "clingo.h clingo_signature_is_equal_to" 
    signatureIsEqualToFFI :: Signature -> Signature -> CBool
foreign import ccall "clingo.h clingo_signature_is_less_than" 
    signatureIsLessThanFFI :: Signature -> Signature -> CBool
foreign import ccall "clingo.h clingo_signature_hash" signatureHashFFI ::
    Signature -> CSize
foreign import ccall "clingo.h clingo_symbol_create_number" 
    symbolCreateNumberFFI :: CInt -> Ptr Symbol -> IO ()
foreign import ccall "clingo.h clingo_symbol_create_supremum" 
    symbolCreateSupremumFFI :: Ptr Symbol -> IO ()
foreign import ccall "clingo.h clingo_symbol_create_infimum" 
    symbolCreateInfimumFFI :: Ptr Symbol -> IO ()
foreign import ccall "clingo.h clingo_symbol_create_string" 
    symbolCreateStringFFI :: CString -> Ptr Symbol -> IO CBool
foreign import ccall "clingo.h clingo_symbol_create_id" symbolCreateIdFFI ::
    CString -> CBool -> Ptr Symbol -> IO CBool
foreign import ccall "clingo.h clingo_symbol_create_function" 
    symbolCreateFunctionFFI :: CString -> Ptr Symbol -> CSize -> CBool 
                            -> Ptr Symbol -> IO CBool
foreign import ccall "clingo.h clingo_symbol_number" symbolNumberFFI ::
    Symbol -> Ptr CInt -> IO CBool
foreign import ccall "clingo.h clingo_symbol_name" symbolNameFFI ::
    Symbol -> Ptr CString -> IO CBool
foreign import ccall "clingo.h clingo_symbol_string" symbolStringFFI ::
    Symbol -> Ptr CString -> IO CBool
foreign import ccall "clingo.h clingo_symbol_is_positive" symbolIsPositiveFFI ::
    Symbol -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_symbol_is_negative" symbolIsNegativeFFI ::
    Symbol -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_symbol_arguments" symbolArgumentsFFI ::
    Symbol -> Ptr (Ptr Symbol) -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_symbol_type" symbolTypeFFI ::
    Symbol -> SymbolType
foreign import ccall "clingo.h clingo_symbol_to_string_size" 
    symbolToStringSizeFFI :: Symbol -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_symbol_to_string" 
    symbolToStringFFI :: Symbol -> Ptr CChar -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_symbol_is_equal_to" symbolIsEqualToFFI ::
    Symbol -> Symbol -> CBool
foreign import ccall "clingo.h clingo_symbol_is_less_than" 
    symbolIsLessThanFFI :: Symbol -> Symbol -> CBool
foreign import ccall "clingo.h clingo_symbol_hash" symbolHashFFI ::
    Symbol -> CSize

addString :: MonadIO m => CString -> Ptr CString -> m CBool
addString a b = liftIO $ addStringFFI a b

parseTerm :: MonadIO m => CString -> FunPtr (Logger a) -> Ptr a -> CUInt 
                       -> Ptr Symbol -> m CBool
parseTerm a b c d e = liftIO $ parseTermFFI a b c d e

signatureCreate :: MonadIO m => CString -> Word32 -> CBool -> Ptr Signature 
                             -> m CBool
signatureCreate a b c d = liftIO $ signatureCreateFFI a b c d

signatureName :: Signature -> CString
signatureName = signatureNameFFI

signatureArity :: Signature -> Word32
signatureArity = signatureArityFFI

signatureIsPositive :: Signature -> CBool
signatureIsPositive = signatureIsPositiveFFI

signatureIsNegative :: Signature -> CBool
signatureIsNegative = signatureIsNegativeFFI

signatureIsEqualTo :: Signature -> Signature -> CBool
signatureIsEqualTo = signatureIsEqualToFFI

signatureIsLessThan :: Signature -> Signature -> CBool
signatureIsLessThan = signatureIsLessThanFFI

signatureHash :: Signature -> CSize
signatureHash = signatureHashFFI

symbolCreateNumber :: MonadIO m => CInt -> Ptr Symbol -> m ()
symbolCreateNumber a b = liftIO $ symbolCreateNumberFFI a b

symbolCreateSupremum :: MonadIO m => Ptr Symbol -> m ()
symbolCreateSupremum a = liftIO $ symbolCreateSupremumFFI a

symbolCreateInfimum :: MonadIO m => Ptr Symbol -> m ()
symbolCreateInfimum a = liftIO $ symbolCreateInfimumFFI a

symbolCreateString :: MonadIO m => CString -> Ptr Symbol -> m CBool
symbolCreateString a b = liftIO $ symbolCreateStringFFI a b

symbolCreateId :: MonadIO m => CString -> CBool -> Ptr Symbol -> m CBool
symbolCreateId a b c = liftIO $ symbolCreateIdFFI a b c

symbolCreateFunction :: MonadIO m => CString -> Ptr Symbol -> CSize -> CBool 
                                  -> Ptr Symbol -> m CBool
symbolCreateFunction a b c d e = liftIO $ symbolCreateFunctionFFI a b c d e

symbolNumber :: MonadIO m => Symbol -> Ptr CInt -> m CBool
symbolNumber a b = liftIO $ symbolNumberFFI a b

symbolName :: MonadIO m => Symbol -> Ptr CString -> m CBool
symbolName a b = liftIO $ symbolNameFFI a b

symbolString :: MonadIO m => Symbol -> Ptr CString -> m CBool
symbolString a b = liftIO $ symbolStringFFI a b

symbolIsPositive :: MonadIO m => Symbol -> Ptr CBool -> m CBool
symbolIsPositive a b = liftIO $ symbolIsPositiveFFI a b

symbolIsNegative :: MonadIO m => Symbol -> Ptr CBool -> m CBool
symbolIsNegative a b = liftIO $ symbolIsNegativeFFI a b

symbolArguments :: MonadIO m => Symbol -> Ptr (Ptr Symbol) -> Ptr CSize 
                             -> m CBool
symbolArguments a b c = liftIO $ symbolArgumentsFFI a b c

symbolType :: Symbol -> SymbolType
symbolType = symbolTypeFFI

symbolToStringSize :: MonadIO m => Symbol -> Ptr CSize -> m CBool
symbolToStringSize a b = liftIO $ symbolToStringSizeFFI a b

symbolToString :: MonadIO m => Symbol -> Ptr CChar -> CSize -> m CBool
symbolToString a b c = liftIO $ symbolToStringFFI a b c

symbolIsEqualTo :: Symbol -> Symbol -> CBool
symbolIsEqualTo = symbolIsEqualToFFI

symbolIsLessThan :: Symbol -> Symbol -> CBool
symbolIsLessThan = symbolIsLessThanFFI

symbolHash :: Symbol -> CSize
symbolHash = symbolHashFFI
