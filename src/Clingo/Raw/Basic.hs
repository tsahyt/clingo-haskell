{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Basic
(
    errorString,
    errorCode,
    errorMessage,
    setError,
    warningString,
    version
)
where

import Foreign
import Foreign.C
import Foreign.C.String
import Control.Monad.IO.Class
import Clingo.Raw.Enums
import Clingo.Raw.Types

foreign import ccall "clingo.h clingo_error_string" errorStringFFI ::
    ClingoError -> IO CString
foreign import ccall "clingo.h clingo_error_code" errorCodeFFI ::
    IO ClingoError
foreign import ccall "clingo.h clingo_error_message" errorMessageFFI ::
    IO CString
foreign import ccall "clingo.h clingo_set_error" setErrorFFI ::
    ClingoError -> CString -> IO ()
foreign import ccall "clingo.h clingo_warning_string" warningStringFFI ::
    ClingoWarning -> IO CString
foreign import ccall "clingo.h clingo_version" versionFFI ::
    Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

errorString :: MonadIO m => ClingoError -> m CString
errorString = liftIO . errorStringFFI

errorCode :: MonadIO m => m ClingoError
errorCode = liftIO errorCodeFFI

errorMessage :: MonadIO m => m CString
errorMessage = liftIO errorMessageFFI

setError :: MonadIO m => ClingoError -> CString -> m ()
setError a b = liftIO $ setErrorFFI a b

warningString :: MonadIO m => ClingoWarning -> m CString
warningString = liftIO . warningStringFFI

version :: MonadIO m => Ptr CInt -> Ptr CInt -> Ptr CInt -> m ()
version a b c = liftIO $ versionFFI a b c
