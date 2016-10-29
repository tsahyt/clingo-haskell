{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Configuration
(
    configurationRoot,
    configurationType,
    configurationDescription,

    -- * Functions to access arrays
    configurationArraySize,
    configurationArrayAt,
    
    -- * Functions to access maps
    configurationMapSize,
    configurationMapSubkeyName,
    configurationMapAt,

    -- * Functions to access values
    configurationValueIsAssigned,
    configurationValueGetSize,
    configurationValueGet,
    configurationValueSet
)
where

import Control.Monad.IO.Class

import Foreign
import Foreign.C

import Clingo.Raw.Enums
import Clingo.Raw.Types

foreign import ccall "clingo.h clingo_configuration_root" configurationRootFFI ::
    Configuration -> Ptr Identifier -> IO CBool
foreign import ccall "clingo.h clingo_configuration_type" configurationTypeFFI ::
    Configuration -> Identifier -> Ptr ConfigurationType -> IO CBool
foreign import ccall "clingo.h clingo_configuration_description" configurationDescriptionFFI ::
    Configuration -> Identifier -> Ptr CString -> IO CBool
foreign import ccall "clingo.h clingo_configuration_array_size" configurationArraySizeFFI ::
    Configuration -> Word64 -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_configuration_array_at" configurationArrayAtFFI ::
    Configuration -> Identifier -> CSize -> Ptr Identifier -> IO CBool
foreign import ccall "clingo.h clingo_configuration_map_size" configurationMapSizeFFI ::
    Configuration -> Identifier -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_configuration_map_subkey_name" configurationMapSubkeyNameFFI ::
    Configuration -> Identifier -> CSize -> Ptr CString -> IO CBool
foreign import ccall "clingo.h clingo_configuration_map_at" configurationMapAtFFI ::
    Configuration -> Identifier -> CString -> Ptr Identifier -> IO CBool
foreign import ccall "clingo.h clingo_configuration_value_is_assigned" configurationValueIsAssignedFFI ::
    Configuration -> Identifier -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_configuration_value_get_size" configurationValueGetSizeFFI ::
    Configuration -> Identifier -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_configuration_value_get" configurationValueGetFFI ::
    Configuration -> Identifier -> CString -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_configuration_value_set" configurationValueSetFFI ::
    Configuration -> Identifier -> CString -> IO CBool

configurationRoot :: MonadIO m => Configuration -> Ptr Identifier -> m CBool
configurationRoot a b = liftIO $ configurationRootFFI a b

configurationType :: MonadIO m => Configuration -> Identifier -> Ptr ConfigurationType -> m CBool
configurationType a b c = liftIO $ configurationTypeFFI a b c

configurationDescription :: MonadIO m => Configuration -> Identifier -> Ptr CString -> m CBool
configurationDescription a b c = liftIO $ configurationDescriptionFFI a b c

configurationArraySize :: MonadIO m => Configuration -> Word64 -> Ptr CSize -> m CBool
configurationArraySize a b c = liftIO $ configurationArraySizeFFI a b c

configurationArrayAt :: MonadIO m => Configuration -> Identifier -> CSize -> Ptr Identifier -> m CBool
configurationArrayAt a b c d = liftIO $ configurationArrayAtFFI a b c d

configurationMapSize :: MonadIO m => Configuration -> Identifier -> Ptr CSize -> m CBool
configurationMapSize a b c = liftIO $ configurationMapSizeFFI a b c

configurationMapSubkeyName :: MonadIO m => Configuration -> Identifier -> CSize -> Ptr CString -> m CBool
configurationMapSubkeyName a b c d = liftIO $ configurationMapSubkeyNameFFI a b c d

configurationMapAt :: MonadIO m => Configuration -> Identifier -> CString -> Ptr Identifier -> m CBool
configurationMapAt a b c d = liftIO $ configurationMapAtFFI a b c d

configurationValueIsAssigned :: MonadIO m => Configuration -> Identifier -> Ptr CBool -> m CBool
configurationValueIsAssigned a b c = liftIO $ configurationValueIsAssignedFFI a b c

configurationValueGetSize :: MonadIO m => Configuration -> Identifier -> Ptr CSize -> m CBool
configurationValueGetSize a b c = liftIO $ configurationValueGetSizeFFI a b c

configurationValueGet :: MonadIO m => Configuration -> Identifier -> CString -> CSize -> m CBool
configurationValueGet a b c d = liftIO $ configurationValueGetFFI a b c d

configurationValueSet :: MonadIO m => Configuration -> Identifier -> CString -> m CBool
configurationValueSet a b c = liftIO $ configurationValueSetFFI a b c
