{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Configuration
(
    configurationRoot,
    configurationType,
    configurationDescription,

    -- * Functions to access arrays
    configurationArraySize,
    configurationArrayAt,
    statisticsArraySize,
    statisticsArrayAt,
    
    -- * Functions to access maps
    configurationMapSize,
    configurationMapSubkeyName,
    configurationMapAt,
    statisticsMapSize,
    statisticsMapSubkeyName,
    statisticsMapAt,

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
    Ptr Configuration -> Ptr Identifier -> IO CBool
foreign import ccall "clingo.h clingo_configuration_type" configurationTypeFFI ::
    Ptr Configuration -> Identifier -> Ptr ConfigurationType -> IO CBool
foreign import ccall "clingo.h clingo_configuration_description" configurationDescriptionFFI ::
    Ptr Configuration -> Identifier -> Ptr CString -> IO CBool
foreign import ccall "clingo.h clingo_configuration_array_size" configurationArraySizeFFI ::
    Ptr Configuration -> Identifier -> Word64 -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_configuration_array_at" configurationArrayAtFFI ::
    Ptr Configuration -> Identifier -> CSize -> Ptr Identifier -> IO CBool
foreign import ccall "clingo.h clingo_statistics_array_size" statisticsArraySizeFFI ::
    Ptr Statistics -> Word64 -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_statistics_array_at" statisticsArrayAtFFI ::
    Ptr Statistics -> Word64 -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_configuration_map_size" configurationMapSizeFFI ::
    Ptr Configuration -> Identifier -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_configuration_map_subkey_name" configurationMapSubkeyNameFFI ::
    Ptr Configuration -> Identifier -> CSize -> Ptr CString -> IO CBool
foreign import ccall "clingo.h clingo_configuration_map_at" configurationMapAtFFI ::
    Ptr Configuration -> Identifier -> CString -> Ptr Identifier -> IO CBool
foreign import ccall "clingo.h clingo_statistics_map_size" statisticsMapSizeFFI ::
    Ptr Statistics -> Word64 -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_statistics_map_subkey_name" statisticsMapSubkeyNameFFI ::
    Ptr Statistics -> Word64 -> CSize -> Ptr CString -> IO CBool
foreign import ccall "clingo.h clingo_statistics_map_at" statisticsMapAtFFI ::
    Ptr Statistics -> Word64 -> CString -> Ptr Word64 -> IO CBool
foreign import ccall "clingo.h clingo_configuration_value_is_assigned" configurationValueIsAssignedFFI ::
    Ptr Configuration -> Identifier -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_configuration_value_get_size" configurationValueGetSizeFFI ::
    Ptr Configuration -> Identifier -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_configuration_value_get" configurationValueGetFFI ::
    Ptr Configuration -> Identifier -> CString -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_configuration_value_set" configurationValueSetFFI ::
    Ptr Configuration -> Identifier -> CString -> IO CBool

configurationRoot :: MonadIO m => Ptr Configuration -> Ptr Identifier -> m CBool
configurationRoot a b = liftIO $ configurationRootFFI a b

configurationType :: MonadIO m => Ptr Configuration -> Identifier -> Ptr ConfigurationType -> m CBool
configurationType a b c = liftIO $ configurationTypeFFI a b c

configurationDescription :: MonadIO m => Ptr Configuration -> Identifier -> Ptr CString -> m CBool
configurationDescription a b c = liftIO $ configurationDescriptionFFI a b c

configurationArraySize :: MonadIO m => Ptr Configuration -> Identifier -> Word64 -> Ptr CSize -> m CBool
configurationArraySize a b c d = liftIO $ configurationArraySizeFFI a b c d

configurationArrayAt :: MonadIO m => Ptr Configuration -> Identifier -> CSize -> Ptr Identifier -> m CBool
configurationArrayAt a b c d = liftIO $ configurationArrayAtFFI a b c d

statisticsArraySize :: MonadIO m => Ptr Statistics -> Word64 -> Ptr CSize -> m CBool
statisticsArraySize a b c = liftIO $ statisticsArraySizeFFI a b c

statisticsArrayAt :: MonadIO m => Ptr Statistics -> Word64 -> Ptr CSize -> m CBool
statisticsArrayAt a b c = liftIO $ statisticsArrayAtFFI a b c

configurationMapSize :: MonadIO m => Ptr Configuration -> Identifier -> Ptr CSize -> m CBool
configurationMapSize a b c = liftIO $ configurationMapSizeFFI a b c

configurationMapSubkeyName :: MonadIO m => Ptr Configuration -> Identifier -> CSize -> Ptr CString -> m CBool
configurationMapSubkeyName a b c d = liftIO $ configurationMapSubkeyNameFFI a b c d

configurationMapAt :: MonadIO m => Ptr Configuration -> Identifier -> CString -> Ptr Identifier -> m CBool
configurationMapAt a b c d = liftIO $ configurationMapAtFFI a b c d

statisticsMapSize :: MonadIO m => Ptr Statistics -> Word64 -> Ptr CSize -> m CBool
statisticsMapSize a b c = liftIO $ statisticsMapSizeFFI a b c

statisticsMapSubkeyName :: MonadIO m => Ptr Statistics -> Word64 -> CSize -> Ptr CString -> m CBool
statisticsMapSubkeyName a b c d = liftIO $ statisticsMapSubkeyNameFFI a b c d

statisticsMapAt :: MonadIO m => Ptr Statistics -> Word64 -> CString -> Ptr Word64 -> m CBool
statisticsMapAt a b c d = liftIO $ statisticsMapAtFFI a b c d

configurationValueIsAssigned :: MonadIO m => Ptr Configuration -> Identifier -> Ptr CBool -> m CBool
configurationValueIsAssigned a b c = liftIO $ configurationValueIsAssignedFFI a b c

configurationValueGetSize :: MonadIO m => Ptr Configuration -> Identifier -> Ptr CSize -> m CBool
configurationValueGetSize a b c = liftIO $ configurationValueGetSizeFFI a b c

configurationValueGet :: MonadIO m => Ptr Configuration -> Identifier -> CString -> CSize -> m CBool
configurationValueGet a b c d = liftIO $ configurationValueGetFFI a b c d

configurationValueSet :: MonadIO m => Ptr Configuration -> Identifier -> CString -> m CBool
configurationValueSet a b c = liftIO $ configurationValueSetFFI a b c
