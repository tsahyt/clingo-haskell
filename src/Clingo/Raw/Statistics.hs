{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Statistics
(
    statisticsRoot,
    statisticsType,

    -- * Array Access
    statisticsArraySize,
    statisticsArrayAt,

    -- * Map Access
    statisticsMapSize,
    statisticsMapSubkeyName,
    statisticsMapAt,

    -- * Value Access
    statisticsValueGet
)
where

import Control.Monad.IO.Class
import Foreign
import Foreign.C

import Clingo.Raw.Enums
import Clingo.Raw.Types

foreign import ccall "clingo.h clingo_statistics_root" statisticsRootFFI ::
    Ptr Statistics -> Ptr Word64 -> IO CBool
foreign import ccall "clingo.h clingo_statistics_type" statisticsTypeFFI ::
    Ptr Statistics -> Word64 -> Ptr StatisticsType -> IO CBool
foreign import ccall "clingo.h clingo_statistics_array_size" 
    statisticsArraySizeFFI :: Ptr Statistics -> Word64 -> Ptr Word64 -> IO CBool
foreign import ccall "clingo.h clingo_statistics_array_at" 
    statisticsArrayAtFFI :: Ptr Statistics -> Word64 -> CSize -> Ptr Word64
                           -> IO CBool
foreign import ccall "clingo.h clingo_statistics_map_size" 
    statisticsMapSizeFFI :: Ptr Statistics -> Word64 -> Ptr CSize -> IO CBool
foreign import ccall "clingo.h clingo_statistics_map_subkey_name" 
    statisticsMapSubkeyNameFFI :: Ptr Statistics -> Word64 -> CSize 
                               -> Ptr CString -> IO CBool
foreign import ccall "clingo.h clingo_statistics_map_at" statisticsMapAtFFI ::
    Ptr Statistics -> Word64 -> Ptr CString -> Ptr Word64 -> IO CBool
foreign import ccall "clingo.h clingo_statistics_value_get" 
    statisticsValueGetFFI :: Ptr Statistics -> Word64 -> Ptr CString 
                          -> Ptr Word64 -> IO CBool

statisticsRoot :: MonadIO m => Ptr Statistics -> Ptr Word64 -> m CBool
statisticsRoot a b = liftIO $ statisticsRootFFI a b

statisticsType :: MonadIO m => Ptr Statistics -> Word64 -> Ptr StatisticsType 
               -> m CBool
statisticsType a b c = liftIO $ statisticsTypeFFI a b c

statisticsArraySize :: MonadIO m => Ptr Statistics -> Word64 -> Ptr Word64 
                    -> m CBool
statisticsArraySize a b c = liftIO $ statisticsArraySizeFFI a b c

statisticsArrayAt :: MonadIO m => Ptr Statistics -> Word64 -> CSize 
                  -> Ptr Word64 -> m CBool
statisticsArrayAt a b c d = liftIO $ statisticsArrayAtFFI a b c d

statisticsMapSize :: MonadIO m => Ptr Statistics -> Word64 -> Ptr CSize 
                  -> m CBool
statisticsMapSize a b c = liftIO $ statisticsMapSizeFFI a b c

statisticsMapSubkeyName :: MonadIO m => Ptr Statistics -> Word64 -> CSize 
                        -> Ptr CString -> m CBool
statisticsMapSubkeyName a b c d = liftIO $ statisticsMapSubkeyNameFFI a b c d

statisticsMapAt :: MonadIO m => Ptr Statistics -> Word64 -> Ptr CString 
                -> Ptr Word64 -> m CBool
statisticsMapAt a b c d = liftIO $ statisticsMapAtFFI a b c d

statisticsValueGet :: MonadIO m => Ptr Statistics -> Word64 -> Ptr CString 
                   -> Ptr Word64 -> m CBool
statisticsValueGet a b c d = liftIO $ statisticsValueGetFFI a b c d
