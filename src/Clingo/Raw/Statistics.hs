module Clingo.Raw.Statistics
(
)
where

foreign import ccall "clingo.h clingo_statistics_root" statisticsRootFFI ::
foreign import ccall "clingo.h clingo_statistics_type" statisticsTypeFFI ::
foreign import ccall "clingo.h clingo_statistics_array_size" statisticsArraySizeFFI ::
foreign import ccall "clingo.h clingo_statistics_array_at" statisticsArrayAtFFI ::
foreign import ccall "clingo.h clingo_statistics_map_size" statisticsMapSizeFFI ::
foreign import ccall "clingo.h clingo_statistics_map_subkey_name" statisticsMapSubkeyNameFFI ::
foreign import ccall "clingo.h clingo_statistics_map_at" statisticsMapAtFFI ::
foreign import ccall "clingo.h clingo_statistics_value_get" statisticsValueGetFFI ::
