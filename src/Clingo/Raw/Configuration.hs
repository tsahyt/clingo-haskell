module Clingo.Raw.Configuration
(
)
where

foreign import ccall "clingo.h clingo_configuration_root" configurationRootFFI ::
foreign import ccall "clingo.h clingo_configuration_type" configurationTypeFFI ::
foreign import ccall "clingo.h clingo_configuration_description" configurationDescriptionFFI ::
foreign import ccall "clingo.h clingo_configuration_array_size" configurationArraySizeFFI ::
foreign import ccall "clingo.h clingo_configuration_array_at" configurationArrayAtFFI ::
foreign import ccall "clingo.h clingo_statistics_array_size" statisticsArraySizeFFI ::
foreign import ccall "clingo.h clingo_statistics_array_at" statisticsArrayAtFFI ::
foreign import ccall "clingo.h clingo_configuration_map_size" configurationMapSizeFFI ::
foreign import ccall "clingo.h clingo_configuration_map_subkey_name" configurationMapSubkeyNameFFI ::
foreign import ccall "clingo.h clingo_configuration_map_at" configurationMapAtFFI ::
foreign import ccall "clingo.h clingo_statistics_map_size" statisticsMapSizeFFI ::
foreign import ccall "clingo.h clingo_statistics_map_subkey_name" statisticsMapSubkeyNameFFI ::
foreign import ccall "clingo.h clingo_statistics_map_at" statisticsMapAtFFI ::
foreign import ccall "clingo.h clingo_configuration_value_is_assigned" configurationValueIsAssignedFFI ::
foreign import ccall "clingo.h clingo_configuration_value_get_size" configurationValueGetSizeFFI ::
foreign import ccall "clingo.h clingo_configuration_value_get" configurationValueGetFFI ::
foreign import ccall "clingo.h clingo_configuration_value_set" configurationValueSetFFI ::
