#include <clingo.h>

bool clingo_control_register_propagator_ptr(
        clingo_control_t *control, clingo_propagator_t *propagator, 
        void *data, bool sequential
        )
{
    clingo_propagator_t x = *propagator;
    return clingo_control_register_propagator(control, x, data, sequential);
}

bool clingo_control_register_observer_ptr(
        clingo_control_t *control, clingo_ground_program_observer_t *obs,
        void *data)
{
    clingo_ground_program_observer_t x = *obs;
    return clingo_control_register_observer(control, x, data);
}

typedef bool clingo_ground_callback_ptr_t (clingo_location_t *location, char const *name, clingo_symbol_t const *arguments, size_t arguments_size, void *data, clingo_symbol_callback_t *symbol_callback, void *symbol_callback_data);

typedef struct {
    clingo_ground_callback_ptr_t* user_callback;
    void* user_data;
} ground_payload_t;

bool wrapped_ground_callback(clingo_location_t location, char const *name, clingo_symbol_t const *arguments, size_t arguments_size, void *data, clingo_symbol_callback_t *symbol_callback, void *symbol_callback_data)
{
    ground_payload_t* env = (ground_payload_t*)data;
    clingo_location_t loc = location;
    env->user_callback(&loc, name, arguments, arguments_size, env->user_data, symbol_callback, symbol_callback_data);
}

bool clingo_control_ground_ptr(clingo_control_t *control, clingo_part_t const *parts, size_t parts_size, clingo_ground_callback_ptr_t *ground_callback, void *ground_callback_data)
{
    ground_payload_t callback_env = { ground_callback, ground_callback_data };
    return clingo_control_ground(control, parts, parts_size, &wrapped_ground_callback, (void*)&callback_env);
}
