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
