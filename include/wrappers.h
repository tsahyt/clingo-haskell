#ifndef WRAPPERS_H_4T9ZPQBM
#define WRAPPERS_H_4T9ZPQBM

bool clingo_control_register_propagator_ptr(
        clingo_control_t *control, clingo_propagator_t *propagator, 
        void *data, bool sequential);

bool clingo_control_register_observer_ptr(
        clingo_control_t *control, clingo_ground_program_observer_t *obs,
        void *data);

#endif /* end of include guard: WRAPPERS_H_4T9ZPQBM */
