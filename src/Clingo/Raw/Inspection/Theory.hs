module Clingo.Raw.Inspection.Theory
(
)
where

foreign import ccall "clingo.h clingo_theory_atoms_term_type" theoryAtomsTermTypeFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_term_number" theoryAtomsTermNumberFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_term_name" theoryAtomsTermNameFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_term_arguments" theoryAtomsTermArgumentsFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_term_to_string_size" theoryAtomsTermToStringSizeFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_term_to_string" theoryAtomsTermToStringFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_element_tuple" theoryAtomsElementTupleFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_element_condition" theoryAtomsElementConditionFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_element_condition_id" theoryAtomsElementConditionIdFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_element_to_string_size" theoryAtomsElementToStringSizeFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_element_to_string" theoryAtomsElementToStringFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_size" theoryAtomsSizeFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_atom_term" theoryAtomsAtomTermFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_atom_elements" theoryAtomsAtomElementsFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_atom_has_guard" theoryAtomsAtomHasGuardFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_atom_guard" theoryAtomsAtomGuardFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_atom_literal" theoryAtomsAtomLiteralFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_atom_to_string_size" theoryAtomsAtomToStringSizeFFI ::
foreign import ccall "clingo.h clingo_theory_atoms_atom_to_string" theoryAtomsAtomToStringFFI ::
