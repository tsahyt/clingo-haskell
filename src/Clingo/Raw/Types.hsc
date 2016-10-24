{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Types
(
    CBool,

    -- * Basic types
    Literal,
    Atom,
    Identifier,
    Weight,
    Logger,
    Location (..),

    -- * Symbols
    Signature,
    Symbol,
    SymbolicLiteral,

    -- * Model and Solving
    Model (..),
    SolveControl (..),
    IterSolver (..),
    AsyncSolver (..),

    -- * Symbolic and Theory Atoms
    SymbolicAtoms (..),
    SymbolicAtomIterator,
    TheoryAtoms (..),

    -- * Propagators
    PropagateInit (..),
    Assignment (..),
    PropagateControl (..),
    CallbackPropagatorInit,
    CallbackPropagatorPropagate,
    CallbackPropagatorUndo,
    CallbackPropagatorCheck,
    Propagator (..),

    -- * Program Builder
    WeightedLiteral (..),
    Backend (..),
    ProgramBuilder (..),

    -- * Configuration & Statistics
    Configuration (..),
    Statistics (..),

    -- * Program Inspection
    GroundProgramObserver (..),

    -- * Control
    Control (..),
    Part,
    CallbackSymbol,
    CallbackGround,
    CallbackModel,
    CallbackFinish
)
where

import Data.Int
import Data.Word
import Foreign
import Foreign.C

import Clingo.Raw.Enums

#include <clingo.h>

type CBool = #type bool

type Literal = #type clingo_literal_t
type Atom = #type clingo_atom_t
type Identifier = #type clingo_id_t
type Weight = #type clingo_weight_t

type Logger a = ClingoWarning -> Ptr CChar -> Ptr a -> IO ()

data Location = Location
    { locBeginFile :: Ptr CChar
    , locEndFile   :: Ptr CChar
    , locBeginLine :: #type size_t
    , locEndLine   :: #type size_t
    , locBeginCol  :: #type size_t
    , locEndCol    :: #type size_t }
    deriving (Eq, Show)

instance Storable Location where
    sizeOf _ = #{size clingo_location_t}
    alignment = sizeOf

    peek p = Location 
        <$> (#{peek clingo_location_t, begin_file} p)
        <*> (#{peek clingo_location_t, end_file} p)
        <*> (#{peek clingo_location_t, begin_line} p)
        <*> (#{peek clingo_location_t, end_line} p)
        <*> (#{peek clingo_location_t, begin_column} p)
        <*> (#{peek clingo_location_t, end_column} p)

    poke p l = do
        (#poke clingo_location_t, begin_file) p (locBeginFile l)
        (#poke clingo_location_t, end_file) p (locEndFile l)
        (#poke clingo_location_t, begin_line) p (locBeginLine l)
        (#poke clingo_location_t, end_line) p (locEndLine l)
        (#poke clingo_location_t, begin_column) p (locBeginCol l)
        (#poke clingo_location_t, end_column) p (locEndCol l)

type Signature = #type clingo_signature_t
type Symbol = #type clingo_symbol_t

data SymbolicLiteral = SymbolicLiteral
    { slitSymbol   :: Symbol
    , slitPositive :: CBool }

instance Storable SymbolicLiteral where
    sizeOf _ = #{size clingo_symbolic_literal_t}
    alignment = sizeOf
    peek p = SymbolicLiteral
         <$> (#{peek clingo_symbolic_literal_t, symbol} p)
         <*> (#{peek clingo_symbolic_literal_t, positive} p)

    poke p lit = do
        (#poke clingo_symbolic_literal_t, symbol) p (slitSymbol lit)
        (#poke clingo_symbolic_literal_t, positive) p (slitPositive lit)

newtype SolveControl = SolveControl (Ptr SolveControl)
newtype Model = Model (Ptr Model)
newtype IterSolver = IterSolver (Ptr IterSolver)
newtype AsyncSolver = AsyncSolver (Ptr AsyncSolver)
newtype SymbolicAtoms = SymbolicAtoms (Ptr SymbolicAtoms)

type SymbolicAtomIterator = #type clingo_symbolic_atom_iterator_t

newtype TheoryAtoms = TheoryAtoms (Ptr TheoryAtoms)
newtype PropagateInit = PropagateInit (Ptr PropagateInit)
newtype Assignment = Assignment (Ptr Assignment)
newtype PropagateControl = PropagateControl (Ptr PropagateControl)

type CallbackPropagatorInit a = 
    PropagateInit -> Ptr a -> IO (CBool)
type CallbackPropagatorPropagate a = 
    PropagateControl -> Ptr Literal -> CSize -> Ptr a -> IO CBool
type CallbackPropagatorUndo a = 
    PropagateControl -> Ptr Literal -> CSize -> Ptr a -> IO CBool
type CallbackPropagatorCheck a = 
    PropagateControl -> Ptr a -> IO CBool

data Propagator a = Propagator
    { propagatorInit      :: FunPtr (CallbackPropagatorInit a)
    , propagatorPropagate :: FunPtr (CallbackPropagatorPropagate a)
    , propagatorUndo      :: FunPtr (CallbackPropagatorUndo a)
    , propagatorCheck     :: FunPtr (CallbackPropagatorCheck a)
    }

instance Storable (Propagator a) where
    sizeOf _ = #{size clingo_propagator_t}
    alignment = sizeOf
    peek p = Propagator 
         <$> (#{peek clingo_propagator_t, init} p)
         <*> (#{peek clingo_propagator_t, propagate} p)
         <*> (#{peek clingo_propagator_t, undo} p)
         <*> (#{peek clingo_propagator_t, check} p)

    poke p prop = do
        (#poke clingo_propagator_t, init) p (propagatorInit prop)
        (#poke clingo_propagator_t, propagate) p (propagatorPropagate prop)
        (#poke clingo_propagator_t, undo) p (propagatorUndo prop)
        (#poke clingo_propagator_t, check) p (propagatorCheck prop)

data WeightedLiteral = WeightedLiteral
    { wlLiteral :: Literal
    , wlWeight  :: Weight
    }

instance Storable WeightedLiteral where
    sizeOf _ = #{size clingo_weighted_literal_t}
    alignment = sizeOf
    peek p = WeightedLiteral
         <$> (#{peek clingo_weighted_literal_t, literal} p)
         <*> (#{peek clingo_weighted_literal_t, weight} p)

    poke p wl = do
        (#poke clingo_weighted_literal_t, literal) p (wlLiteral wl)
        (#poke clingo_weighted_literal_t, weight) p (wlWeight wl)

newtype Backend = Backend (Ptr Backend)
newtype Configuration = Configuration (Ptr Configuration)
newtype Statistics = Statistics (Ptr Statistics)
newtype ProgramBuilder = ProgramBuilder (Ptr ProgramBuilder)

data GroundProgramObserver a = GroundProgramObserver
    { gpoInitProgram   :: FunPtr (CBool -> Ptr a -> IO CBool)
    , gpoBeginStep     :: FunPtr (Ptr a -> IO CBool)
    , gpoEndStep       :: FunPtr (Ptr a -> IO CBool)
    , gpoRule          :: FunPtr (CBool -> Ptr Atom -> CSize 
                                               -> Ptr Literal -> CSize 
                                               -> Ptr a -> IO CBool)
    , gpoWeightRule    :: FunPtr (CBool -> Ptr Atom -> CSize -> Weight 
                                               -> Ptr WeightedLiteral -> CSize 
                                               -> Ptr a -> IO CBool)
    , gpoMinimize      :: FunPtr (Weight -> Ptr WeightedLiteral -> CSize 
                                         -> Ptr a -> IO CBool)
    , gpoProject       :: FunPtr (Ptr Atom -> CSize -> Ptr a -> IO CBool)
    , gpoExternal      :: FunPtr (Atom -> ExternalType -> Ptr a -> IO CBool)
    , gpoAssume        :: FunPtr (Ptr Literal -> CSize -> Ptr a -> IO CBool)
    , gpoHeuristic     :: FunPtr (Atom -> HeuristicType -> CInt 
                                       -> CUInt -> Ptr Literal -> CSize 
                                       -> Ptr a -> IO CBool)
    , gpoAcycEdge      :: FunPtr (CInt -> CInt -> Ptr Literal -> CSize 
                                       -> Ptr a -> IO CBool)
    , gpoTheoryTermNum :: FunPtr (Identifier -> CInt -> Ptr a -> IO CBool)
    , gpoTheoryTermStr :: FunPtr (Identifier -> Ptr CChar -> IO CBool)
    , gpoTheoryTermCmp :: FunPtr (Identifier -> CInt -> Ptr Identifier -> CSize 
                                             -> Ptr a -> IO CBool)
    , gpoTheoryElement :: FunPtr (Identifier -> Ptr Identifier -> CSize 
                                             -> Ptr Literal -> CSize -> Ptr a 
                                             -> IO CBool)
    , gpoTheoryAtom    :: FunPtr (Identifier -> Identifier -> Ptr Identifier 
                                             -> CSize -> IO CBool)
    , gpoTheoryAtomGrd :: FunPtr (Identifier -> Identifier -> Ptr Identifier 
                                             -> CSize -> Identifier 
                                             -> Identifier -> Ptr a 
                                             -> IO CBool)
    }

instance Storable (GroundProgramObserver a) where
    sizeOf _ = #{size clingo_ground_program_observer_t}
    alignment = sizeOf
    peek p = GroundProgramObserver
        <$> (#{peek clingo_ground_program_observer_t, init_program} p)
        <*> (#{peek clingo_ground_program_observer_t, begin_step} p)
        <*> (#{peek clingo_ground_program_observer_t, end_step} p)
        <*> (#{peek clingo_ground_program_observer_t, rule} p)
        <*> (#{peek clingo_ground_program_observer_t, weight_rule} p)
        <*> (#{peek clingo_ground_program_observer_t, minimize} p)
        <*> (#{peek clingo_ground_program_observer_t, project} p)
        <*> (#{peek clingo_ground_program_observer_t, external} p)
        <*> (#{peek clingo_ground_program_observer_t, assume} p)
        <*> (#{peek clingo_ground_program_observer_t, heuristic} p)
        <*> (#{peek clingo_ground_program_observer_t, acyc_edge} p)
        <*> (#{peek clingo_ground_program_observer_t, theory_term_number} p)
        <*> (#{peek clingo_ground_program_observer_t, theory_term_string} p)
        <*> (#{peek clingo_ground_program_observer_t, theory_term_compound} p)
        <*> (#{peek clingo_ground_program_observer_t, theory_element} p)
        <*> (#{peek clingo_ground_program_observer_t, theory_atom} p)
        <*> (#{peek clingo_ground_program_observer_t, theory_atom_with_guard} p)

    poke p g = do
        (#poke clingo_ground_program_observer_t, init_program) p 
            (gpoInitProgram g)
        (#poke clingo_ground_program_observer_t, begin_step) p (gpoBeginStep g)
        (#poke clingo_ground_program_observer_t, end_step) p (gpoEndStep g)
        (#poke clingo_ground_program_observer_t, rule) p (gpoRule g)
        (#poke clingo_ground_program_observer_t, weight_rule) p 
            (gpoWeightRule g)
        (#poke clingo_ground_program_observer_t, minimize) p (gpoMinimize g)
        (#poke clingo_ground_program_observer_t, project) p (gpoProject g)
        (#poke clingo_ground_program_observer_t, external) p (gpoExternal g)
        (#poke clingo_ground_program_observer_t, assume) p (gpoAssume g)
        (#poke clingo_ground_program_observer_t, heuristic) p (gpoHeuristic g)
        (#poke clingo_ground_program_observer_t, acyc_edge) p (gpoAcycEdge g)
        (#poke clingo_ground_program_observer_t, theory_term_number) p 
            (gpoTheoryTermNum g)
        (#poke clingo_ground_program_observer_t, theory_term_string) p 
            (gpoTheoryTermStr g)
        (#poke clingo_ground_program_observer_t, theory_term_compound) p 
            (gpoTheoryTermCmp g)
        (#poke clingo_ground_program_observer_t, theory_element) p 
            (gpoTheoryElement g)
        (#poke clingo_ground_program_observer_t, theory_atom) p 
            (gpoTheoryAtom g)
        (#poke clingo_ground_program_observer_t, theory_atom_with_guard) p 
            (gpoTheoryAtomGrd g)

newtype Control = Control (Ptr Control)

data Part = Part
    { partName   :: Ptr CChar
    , partParams :: Ptr Symbol
    , partSize   :: CSize
    }

instance Storable Part where
    sizeOf _ = #{size clingo_part_t}
    alignment = sizeOf
    peek p = Part
         <$> (#{peek clingo_part_t, name} p)
         <*> (#{peek clingo_part_t, params} p)
         <*> (#{peek clingo_part_t, size} p)

    poke p part = do
         (#poke clingo_part_t, name) p (partName part)
         (#poke clingo_part_t, params) p (partParams part)
         (#poke clingo_part_t, size) p (partSize part)

type CallbackSymbol a = Ptr Symbol -> CSize -> Ptr a -> IO CBool
type CallbackGround a = 
    Location -> Ptr CChar -> Ptr Symbol -> CSize -> Ptr a 
             -> CallbackSymbol a -> Ptr a -> IO CBool
type CallbackModel a = Model -> Ptr a -> Ptr CBool -> IO CBool
type CallbackFinish a = SolveResult -> Ptr a -> CBool
