{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.Control
(
    controlNew,
    controlFree,

    -- * Grounding
    controlLoad,
    controlAdd,
    controlGround,

    -- * Solving
    controlSolve,
    controlSolveIter,
    controlSolveAsync,
    controlCleanup,
    controlAssignExternal,
    controlReleaseExternal,
    controlRegisterPropagator,
    controlStatistics,
    controlInterrupt,
    controlClaspFacade,

    -- * Configuration
    controlConfiguration,
    controlUseEnumAssumption,
    
    -- * Program Inspection
    controlGetConst,
    controlHasConst,
    controlSymbolicAtoms,
    controlTheoryAtoms,
    controlRegisterObserver,

    -- * Program Modification
    controlBackend,
    controlProgramBuilder,
)
where

import Control.Monad.IO.Class

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Clingo.Raw.Types
import Clingo.Raw.Enums

foreign import ccall "clingo.h clingo_control_new" 
                     controlNewFFI ::
    Ptr CString -> CSize -> FunPtr (Logger a) -> Ptr a -> CUInt 
                -> Ptr Control -> IO CBool
foreign import ccall "clingo.h clingo_control_free" 
                     controlFreeFFI ::
    Control -> IO ()
foreign import ccall "clingo.h clingo_control_load" 
                     controlLoadFFI ::
    Control -> CString -> IO CBool
foreign import ccall "clingo.h clingo_control_add" 
                     controlAddFFI ::
    Control -> CString -> Ptr CString -> CSize -> CString -> IO CBool
foreign import ccall "clingo.h clingo_control_ground_ptr" 
                     controlGroundFFI ::
    Control -> Ptr Part -> CSize -> FunPtr (CallbackGround a) -> Ptr a 
                -> IO CBool
foreign import ccall "clingo.h clingo_control_solve" 
                     controlSolveFFI ::
    Control -> FunPtr (CallbackModel a) -> Ptr a -> Ptr SymbolicLiteral 
                -> CSize -> Ptr SolveResult -> IO CBool
foreign import ccall "clingo.h clingo_control_solve_iteratively" 
                     controlSolveIterFFI ::
    Control -> Ptr SymbolicLiteral -> CSize -> Ptr IterSolver
                -> IO CBool
foreign import ccall "clingo.h clingo_control_solve_async" 
                     controlSolveAsyncFFI ::
    Control -> FunPtr (CallbackModel a) -> Ptr a 
                -> FunPtr (CallbackFinish b) -> Ptr b -> Ptr SymbolicLiteral 
                -> CSize -> Ptr AsyncSolver -> IO CBool
foreign import ccall "clingo.h clingo_control_cleanup" 
                     controlCleanupFFI ::
    Control -> IO CBool
foreign import ccall "clingo.h clingo_control_assign_external" 
                     controlAssignExternalFFI ::
    Control -> Symbol -> TruthValue -> IO CBool
foreign import ccall "clingo.h clingo_control_release_external" 
                     controlReleaseExternalFFI ::
    Control -> Symbol -> IO CBool
foreign import ccall "wrappers.h clingo_control_register_propagator_ptr" 
                     controlRegisterPropagatorFFI ::
    Control -> Ptr (Propagator a) -> Ptr a -> CBool -> IO CBool
foreign import ccall "clingo.h clingo_control_statistics" 
                     controlStatisticsFFI ::
    Control -> Ptr Statistics -> IO CBool
foreign import ccall "clingo.h clingo_control_interrupt" 
                     controlInterruptFFI ::
    Control -> IO ()
foreign import ccall "clingo.h clingo_control_clasp_facade" 
                     controlClaspFacadeFFI ::
    Control -> Ptr (Ptr ()) -> IO CBool
foreign import ccall "clingo.h clingo_control_configuration" 
                     controlConfigurationFFI ::
    Control -> Ptr Configuration -> IO CBool
foreign import ccall "clingo.h clingo_control_use_enumeration_assumption" 
                     controlUseEnumAssumptionFFI ::
    Control -> CBool -> IO CBool
foreign import ccall "clingo.h clingo_control_get_const" 
                     controlGetConstFFI ::
    Control -> CString -> Ptr Symbol -> IO CBool
foreign import ccall "clingo.h clingo_control_has_const" 
                     controlHasConstFFI ::
    Control -> CString -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_control_symbolic_atoms" 
                     controlSymbolicAtomsFFI ::
    Control -> Ptr SymbolicAtoms -> IO CBool
foreign import ccall "clingo.h clingo_control_theory_atoms" 
                     controlTheoryAtomsFFI ::
    Control -> Ptr TheoryAtoms -> IO CBool
foreign import ccall "wrappers.h clingo_control_register_observer_ptr" 
                     controlRegisterObserverFFI ::
    Control -> Ptr (GroundProgramObserver a) -> Ptr a -> IO CBool
foreign import ccall "clingo.h clingo_control_backend" 
                     controlBackendFFI ::
    Control -> Ptr Backend -> IO CBool
foreign import ccall "clingo.h clingo_control_program_builder" 
                     controlProgramBuilderFFI ::
    Control -> Ptr ProgramBuilder -> IO CBool


controlNew :: MonadIO m => Ptr CString -> CSize -> FunPtr (Logger a) 
                        -> Ptr a -> CUInt -> Ptr Control -> m CBool
controlNew a b c d e f = liftIO $ controlNewFFI a b c d e f

controlFree :: MonadIO m => Control -> m ()
controlFree a = liftIO $ controlFreeFFI a

controlLoad :: MonadIO m => Control -> CString -> m CBool
controlLoad a b = liftIO $ controlLoadFFI a b

controlAdd :: MonadIO m => Control -> CString -> Ptr CString -> CSize 
                        -> CString -> m CBool
controlAdd a b c d e = liftIO $ controlAddFFI a b c d e

controlGround :: MonadIO m => Control -> Ptr Part -> CSize 
                           -> FunPtr (CallbackGround a) -> Ptr a -> m CBool
controlGround a b c d e = liftIO $ controlGroundFFI a b c d e

controlSolve :: MonadIO m => Control -> FunPtr (CallbackModel a) -> Ptr a 
                          -> Ptr SymbolicLiteral -> CSize -> Ptr SolveResult 
                          -> m CBool
controlSolve a b c d e f = liftIO $ controlSolveFFI a b c d e f

controlSolveIter :: MonadIO m => Control -> Ptr SymbolicLiteral -> CSize 
                              -> Ptr IterSolver -> m CBool
controlSolveIter a b c d = liftIO $ controlSolveIterFFI a b c d

controlSolveAsync :: MonadIO m => Control -> FunPtr (CallbackModel a) 
                               -> Ptr a -> FunPtr (CallbackFinish b) -> Ptr b 
                               -> Ptr SymbolicLiteral -> CSize 
                               -> Ptr AsyncSolver -> m CBool
controlSolveAsync a b c d e f g h = 
    liftIO $ controlSolveAsyncFFI a b c d e f g h

controlCleanup :: MonadIO m => Control -> m CBool
controlCleanup a = liftIO $ controlCleanupFFI a

controlAssignExternal :: MonadIO m => Control -> Symbol -> TruthValue 
                                   -> m CBool
controlAssignExternal a b c = liftIO $ controlAssignExternalFFI a b c

controlReleaseExternal :: MonadIO m => Control -> Symbol -> m CBool
controlReleaseExternal a b = liftIO $ controlReleaseExternalFFI a b

controlRegisterPropagator :: MonadIO m => Control -> Ptr (Propagator a) 
                                       -> Ptr a -> CBool -> m CBool
controlRegisterPropagator a b c d = 
    liftIO $ controlRegisterPropagatorFFI a b c d

controlStatistics :: MonadIO m => Control -> Ptr Statistics -> m CBool
controlStatistics a b = liftIO $ controlStatisticsFFI a b

controlInterrupt :: MonadIO m => Control -> m ()
controlInterrupt a = liftIO $ controlInterruptFFI a

controlClaspFacade :: MonadIO m => Control -> Ptr (Ptr ()) -> m CBool
controlClaspFacade a b = liftIO $ controlClaspFacadeFFI a b

controlConfiguration :: MonadIO m => Control -> Ptr Configuration
                                  -> m CBool
controlConfiguration a b = liftIO $ controlConfigurationFFI a b

controlUseEnumAssumption :: MonadIO m => Control -> CBool -> m CBool
controlUseEnumAssumption a b = liftIO $ controlUseEnumAssumptionFFI a b

controlGetConst :: MonadIO m => Control -> CString -> Ptr Symbol -> m CBool
controlGetConst a b c = liftIO $ controlGetConstFFI a b c

controlHasConst :: MonadIO m => Control -> CString -> Ptr CBool -> m CBool
controlHasConst a b c = liftIO $ controlHasConstFFI a b c

controlSymbolicAtoms :: MonadIO m => Control -> Ptr SymbolicAtoms
                                  -> m CBool
controlSymbolicAtoms a b = liftIO $ controlSymbolicAtomsFFI a b

controlTheoryAtoms :: MonadIO m => Control -> Ptr TheoryAtoms
                                -> m CBool
controlTheoryAtoms a b = liftIO $ controlTheoryAtomsFFI a b

controlRegisterObserver :: MonadIO m => Control 
                                     -> Ptr (GroundProgramObserver a) -> Ptr a 
                                     -> m CBool
controlRegisterObserver a b c = liftIO $ controlRegisterObserverFFI a b c

controlBackend :: MonadIO m => Control -> Ptr Backend -> m CBool
controlBackend a b = liftIO $ controlBackendFFI a b

controlProgramBuilder :: MonadIO m => Control -> Ptr ProgramBuilder
                                   -> m CBool
controlProgramBuilder a b = liftIO $ controlProgramBuilderFFI a b
