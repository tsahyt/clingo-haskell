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
                -> Ptr (Ptr Control) -> IO CBool
foreign import ccall "clingo.h clingo_control_free" 
                     controlFreeFFI ::
    Ptr Control -> IO ()
foreign import ccall "clingo.h clingo_control_load" 
                     controlLoadFFI ::
    Ptr Control -> CString -> IO CBool
foreign import ccall "clingo.h clingo_control_add" 
                     controlAddFFI ::
    Ptr Control -> CString -> Ptr CString -> CSize -> CString -> IO CBool
foreign import ccall "clingo.h clingo_control_ground" 
                     controlGroundFFI ::
    Ptr Control -> Ptr Part -> CSize -> FunPtr (CallbackGround a) -> Ptr a 
                -> IO CBool
foreign import ccall "clingo.h clingo_control_solve" 
                     controlSolveFFI ::
    Ptr Control -> FunPtr (CallbackModel a) -> Ptr a -> Ptr SymbolicLiteral 
                -> CSize -> Ptr SolveResult -> IO CBool
foreign import ccall "clingo.h clingo_control_solve_iteratively" 
                     controlSolveIterFFI ::
    Ptr Control -> Ptr SymbolicLiteral -> CSize -> Ptr (Ptr IterSolver) 
                -> IO CBool
foreign import ccall "clingo.h clingo_control_solve_async" 
                     controlSolveAsyncFFI ::
    Ptr Control -> FunPtr (CallbackModel a) -> Ptr a 
                -> FunPtr (CallbackFinish b) -> Ptr b -> Ptr SymbolicLiteral 
                -> CSize -> Ptr (Ptr AsyncSolver) -> IO CBool
foreign import ccall "clingo.h clingo_control_cleanup" 
                     controlCleanupFFI ::
    Ptr Control -> IO CBool
foreign import ccall "clingo.h clingo_control_assign_external" 
                     controlAssignExternalFFI ::
    Ptr Control -> Symbol -> TruthValue -> IO CBool
foreign import ccall "clingo.h clingo_control_release_external" 
                     controlReleaseExternalFFI ::
    Ptr Control -> Symbol -> IO CBool
foreign import ccall "wrappers.h clingo_control_register_propagator_ptr" 
                     controlRegisterPropagatorFFI ::
    Ptr Control -> Ptr (Propagator a) -> Ptr a -> CBool -> IO CBool
foreign import ccall "clingo.h clingo_control_statistics" 
                     controlStatisticsFFI ::
    Ptr Control -> Ptr (Ptr Statistics) -> IO CBool
foreign import ccall "clingo.h clingo_control_interrupt" 
                     controlInterruptFFI ::
    Ptr Control -> IO ()
foreign import ccall "clingo.h clingo_control_clasp_facade" 
                     controlClaspFacadeFFI ::
    Ptr Control -> Ptr (Ptr ()) -> IO CBool
foreign import ccall "clingo.h clingo_control_configuration" 
                     controlConfigurationFFI ::
    Ptr Control -> Ptr (Ptr Configuration) -> IO CBool
foreign import ccall "clingo.h clingo_control_use_enumeration_assumption" 
                     controlUseEnumAssumptionFFI ::
    Ptr Control -> CBool -> IO CBool
foreign import ccall "clingo.h clingo_control_get_const" 
                     controlGetConstFFI ::
    Ptr Control -> CString -> Ptr Symbol -> IO CBool
foreign import ccall "clingo.h clingo_control_has_const" 
                     controlHasConstFFI ::
    Ptr Control -> CString -> Ptr CBool -> IO CBool
foreign import ccall "clingo.h clingo_control_symbolic_atoms" 
                     controlSymbolicAtomsFFI ::
    Ptr Control -> Ptr (Ptr SymbolicAtoms) -> IO CBool
foreign import ccall "clingo.h clingo_control_theory_atoms" 
                     controlTheoryAtomsFFI ::
    Ptr Control -> Ptr (Ptr TheoryAtoms) -> IO CBool
foreign import ccall "wrappers.h clingo_control_register_observer_ptr" 
                     controlRegisterObserverFFI ::
    Ptr Control -> Ptr (GroundProgramObserver a) -> Ptr a -> IO CBool
foreign import ccall "clingo.h clingo_control_backend" 
                     controlBackendFFI ::
    Ptr Control -> Ptr (Ptr Backend) -> IO CBool
foreign import ccall "clingo.h clingo_control_program_builder" 
                     controlProgramBuilderFFI ::
    Ptr Control -> Ptr (Ptr ProgramBuilder) -> IO CBool


controlNew :: MonadIO m => Ptr CString -> CSize -> FunPtr (Logger a) 
                        -> Ptr a -> CUInt -> Ptr (Ptr Control) -> m CBool
controlNew a b c d e f = liftIO $ controlNewFFI a b c d e f

controlFree :: MonadIO m => Ptr Control -> m ()
controlFree a = liftIO $ controlFreeFFI a

controlLoad :: MonadIO m => Ptr Control -> CString -> m CBool
controlLoad a b = liftIO $ controlLoadFFI a b

controlAdd :: MonadIO m => Ptr Control -> CString -> Ptr CString -> CSize 
                        -> CString -> m CBool
controlAdd a b c d e = liftIO $ controlAddFFI a b c d e

controlGround :: MonadIO m => Ptr Control -> Ptr Part -> CSize 
                           -> FunPtr (CallbackGround a) -> Ptr a -> m CBool
controlGround a b c d e = liftIO $ controlGroundFFI a b c d e

controlSolve :: MonadIO m => Ptr Control -> FunPtr (CallbackModel a) -> Ptr a 
                          -> Ptr SymbolicLiteral -> CSize -> Ptr SolveResult 
                          -> m CBool
controlSolve a b c d e f = liftIO $ controlSolveFFI a b c d e f

controlSolveIter :: MonadIO m => Ptr Control -> Ptr SymbolicLiteral -> CSize 
                              -> Ptr (Ptr IterSolver) -> m CBool
controlSolveIter a b c d = liftIO $ controlSolveIterFFI a b c d

controlSolveAsync :: MonadIO m => Ptr Control -> FunPtr (CallbackModel a) 
                               -> Ptr a -> FunPtr (CallbackFinish b) -> Ptr b 
                               -> Ptr SymbolicLiteral -> CSize 
                               -> Ptr (Ptr AsyncSolver) -> m CBool
controlSolveAsync a b c d e f g h = 
    liftIO $ controlSolveAsyncFFI a b c d e f g h

controlCleanup :: MonadIO m => Ptr Control -> m CBool
controlCleanup a = liftIO $ controlCleanupFFI a

controlAssignExternal :: MonadIO m => Ptr Control -> Symbol -> TruthValue 
                                   -> m CBool
controlAssignExternal a b c = liftIO $ controlAssignExternalFFI a b c

controlReleaseExternal :: MonadIO m => Ptr Control -> Symbol -> m CBool
controlReleaseExternal a b = liftIO $ controlReleaseExternalFFI a b

controlRegisterPropagator :: MonadIO m => Ptr Control -> Ptr (Propagator a) 
                                       -> Ptr a -> CBool -> m CBool
controlRegisterPropagator a b c d = 
    liftIO $ controlRegisterPropagatorFFI a b c d

controlStatistics :: MonadIO m => Ptr Control -> Ptr (Ptr Statistics) -> m CBool
controlStatistics a b = liftIO $ controlStatisticsFFI a b

controlInterrupt :: MonadIO m => Ptr Control -> m ()
controlInterrupt a = liftIO $ controlInterruptFFI a

controlClaspFacade :: MonadIO m => Ptr Control -> Ptr (Ptr ()) -> m CBool
controlClaspFacade a b = liftIO $ controlClaspFacadeFFI a b

controlConfiguration :: MonadIO m => Ptr Control -> Ptr (Ptr Configuration) 
                                  -> m CBool
controlConfiguration a b = liftIO $ controlConfigurationFFI a b

controlUseEnumAssumption :: MonadIO m => Ptr Control -> CBool -> m CBool
controlUseEnumAssumption a b = liftIO $ controlUseEnumAssumptionFFI a b

controlGetConst :: MonadIO m => Ptr Control -> CString -> Ptr Symbol -> m CBool
controlGetConst a b c = liftIO $ controlGetConstFFI a b c

controlHasConst :: MonadIO m => Ptr Control -> CString -> Ptr CBool -> m CBool
controlHasConst a b c = liftIO $ controlHasConstFFI a b c

controlSymbolicAtoms :: MonadIO m => Ptr Control -> Ptr (Ptr SymbolicAtoms) 
                                  -> m CBool
controlSymbolicAtoms a b = liftIO $ controlSymbolicAtomsFFI a b

controlTheoryAtoms :: MonadIO m => Ptr Control -> Ptr (Ptr TheoryAtoms) 
                                -> m CBool
controlTheoryAtoms a b = liftIO $ controlTheoryAtomsFFI a b

controlRegisterObserver :: MonadIO m => Ptr Control 
                                     -> Ptr (GroundProgramObserver a) -> Ptr a 
                                     -> m CBool
controlRegisterObserver a b c = liftIO $ controlRegisterObserverFFI a b c

controlBackend :: MonadIO m => Ptr Control -> Ptr (Ptr Backend) -> m CBool
controlBackend a b = liftIO $ controlBackendFFI a b

controlProgramBuilder :: MonadIO m => Ptr Control -> Ptr (Ptr ProgramBuilder) 
                                   -> m CBool
controlProgramBuilder a b = liftIO $ controlProgramBuilderFFI a b
