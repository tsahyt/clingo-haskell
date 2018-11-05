{-# LANGUAGE ForeignFunctionInterface #-}
module Clingo.Raw.ProgramBuilding
(
    backendRule,
    backendWeightRule,
    backendMinimize,
    backendProject,
    backendExternal,
    backendAssume,
    backendHeuristic,
    backendAcycEdge,
    backendAddAtom,
    programBuilderBegin,
    programBuilderAdd,
    programBuilderEnd
)
where

import Control.Monad.IO.Class

import Foreign
import Foreign.C

import Clingo.Raw.AST
import Clingo.Raw.Types
import Clingo.Raw.Enums

foreign import ccall "clingo.h clingo_backend_rule" backendRuleFFI ::
    Backend -> CBool -> Ptr Atom -> CSize -> Ptr Literal -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_backend_weight_rule" backendWeightRuleFFI ::
    Backend -> CBool -> Ptr Atom -> CSize -> Weight -> Ptr WeightedLiteral -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_backend_minimize" backendMinimizeFFI ::
    Backend -> Weight -> Ptr WeightedLiteral -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_backend_project" backendProjectFFI ::
    Backend -> Ptr Atom -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_backend_external" backendExternalFFI ::
    Backend -> Atom -> ExternalType -> IO CBool
foreign import ccall "clingo.h clingo_backend_assume" backendAssumeFFI ::
    Backend -> Ptr Literal -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_backend_heuristic" backendHeuristicFFI ::
    Backend -> Atom -> HeuristicType -> CInt -> CUInt -> Ptr Literal -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_backend_acyc_edge" backendAcycEdgeFFI ::
    Backend -> CInt -> CInt -> Ptr Literal -> CSize -> IO CBool
foreign import ccall "clingo.h clingo_backend_add_atom" backendAddAtomFFI ::
    Backend -> Ptr Symbol -> Ptr Atom -> IO CBool
foreign import ccall "clingo.h clingo_program_builder_begin" programBuilderBeginFFI ::
    ProgramBuilder -> IO CBool
foreign import ccall "clingo.h clingo_program_builder_add" programBuilderAddFFI ::
    ProgramBuilder -> Ptr AstStatement -> IO CBool
foreign import ccall "clingo.h clingo_program_builder_end" programBuilderEndFFI ::
    ProgramBuilder -> IO CBool

backendRule :: MonadIO m => Backend -> CBool -> Ptr Atom -> CSize -> Ptr Literal -> CSize -> m CBool
backendRule a b c d e f = liftIO $ backendRuleFFI a b c d e f

backendWeightRule :: MonadIO m => Backend -> CBool -> Ptr Atom -> CSize -> Weight -> Ptr WeightedLiteral -> CSize -> m CBool
backendWeightRule a b c d e f g = liftIO $ backendWeightRuleFFI a b c d e f g

backendMinimize :: MonadIO m => Backend -> Weight -> Ptr WeightedLiteral -> CSize -> m CBool
backendMinimize a b c d = liftIO $ backendMinimizeFFI a b c d

backendProject :: MonadIO m => Backend -> Ptr Atom -> CSize -> m CBool
backendProject a b c = liftIO $ backendProjectFFI a b c

backendExternal :: MonadIO m => Backend -> Atom -> ExternalType -> m CBool
backendExternal a b c = liftIO $ backendExternalFFI a b c 

backendAssume :: MonadIO m => Backend -> Ptr Literal -> CSize -> m CBool
backendAssume a b c = liftIO $ backendAssumeFFI a b c

backendHeuristic :: MonadIO m => Backend -> Atom -> HeuristicType -> CInt -> CUInt -> Ptr Literal -> CSize -> m CBool
backendHeuristic a b c d e f g = liftIO $ backendHeuristicFFI a b c d e f g

backendAcycEdge :: MonadIO m => Backend -> CInt -> CInt -> Ptr Literal -> CSize -> m CBool
backendAcycEdge a b c d e = liftIO $ backendAcycEdgeFFI a b c d e

backendAddAtom :: MonadIO m => Backend -> Ptr Symbol -> Ptr Atom -> m CBool
backendAddAtom a b c = liftIO $ backendAddAtomFFI a b c

programBuilderBegin :: MonadIO m => ProgramBuilder -> m CBool
programBuilderBegin a = liftIO $ programBuilderBeginFFI a

programBuilderAdd :: MonadIO m => ProgramBuilder -> Ptr AstStatement -> m CBool
programBuilderAdd a b = liftIO $ programBuilderAddFFI a b

programBuilderEnd :: MonadIO m => ProgramBuilder -> m CBool
programBuilderEnd a = liftIO $ programBuilderEndFFI a
