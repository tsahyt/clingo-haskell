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
    backendBuilderBegin,
    backendBuilderAdd,
    backendBuilderEnd
)
where

import Control.Monad.IO.Class

import Foreign
import Foreign.C

import Clingo.Raw.AST
import Clingo.Raw.Types
import Clingo.Raw.Enums

foreign import ccall "clingo.h backend_rule" backendRuleFFI ::
    Ptr Backend -> CBool -> Ptr Atom -> CSize -> Ptr Literal -> CSize -> IO CBool
foreign import ccall "clingo.h backend_weight_rule" backendWeightRuleFFI ::
    Ptr Backend -> CBool -> Ptr Atom -> CSize -> Weight -> Ptr WeightedLiteral -> CSize -> IO CBool
foreign import ccall "clingo.h backend_minimize" backendMinimizeFFI ::
    Ptr Backend -> Weight -> Ptr WeightedLiteral -> CSize -> IO CBool
foreign import ccall "clingo.h backend_project" backendProjectFFI ::
    Ptr Backend -> Ptr Atom -> CSize -> IO CBool
foreign import ccall "clingo.h backend_external" backendExternalFFI ::
    Ptr Backend -> Atom -> ExternalType -> IO CBool
foreign import ccall "clingo.h backend_assume" backendAssumeFFI ::
    Ptr Backend -> Ptr Literal -> CSize -> IO CBool
foreign import ccall "clingo.h backend_heuristic" backendHeuristicFFI ::
    Ptr Backend -> Atom -> HeuristicType -> CInt -> CUInt -> Ptr Literal -> CSize -> IO CBool
foreign import ccall "clingo.h backend_acyc_edge" backendAcycEdgeFFI ::
    Ptr Backend -> CInt -> CInt -> Ptr Literal -> CSize -> IO CBool
foreign import ccall "clingo.h backend_add_atom" backendAddAtomFFI ::
    Ptr Backend -> Ptr Atom -> IO CBool
foreign import ccall "clingo.h backend_builder_begin" backendBuilderBeginFFI ::
    Ptr ProgramBuilder -> IO CBool
foreign import ccall "clingo.h backend_builder_add" backendBuilderAddFFI ::
    Ptr ProgramBuilder -> Ptr AstStatement -> IO CBool
foreign import ccall "clingo.h backend_builder_end" backendBuilderEndFFI ::
    Ptr ProgramBuilder -> IO CBool

backendRule :: MonadIO m => Ptr Backend -> CBool -> Ptr Atom -> CSize -> Ptr Literal -> CSize -> m CBool
backendRule a b c d e f = liftIO $ backendRuleFFI a b c d e f

backendWeightRule :: MonadIO m => Ptr Backend -> CBool -> Ptr Atom -> CSize -> Weight -> Ptr WeightedLiteral -> CSize -> m CBool
backendWeightRule a b c d e f g = liftIO $ backendWeightRuleFFI a b c d e f g

backendMinimize :: MonadIO m => Ptr Backend -> Weight -> Ptr WeightedLiteral -> CSize -> m CBool
backendMinimize a b c d = liftIO $ backendMinimizeFFI a b c d

backendProject :: MonadIO m => Ptr Backend -> Ptr Atom -> CSize -> m CBool
backendProject a b c = liftIO $ backendProjectFFI a b c

backendExternal :: MonadIO m => Ptr Backend -> Atom -> ExternalType -> m CBool
backendExternal a b c = liftIO $ backendExternalFFI a b c 

backendAssume :: MonadIO m => Ptr Backend -> Ptr Literal -> CSize -> m CBool
backendAssume a b c = liftIO $ backendAssumeFFI a b c

backendHeuristic :: MonadIO m => Ptr Backend -> Atom -> HeuristicType -> CInt -> CUInt -> Ptr Literal -> CSize -> m CBool
backendHeuristic a b c d e f g = liftIO $ backendHeuristicFFI a b c d e f g

backendAcycEdge :: MonadIO m => Ptr Backend -> CInt -> CInt -> Ptr Literal -> CSize -> m CBool
backendAcycEdge a b c d e = liftIO $ backendAcycEdgeFFI a b c d e

backendAddAtom :: MonadIO m => Ptr Backend -> Ptr Atom -> m CBool
backendAddAtom a b = liftIO $ backendAddAtomFFI a b

backendBuilderBegin :: MonadIO m => Ptr ProgramBuilder -> m CBool
backendBuilderBegin a = liftIO $ backendBuilderBeginFFI a

backendBuilderAdd :: MonadIO m => Ptr ProgramBuilder -> Ptr AstStatement -> m CBool
backendBuilderAdd a b = liftIO $ backendBuilderAddFFI a b

backendBuilderEnd :: MonadIO m => Ptr ProgramBuilder -> m CBool
backendBuilderEnd a = liftIO $ backendBuilderEndFFI a
