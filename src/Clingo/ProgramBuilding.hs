module Clingo.ProgramBuilding
(
    Node,
    Literal,
    ExternalType,
    HeuristicType,

    assume,

    acycEdge,
    atom,
    external,
    heuristic,
    minimize,
    rule,
    weightedRule,
    project
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Foldable

import Foreign
import Foreign.C
import Numeric.Natural

import qualified Clingo.Raw as Raw

import Clingo.Internal.Types
import Clingo.Internal.Utils

newtype Node = Node { unNode :: CInt }

newtype ExternalType = ExternalType { rawExtT :: Raw.ExternalType }

newtype HeuristicType = HeuristicType { rawHeuT :: Raw.HeuristicType }

data WeightedLiteral s = WeightedLiteral (Literal s) Integer

rawWeightedLiteral :: WeightedLiteral s -> Raw.WeightedLiteral
rawWeightedLiteral (WeightedLiteral l w) = 
    Raw.WeightedLiteral (rawLiteral l) (fromIntegral w)

acycEdge :: (MonadIO m, MonadThrow m, Foldable t)
         => Backend s -> Node -> Node -> t (Literal s) -> m ()
acycEdge (Backend h) a b lits = marshall0 $
    withArrayLen (map rawLiteral . toList $ lits) $ \len arr ->
        Raw.backendAcycEdge h (unNode a) (unNode b) arr (fromIntegral len)

atom :: (MonadIO m, MonadThrow m)
     => Backend s -> m (Atom s)
atom (Backend h) = Atom <$> marshall1 (Raw.backendAddAtom h)

assume :: (MonadIO m, MonadThrow m, Foldable t)
       => Backend s -> t (Literal s) -> m ()
assume (Backend h) lits = marshall0 $ 
    withArrayLen (map rawLiteral . toList $ lits) $ \len arr ->
        Raw.backendAssume h arr (fromIntegral len)

external :: (MonadIO m, MonadThrow m)
         => Backend s -> Atom s -> ExternalType -> m ()
external (Backend h) atom t = marshall0 $
    Raw.backendExternal h (rawAtom atom) (rawExtT t)

heuristic :: (MonadIO m, MonadThrow m)
          => Backend s 
          -> Atom s 
          -> HeuristicType 
          -> Int 
          -> Natural 
          -> [Literal s]
          -> m ()
heuristic (Backend h) a t bias pri cs = marshall0 $
    withArrayLen (map rawLiteral cs) $ \len arr ->
        Raw.backendHeuristic h (rawAtom a) (rawHeuT t) 
            (fromIntegral bias) (fromIntegral pri) arr (fromIntegral len)

minimize :: (MonadIO m, MonadThrow m, Foldable t)
         => Backend s -> Integer -> t (WeightedLiteral s) -> m ()
minimize (Backend h) priority lits = marshall0 $
    withArrayLen (map rawWeightedLiteral . toList $ lits) $ \len arr ->
        Raw.backendMinimize h (fromIntegral priority) arr (fromIntegral len)

rule :: (MonadIO m, MonadThrow m, Foldable t)
     => Backend s -> Bool -> t (Atom s) -> t (Literal s) -> m ()
rule (Backend h) choice hd bd = marshall0 $
    withArrayLen (map rawAtom . toList $ hd) $ \hlen harr ->
        withArrayLen (map rawLiteral . toList $ bd) $ \blen barr ->
            Raw.backendRule h (fromBool choice) harr (fromIntegral hlen) 
                                                barr (fromIntegral blen)

weightedRule :: (MonadIO m, MonadThrow m, Foldable t)
             => Backend s 
             -> Bool -> t (Atom s) -> Integer -> t (WeightedLiteral s) -> m ()
weightedRule (Backend h) choice hd weight bd = marshall0 $
    withArrayLen (map rawAtom . toList $ hd) $ \hlen harr ->
        withArrayLen (map rawWeightedLiteral . toList $ bd) $ \blen barr ->
            Raw.backendWeightRule h (fromBool choice) harr (fromIntegral hlen) 
                                    (fromIntegral weight)
                                    barr (fromIntegral blen)

project :: (MonadIO m, MonadThrow m, Foldable t)
        => Backend s -> t (Atom s) -> m ()
project (Backend h) atoms = marshall0 $
    withArrayLen (map rawAtom . toList $ atoms) $ \len arr ->
        Raw.backendProject h arr (fromIntegral len)
