module Clingo.ProgramBuilding
(
    Node,

)
where

import Control.Monad.IO.Class
import Control.Monad.Catch

import Foreign
import Foreign.C
import Numeric.Natural

import qualified Clingo.Raw as Raw

import Clingo.Internal.Types
import Clingo.Internal.Utils

newtype Node = Node { unNode :: CInt }

newtype ExternalType = ExternalType { rawExtT :: Raw.ExternalType }

newtype HeuristicType = HeuristicType { rawHeuT :: Raw.HeuristicType }

acycEdge :: (MonadIO m, MonadThrow m)
         => Backend s -> Node -> Node -> [Literal s] -> m ()
acycEdge (Backend h) a b lits = marshall0 $
    withArrayLen (map rawLiteral lits) $ \len arr ->
        Raw.backendAcycEdge h (unNode a) (unNode b) arr (fromIntegral len)

atom :: (MonadIO m, MonadThrow m)
     => Backend s -> m (Atom s)
atom (Backend h) = Atom <$> marshall1 (Raw.backendAddAtom h)

assume :: (MonadIO m, MonadThrow m)
       => Backend s -> [Literal s] -> m ()
assume (Backend h) lits = marshall0 $ 
    withArrayLen (map rawLiteral lits) $ \len arr ->
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

minimize :: (MonadIO m, MonadThrow m)
         => Backend s -> Integer -> [WeightedLiteral s] -> m ()
minimize (Backend h) priority lits = marshall0 $
    withArrayLen (map rawWeightedLiteral lits) $ \len arr ->
        Raw.backendMinimize h (fromIntegral priority) arr (fromIntegral len)

rule :: (MonadIO m, MonadThrow m)
     => Backend s -> Bool -> [Atom s] -> [Literal s] -> m ()
rule (Backend h) choice hd bd = marshall0 $
    withArrayLen (map rawAtom hd) $ \hlen harr ->
        withArrayLen (map rawLiteral bd) $ \blen barr ->
            Raw.backendRule h (fromBool choice) harr (fromIntegral hlen) 
                                                barr (fromIntegral blen)

weightedRule :: (MonadIO m, MonadThrow m)
             => Backend s 
             -> Bool -> [Atom s] -> Integer -> [WeightedLiteral s] -> m ()
weightedRule (Backend h) choice hd weight bd = marshall0 $
    withArrayLen (map rawAtom hd) $ \hlen harr ->
        withArrayLen (map rawWeightedLiteral bd) $ \blen barr ->
            Raw.backendWeightRule h (fromBool choice) harr (fromIntegral hlen) 
                                    (fromIntegral weight)
                                    barr (fromIntegral blen)

project :: (MonadIO m, MonadThrow m) => Backend s -> [Atom s] -> m ()
project (Backend h) atoms = marshall0 $
    withArrayLen (map rawAtom atoms) $ \len arr ->
        Raw.backendProject h arr (fromIntegral len)
