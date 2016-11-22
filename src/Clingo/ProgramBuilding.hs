-- | A module providing program building capabilities for both ground and
-- non-ground programs.
module Clingo.ProgramBuilding
(
    Node,
    Literal,
    ExternalType,
    HeuristicType,

    assume,

    -- * Ground Programs
    acycEdge,
    atom,
    external,
    heuristic,
    minimize,
    rule,
    weightedRule,
    project,

    -- * Non-Ground Programs
    addStatements
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Foldable
import Data.Traversable

import Foreign
import Foreign.C
import Numeric.Natural

import qualified Clingo.Raw as Raw

import Clingo.Internal.AST (Statement, rawStatement, freeStatement)
import Clingo.Internal.Types
import Clingo.Internal.Utils

newtype Node = Node { unNode :: CInt }

newtype ExternalType = ExternalType { rawExtT :: Raw.ExternalType }

newtype HeuristicType = HeuristicType { rawHeuT :: Raw.HeuristicType }

data WeightedLiteral s = WeightedLiteral (Literal s) Integer

rawWeightedLiteral :: WeightedLiteral s -> Raw.WeightedLiteral
rawWeightedLiteral (WeightedLiteral l w) = 
    Raw.WeightedLiteral (rawLiteral l) (fromIntegral w)

-- | Add an edge directive.
acycEdge :: (MonadIO m, MonadThrow m, Foldable t)
         => Backend s -> Node -> Node -> t (Literal s) -> m ()
acycEdge (Backend h) a b lits = marshall0 $
    withArrayLen (map rawLiteral . toList $ lits) $ \len arr ->
        Raw.backendAcycEdge h (unNode a) (unNode b) arr (fromIntegral len)

-- | Obtain a fresh atom to be used in aspif directives.
atom :: (MonadIO m, MonadThrow m)
     => Backend s -> m (Atom s)
atom (Backend h) = Atom <$> marshall1 (Raw.backendAddAtom h)

-- | Add an assumption directive.
assume :: (MonadIO m, MonadThrow m, Foldable t)
       => Backend s -> t (AspifLiteral s) -> m ()
assume (Backend h) lits = marshall0 $ 
    withArrayLen (map rawAspifLiteral . toList $ lits) $ \len arr ->
        Raw.backendAssume h arr (fromIntegral len)

-- | Add an external statement.
external :: (MonadIO m, MonadThrow m)
         => Backend s -> Atom s -> ExternalType -> m ()
external (Backend h) atom t = marshall0 $
    Raw.backendExternal h (rawAtom atom) (rawExtT t)

-- | Add a heuristic directive.
heuristic :: (MonadIO m, MonadThrow m)
          => Backend s 
          -> Atom s 
          -> HeuristicType 
          -> Int                    -- ^ Bias
          -> Natural                -- ^ Priority
          -> [AspifLiteral s]       -- ^ Condition
          -> m ()
heuristic (Backend h) a t bias pri cs = marshall0 $
    withArrayLen (map rawAspifLiteral cs) $ \len arr ->
        Raw.backendHeuristic h (rawAtom a) (rawHeuT t) 
            (fromIntegral bias) (fromIntegral pri) arr (fromIntegral len)

-- | Add a minimize constraint (or weak constraint).
minimize :: (MonadIO m, MonadThrow m, Foldable t)
         => Backend s 
         -> Integer                 -- ^ Priority
         -> t (WeightedLiteral s)   -- ^ Literals to minimize
         -> m ()
minimize (Backend h) priority lits = marshall0 $
    withArrayLen (map rawWeightedLiteral . toList $ lits) $ \len arr ->
        Raw.backendMinimize h (fromIntegral priority) arr (fromIntegral len)

-- | Add a rule.
rule :: (MonadIO m, MonadThrow m, Foldable t)
     => Backend s 
     -> Bool            -- ^ Is a choice rule?
     -> t (Atom s)      -- ^ Head
     -> t (Literal s)   -- ^ Body
     -> m ()
rule (Backend h) choice hd bd = marshall0 $
    withArrayLen (map rawAtom . toList $ hd) $ \hlen harr ->
        withArrayLen (map rawLiteral . toList $ bd) $ \blen barr ->
            Raw.backendRule h (fromBool choice) harr (fromIntegral hlen) 
                                                barr (fromIntegral blen)

-- | Add a weighted rule.
weightedRule :: (MonadIO m, MonadThrow m, Foldable t)
             => Backend s 
             -> Bool                    -- ^ Is a choice rule?
             -> t (Atom s)              -- ^ Head
             -> Natural                 -- ^ Lower Bound
             -> t (WeightedLiteral s)   -- ^ Body
             -> m ()
weightedRule (Backend h) choice hd weight bd = marshall0 $
    withArrayLen (map rawAtom . toList $ hd) $ \hlen harr ->
        withArrayLen (map rawWeightedLiteral . toList $ bd) $ \blen barr ->
            Raw.backendWeightRule h (fromBool choice) harr (fromIntegral hlen) 
                                    (fromIntegral weight)
                                    barr (fromIntegral blen)

-- | Add a projection directive
project :: (MonadIO m, MonadThrow m, Foldable t)
        => Backend s -> t (Atom s) -> m ()
project (Backend h) atoms = marshall0 $
    withArrayLen (map rawAtom . toList $ atoms) $ \len arr ->
        Raw.backendProject h arr (fromIntegral len)

-- | Add a collection of non-ground statements to the solver.
addStatements :: (MonadIO m, MonadMask m, Traversable t)
              => ProgramBuilder s -> t Statement -> m ()
addStatements (ProgramBuilder b) stmts = do
    marshall0 (Raw.programBuilderBegin b)
    mapM_ go stmts `finally` marshall0 (Raw.programBuilderEnd b)

    where go stmt = do
              stmt' <- liftIO (rawStatement stmt)
              marshall0 $ 
                  with stmt' $ \ptr ->
                      Raw.programBuilderAdd b ptr
              liftIO (freeStatement stmt')
