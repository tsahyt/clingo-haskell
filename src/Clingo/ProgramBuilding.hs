-- | A module providing program building capabilities for both ground and
-- non-ground programs.
{-# LANGUAGE RankNTypes #-}
module Clingo.ProgramBuilding
(
    Backend,
    ProgramBuilder,
    Node (..),
    Literal,
    Atom,
    ExternalType (..),
    HeuristicType (..),

    assume,

    -- * Ground Programs
    GroundStatement,
    addGroundStatements,
    acycEdge,
    atom,
    atomAspifLiteral,
    negateAspifLiteral,
    external,
    heuristic,
    minimize,
    rule,
    weightedRule,
    project,

    -- * Non-Ground Programs
    --
    -- | See 'Clingo.AST' for the abstract syntax tree to build 'Statement's.
    addStatements
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Foldable

import Foreign
import Numeric.Natural

import qualified Clingo.Raw as Raw

import Clingo.AST (Statement)
import Clingo.Internal.AST (rawStatement, freeStatement)
import Clingo.Internal.Types
import Clingo.Internal.Utils

newtype Node = Node { unNode :: Int }


-- | A 'GroundStatement' is a statement built from ground atoms. Because the
-- atoms are only valid within the context of clingo, they may not leave this
-- context. They can be added to the current program using the
-- 'addGroundStatements' function.
newtype GroundStatement s = 
    GStmt { addGStmt :: forall m. (MonadIO m, MonadThrow m) 
                     => Backend s -> m () }

-- | Build an edge directive.
acycEdge :: Foldable t
         => Node -> Node -> t (Literal s) -> GroundStatement s
acycEdge a b lits = GStmt $ \(Backend h) -> marshal0 $
    withArrayLen (map rawLiteral . toList $ lits) $ \len arr ->
        Raw.backendAcycEdge h (fromIntegral $ unNode a) 
            (fromIntegral $ unNode b) arr (fromIntegral len)

-- | Obtain a fresh atom to be used in aspif directives.
atom :: (MonadIO m, MonadThrow m)
     => Backend s -> Maybe (Symbol s) -> m (Atom s)
atom (Backend h) (Just s) =
    liftIO $
    with (rawSymbol s) $ \ptr -> Atom <$> marshal1 (Raw.backendAddAtom h ptr)
atom (Backend h) Nothing = Atom <$> marshal1 (Raw.backendAddAtom h nullPtr)

-- | Use an Atom as a positive AspifLiteral
atomAspifLiteral :: Atom s -> AspifLiteral s
atomAspifLiteral (Atom x) = AspifLiteral (fromIntegral x)

negateAspifLiteral :: AspifLiteral s -> AspifLiteral s
negateAspifLiteral (AspifLiteral x) = AspifLiteral (negate x)

-- | Add an assumption directive.
assume :: Foldable t
       => t (AspifLiteral s) -> GroundStatement s
assume lits = GStmt $ \(Backend h) -> marshal0 $ 
    withArrayLen (map rawAspifLiteral . toList $ lits) $ \len arr ->
        Raw.backendAssume h arr (fromIntegral len)

-- | Build an external statement.
external :: Atom s -> ExternalType -> GroundStatement s
external a t = GStmt $ \(Backend h) -> marshal0 $
    Raw.backendExternal h (rawAtom a) (rawExtT t)

-- | Build a heuristic directive.
heuristic :: (Foldable t)
          => Atom s 
          -> HeuristicType 
          -> Int                    -- ^ Bias
          -> Natural                -- ^ Priority
          -> t (AspifLiteral s)     -- ^ Condition
          -> GroundStatement s
heuristic a t bias pri cs = GStmt $ \(Backend h) -> marshal0 $
    withArrayLen (map rawAspifLiteral . toList $ cs) $ \len arr ->
        Raw.backendHeuristic h (rawAtom a) (rawHeuT t) 
            (fromIntegral bias) (fromIntegral pri) arr (fromIntegral len)

-- | Build a minimize constraint (or weak constraint).
minimize :: Foldable t
         => Integer                 -- ^ Priority
         -> t (WeightedLiteral s)   -- ^ Literals to minimize
         -> GroundStatement s
minimize priority lits = GStmt $ \(Backend h) -> marshal0 $
    withArrayLen (map rawWeightedLiteral . toList $ lits) $ \len arr ->
        Raw.backendMinimize h (fromIntegral priority) arr (fromIntegral len)

-- | Build a rule.
rule :: Foldable t
     => Bool                 -- ^ Is a choice rule?
     -> t (Atom s)           -- ^ Head
     -> t (AspifLiteral s)   -- ^ Body
     -> GroundStatement s
rule choice hd bd = GStmt $ \(Backend h) -> marshal0 $
    withArrayLen (map rawAtom . toList $ hd) $ \hlen harr ->
        withArrayLen (map rawAspifLiteral . toList $ bd) $ \blen barr ->
            Raw.backendRule h (fromBool choice) harr (fromIntegral hlen) 
                                                barr (fromIntegral blen)

-- | Build a weighted rule.
weightedRule :: Foldable t
             => Bool                    -- ^ Is a choice rule?
             -> t (Atom s)              -- ^ Head
             -> Natural                 -- ^ Lower Bound
             -> t (WeightedLiteral s)   -- ^ Body
             -> GroundStatement s
weightedRule choice hd weight bd = GStmt $ \(Backend h) -> marshal0 $
    withArrayLen (map rawAtom . toList $ hd) $ \hlen harr ->
        withArrayLen (map rawWeightedLiteral . toList $ bd) $ \blen barr ->
            Raw.backendWeightRule h (fromBool choice) harr (fromIntegral hlen) 
                                    (fromIntegral weight)
                                    barr (fromIntegral blen)

-- | Build a projection directive
project :: Foldable t
        => t (Atom s) -> GroundStatement s
project atoms = GStmt $ \(Backend h) -> marshal0 $
    withArrayLen (map rawAtom . toList $ atoms) $ \len arr ->
        Raw.backendProject h arr (fromIntegral len)

-- | Add a collection of 'GroundStatement' to the program via a 'Backend'
-- handle.
addGroundStatements ::
       (MonadThrow m, MonadIO m, Foldable t)
    => Backend s
    -> t (GroundStatement s)
    -> ClingoT m s ()
addGroundStatements b xs = mapM_ (`addGStmt` b) (toList xs)

-- | Add a collection of non-ground statements to the solver.
addStatements ::
       (MonadThrow m, MonadMask m, MonadIO m, Traversable t)
    => ProgramBuilder s
    -> t (Statement (Symbol s) (Signature s))
    -> ClingoT m s ()
addStatements (ProgramBuilder b) stmts = do
    marshal0 (Raw.programBuilderBegin b)
    mapM_ go stmts `finally` marshal0 (Raw.programBuilderEnd b)

    where go stmt = do
              stmt' <- liftIO (rawStatement stmt)
              marshal0 $
                  with stmt' $ \ptr ->
                      Raw.programBuilderAdd b ptr
              liftIO (freeStatement stmt')
