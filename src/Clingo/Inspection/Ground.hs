module Clingo.Inspection.Ground
(
    TermId (..),
    ElementId (..),
    AspifStmt (..),
    registerGroundObserver
)
where

import Control.Monad.IO.Class
import Data.Text (Text, pack)

import Foreign
import Foreign.C
import Numeric.Natural

import Clingo.ProgramBuilding
import Clingo.Internal.Types
import Clingo.Internal.Utils

import qualified Clingo.Raw as Raw

newtype TermId = TermId { termId :: Raw.Identifier }
    deriving (Show, Eq, Ord)

newtype ElementId = ElementId { elementId :: Raw.Identifier }
    deriving (Show, Eq, Ord)

data AspifStmt s
    = GRule Bool [Atom s] [Literal s]
    | GWeightedRule Bool [Atom s] Natural [WeightedLiteral s]
    | GMinimize Natural [WeightedLiteral s]
    | GProject [Atom s]
    | GExternal (Atom s) ExternalType
    | GAssume [Literal s]
    | GHeuristic (Atom s) HeuristicType Integer Natural [Literal s]
    | GAcycEdge Node Node [Literal s]
    | GTheoryTermNumber TermId Integer
    | GTheoryTermString TermId Text
    | GTheoryTermTuple TermId [TermId]
    | GTheoryTermSet TermId [TermId]
    | GTheoryTermList TermId [TermId]
    | GTheoryTermFunction TermId TermId [TermId]
    | GTheoryElement ElementId [TermId] [Literal s]
    | GTheoryAtom (Maybe (Atom s)) TermId [ElementId]
    | GTheoryAtomGuard (Maybe (Atom s)) TermId [ElementId] TermId TermId

mkRawGPO :: MonadIO m
         => (Bool -> IOSym s ())
         -> (AspifStmt s -> IOSym s ())
         -> m (Raw.GroundProgramObserver ())
mkRawGPO inf k = liftIO $ Raw.GroundProgramObserver
    <$> Raw.mkGpoInitProgram (\b _ -> reraiseIO (iosym (inf (toBool b))))
    <*> pure nullFunPtr
    <*> pure nullFunPtr
    <*> Raw.mkGpoRule rul
    <*> Raw.mkGpoWeightRule wrule
    <*> Raw.mkGpoMinimize minim
    <*> Raw.mkGpoProject proj
    <*> Raw.mkGpoExternal exter
    <*> Raw.mkGpoAssume assum
    <*> Raw.mkGpoHeuristic heuristi
    <*> Raw.mkGpoAcycEdge acyc
    <*> Raw.mkGpoTheoryTermNum ttnum
    <*> Raw.mkGpoTheoryTermStr ttstr
    <*> Raw.mkGpoTheoryTermCmp ttcomp
    <*> Raw.mkGpoTheoryElement telem
    <*> Raw.mkGpoTheoryAtom tatom
    <*> Raw.mkGpoTheoryAtomGrd tatomg

    where rul b as an ls ln _ = reraiseIO $ do
              as' <- map Atom <$> peekArray (fromIntegral an) as
              ls' <- map Literal <$> peekArray (fromIntegral ln) ls
              iosym (k (GRule (toBool b) as' ls'))
          wrule b as an w ws wn _ = reraiseIO $ do
              as' <- map Atom <$> peekArray (fromIntegral an) as
              ws' <- map fromRawWeightedLiteral <$> 
                        peekArray (fromIntegral wn) ws
              iosym (k (GWeightedRule (toBool b) as' (fromIntegral w) ws'))
          minim w ws wn _ = reraiseIO $ do
              ws' <- map fromRawWeightedLiteral <$>
                        peekArray (fromIntegral wn) ws
              iosym (k (GMinimize (fromIntegral w) ws'))
          proj as an _ = reraiseIO $ do
              as' <- map Atom <$> peekArray (fromIntegral an) as
              iosym (k (GProject as'))
          exter a t _ = reraiseIO $
              iosym (k (GExternal (Atom a) (fromRawExtT t)))
          assum ls n _ = reraiseIO $ do
              ls' <- map Literal <$> peekArray (fromIntegral n) ls
              iosym (k (GAssume ls'))
          heuristi a t c d ls n _ = reraiseIO $ do
              let a' = Atom a
                  t' = fromRawHeuT t
                  c' = fromIntegral c
                  d' = fromIntegral d
              ls' <- map Literal <$> peekArray (fromIntegral n) ls
              iosym (k (GHeuristic a' t' c' d' ls'))
          acyc a b ls n _ = reraiseIO $ do
              let a' = Node . fromIntegral $ a
                  b' = Node . fromIntegral $ b
              ls' <- map Literal <$> peekArray (fromIntegral n) ls
              iosym (k (GAcycEdge a' b' ls'))
          ttnum i x _ = reraiseIO $
              iosym (k (GTheoryTermNumber (TermId i) (fromIntegral x)))
          ttstr i t _ = reraiseIO $ do
              t' <- pack <$> peekCString t
              iosym (k (GTheoryTermString (TermId i) t'))
          ttcomp i a is n _ = reraiseIO $ do
              is' <- map TermId <$> peekArray (fromIntegral n) is
              case a of
                  (-1) -> iosym (k (GTheoryTermTuple (TermId i) is'))
                  (-2) -> iosym (k (GTheoryTermSet (TermId i) is'))
                  (-3) -> iosym (k (GTheoryTermList (TermId i) is'))
                  a'   -> iosym (k (GTheoryTermFunction (TermId i) 
                                       (TermId (fromIntegral a')) is'))
          telem i is n ls ln _ = reraiseIO $ do
              is' <- map TermId <$> peekArray (fromIntegral n) is
              ls' <- map Literal <$> peekArray (fromIntegral ln) ls
              iosym (k (GTheoryElement (ElementId i) is' ls'))
          tatom i t es n = reraiseIO $ do
              let i' = if i == 0 then Nothing else Just (Atom (fromIntegral i))
                  t' = TermId t
              es' <- map ElementId <$> peekArray (fromIntegral n) es
              iosym (k (GTheoryAtom i' t' es'))
          tatomg i t es n a b _ = reraiseIO $ do
              let i' = if i == 0 then Nothing else Just (Atom (fromIntegral i))
                  t' = TermId t
                  a' = TermId a
                  b' = TermId b
              es' <- map ElementId <$> peekArray (fromIntegral n) es
              iosym (k (GTheoryAtomGuard i' t' es' a' b'))

registerGroundObserver :: (Bool -> IOSym s ()) 
                       -> (AspifStmt s -> IOSym s ())
                       -> Clingo s ()
registerGroundObserver i k = do
    ctrl <- askC
    gpo  <- mkRawGPO i k
    marshall0 $ with gpo $ \ptr -> Raw.controlRegisterObserver ctrl ptr nullPtr
