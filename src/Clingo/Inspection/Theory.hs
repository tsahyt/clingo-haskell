{-# LANGUAGE DeriveGeneric #-}
module Clingo.Inspection.Theory
(
    TheoryAtoms,
    AspifLiteral,

    Guard (..),
    Element (..),
    GroundTheoryAtom (..),
    GroundTheoryTerm (..),
    renderTerm,
    termName,

    fromTheoryAtoms
)
where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Maybe

import Data.Text (Text)
import GHC.Generics

import qualified Clingo.Internal.Inspection.Theory as I
import Clingo.Internal.Types

import System.IO.Unsafe

data Guard s = Guard Text (GroundTheoryTerm s)
    deriving (Generic)

instance NFData (Guard s)

data Element s = Element
    { elementTuple       :: [GroundTheoryTerm s]
    , elementCondition   :: [AspifLiteral s]
    , elementConditionId :: AspifLiteral s
    , renderElement      :: Text
    }
    deriving (Generic)

instance NFData (Element s)

data GroundTheoryAtom s = GroundTheoryAtom 
    { atomGuard    :: Maybe (Guard s)
    , atomTerm     :: GroundTheoryTerm s
    , atomElements :: [Element s]
    , atomLiteral  :: AspifLiteral s
    , renderAtom   :: Text
    }
    deriving (Generic)

instance NFData (GroundTheoryAtom s)

data GroundTheoryTerm s
    = SymbolTerm Text Text
    | FunctionTerm Text Text [GroundTheoryTerm s]
    | NumberTerm Text Integer
    | TupleTerm Text [Element s]
    | ListTerm Text [Element s]
    | SetTerm Text [Element s]
    deriving (Generic)

instance NFData (GroundTheoryTerm s)

renderTerm :: GroundTheoryTerm s -> Text
renderTerm (SymbolTerm r _) = r
renderTerm (FunctionTerm r _ _) = r
renderTerm (NumberTerm r _) = r
renderTerm (TupleTerm r _) = r
renderTerm (ListTerm r _) = r
renderTerm (SetTerm r _) = r

termName :: GroundTheoryTerm s -> Maybe Text
termName (SymbolTerm _ n) = Just n
termName (FunctionTerm _ n _) = Just n
termName _ = Nothing

fromTheoryAtoms :: (MonadIO m, MonadThrow m, NFData w)
                => TheoryAtoms s -> ([GroundTheoryAtom s] -> w) -> m w
fromTheoryAtoms t f = do
    size <- I.theoryAtomsSize t
    ids  <- mapM (I.theoryAtomsId t) (take (fromIntegral size) [0..])
    atoms <- mapM (buildAtom t) (catMaybes ids)
    return . force $ f atoms

buildTerm :: MonadIO m
          => TheoryAtoms s -> I.TermId -> m (GroundTheoryTerm s)
buildTerm t i = liftIO . unsafeInterleaveIO $ do
    typ <- I.theoryAtomsTermType t i
    case typ of
        I.TheorySymbol -> SymbolTerm
            <$> I.theoryAtomsTermToString t i
            <*> I.theoryAtomsTermName t i
        I.TheoryFunction -> FunctionTerm
            <$> I.theoryAtomsTermToString t i
            <*> I.theoryAtomsTermName t i
            <*> (mapM (buildTerm t) =<< I.theoryAtomsTermArguments t i)
        I.TheoryNumber -> NumberTerm
            <$> I.theoryAtomsTermToString t i
            <*> I.theoryAtomsTermNumber t i
        I.TheoryList -> ListTerm
            <$> I.theoryAtomsTermToString t i
            <*> pure []
        I.TheorySet -> SetTerm
            <$> I.theoryAtomsTermToString t i
            <*> pure []
        I.TheoryTuple -> TupleTerm
            <$> I.theoryAtomsTermToString t i
            <*> pure []
        _ -> error "Invalid theory term type"

buildElement :: MonadIO m
             => TheoryAtoms s -> I.ElementId -> m (Element s)
buildElement t i = liftIO . unsafeInterleaveIO $ Element 
    <$> (mapM (buildTerm t) =<< I.theoryAtomsElementTuple t i)
    <*> I.theoryAtomsElementCondition t i
    <*> I.theoryAtomsElementConditionId t i
    <*> I.theoryAtomsElementToString t i

buildAtom :: MonadIO m
          => TheoryAtoms s -> I.AtomId -> m (GroundTheoryAtom s)
buildAtom t i = liftIO . unsafeInterleaveIO $ GroundTheoryAtom
    <$> buildGuard
    <*> (buildTerm t =<< I.theoryAtomsAtomTerm t i)
    <*> (mapM (buildElement t) =<< I.theoryAtomsAtomElements t i)
    <*> I.theoryAtomsAtomLiteral t i
    <*> I.theoryAtomsAtomToString t i

    where buildGuard = do
              b <- I.theoryAtomsAtomHasGuard t i
              if b 
                  then do
                       (x, tid) <- I.theoryAtomsAtomGuard t i
                       term <- buildTerm t tid
                       return $ Just (Guard x term)
                  else return Nothing
