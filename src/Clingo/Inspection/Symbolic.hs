module Clingo.Inspection.Symbolic
(
    SymbolicAtom (..),
    fromSymbolicAtoms
)
where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.Catch

import Clingo.Internal.Types
import qualified Clingo.Internal.Inspection.Symbolic as S

import System.IO.Unsafe

data SymbolicAtom s = SymbolicAtom
    { external :: Bool
    , literal  :: Literal s
    , symbol   :: Symbol s
    , fact     :: Bool
    }

fromSymbolicAtoms :: (MonadIO m, MonadThrow m, NFData a)
                  => SymbolicAtoms s 
                  -> ([SymbolicAtom s] -> [a]) 
                  -> m [a]
fromSymbolicAtoms s f = force . f <$> buildSAtoms s

getSAtom :: (MonadIO m, MonadThrow m)
         => S.SymbolicAtoms s -> S.SIterator s -> m (SymbolicAtom s)
getSAtom s i = SymbolicAtom
           <$> S.symbolicAtomsIsExternal s i
           <*> S.symbolicAtomsLiteral s i
           <*> S.symbolicAtomsSymbol s i
           <*> S.symbolicAtomsIsFact s i

buildSAtoms :: (MonadIO m, MonadThrow m)
            => S.SymbolicAtoms s -> m [SymbolicAtom s]
buildSAtoms s = do
    start <- S.symbolicAtomsBegin s Nothing
    end   <- S.symbolicAtomsEnd s
    liftIO $ go end start
    where go end i = unsafeInterleaveIO $ do
              abort <- S.symbolicAtomsIteratorEq s end i
              if abort then return []
              else let next = S.symbolicAtomsNext s i
                    in (:) <$> getSAtom s i <*> (go end =<< next)
