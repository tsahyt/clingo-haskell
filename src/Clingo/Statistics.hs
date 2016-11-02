{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Statistics
(
    -- * Tree Interface
    StatsTree (..),
    AMVTree (..),
    (>=>),
    fromTree,
    fromTreeMany,
    subTree,

    -- * Direct Interface
    Statistics,
    StatisticsType,
    pattern StatsEmpty,
    pattern StatsValue,
    pattern StatsArray,
    pattern StatsMap,
    SKey,

    statisticsRoot,
    statisticsType,

    -- ** Array Access
    statisticsArraySize,
    statisticsArrayAt,

    -- ** Map Access
    statisticsMapSize,
    statisticsMapSubkeyName,
    statisticsMapAt,

    -- ** Value Access
    statisticsValueGet

)
where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.DeepSeq
import Data.Text (Text, pack, unpack)

import Numeric.Natural

import Foreign
import Foreign.C
import GHC.Generics

import qualified Clingo.Raw as Raw
import Clingo.Internal.Types
import Clingo.Internal.Utils

import System.IO.Unsafe

data StatsTree v
    = SValue v
    | SEmpty
    | SMap [(Text, StatsTree v)]
    | SArray [(Int, StatsTree v)]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData v => NFData (StatsTree v)

getTree :: (MonadIO m, MonadThrow m) => Statistics s -> m (StatsTree Double)
getTree s = statisticsRoot s >>= liftIO . go
    where go k = unsafeInterleaveIO $ do
              t <- statisticsType s k
              case t of
                  StatsArray -> do
                      len <- statisticsArraySize s k
                      let offsets = take (fromIntegral len) [0..]
                      cs  <- mapM (go <=< statisticsArrayAt s k) offsets
                      return $ SArray (zip (map fromIntegral offsets) cs)
                  StatsMap   -> do
                      len <- statisticsMapSize s k
                      let offsets = take (fromIntegral len) [0..]
                      nms <- mapM (statisticsMapSubkeyName s k) offsets
                      cs  <- mapM (go <=< statisticsMapAt s k) nms
                      return $ SMap (zip nms cs)
                  StatsValue -> SValue <$> statisticsValueGet s k
                  _ -> error "Encountered empty statistics node"

instance AMVTree StatsTree where
    atArray i (SArray a) = lookup i a
    atArray _ _ = Nothing

    atMap i (SMap m) = lookup i m
    atMap _ _ = Nothing

    value (SValue v) = Just v
    value _          = Nothing

-- | Get a statistics value from the tree. If any lookup fails, the result will
-- be 'Nothing'. The tree will be traversed lazily, but the result is evaluated
-- before returning!
fromTree :: (MonadIO m, MonadThrow m, NFData w) 
         => Statistics s -> (StatsTree Double -> Maybe w) -> m (Maybe w)
fromTree s f = head <$> fromTreeMany s [f]

-- | Like 'fromTree' but supporting multiple paths.
fromTreeMany :: (MonadIO m, MonadThrow m, NFData w)
             => Statistics s -> [StatsTree Double -> Maybe w] -> m [Maybe w]
fromTreeMany s fs = getTree s >>= \t -> return (force (fs <*> [t]))

-- | Get an entire subtree from the statistics. The entire subtree will be
-- evaluated before returning!
subTree :: (MonadIO m, MonadThrow m, NFData w)
        => Statistics s -> (StatsTree Double -> Maybe (StatsTree w))
        -> m (Maybe (StatsTree w))
subTree s f = force . f <$> getTree s

-- Direct Interface 
-- ----------------
newtype SKey = SKey Word64
    deriving (Show, Eq, Ord)

statisticsRoot :: (MonadIO m, MonadThrow m) => Statistics s -> m SKey
statisticsRoot (Statistics s) = 
    SKey <$> marshall1 (Raw.statisticsRoot s)

newtype StatisticsType = StatisticsType Raw.StatisticsType

pattern StatsEmpty = StatisticsType Raw.StatsEmpty
pattern StatsValue = StatisticsType Raw.StatsValue
pattern StatsArray = StatisticsType Raw.StatsArray
pattern StatsMap = StatisticsType Raw.StatsMap

statisticsType :: (MonadIO m, MonadThrow m) 
               => Statistics s -> SKey -> m StatisticsType
statisticsType (Statistics s) (SKey k) = 
    StatisticsType <$> marshall1 (Raw.statisticsType s k)

statisticsArraySize :: (MonadIO m, MonadThrow m) 
                    => Statistics s -> SKey -> m Natural
statisticsArraySize (Statistics s) (SKey k) = 
    fromIntegral <$> marshall1 (Raw.statisticsArraySize s k)

statisticsArrayAt :: (MonadIO m, MonadThrow m)
                  => Statistics s -> SKey -> Natural -> m SKey
statisticsArrayAt (Statistics s) (SKey k) offset =
    SKey <$> marshall1 (Raw.statisticsArrayAt s k (fromIntegral offset))

statisticsMapSize :: (MonadIO m, MonadThrow m)
                  => Statistics s -> SKey -> m Natural
statisticsMapSize (Statistics s) (SKey k) =
    fromIntegral <$> marshall1 (Raw.statisticsMapSize s k)

statisticsMapSubkeyName :: (MonadIO m, MonadThrow m)
                        => Statistics s -> SKey -> Natural -> m Text
statisticsMapSubkeyName (Statistics s) (SKey k) offset = do
    cstr <- marshall1 (Raw.statisticsMapSubkeyName s k (fromIntegral offset))
    pack <$> liftIO (peekCString cstr)

statisticsMapAt :: (MonadIO m, MonadThrow m)
                => Statistics s -> SKey -> Text -> m SKey
statisticsMapAt (Statistics s) (SKey k) name =
    SKey <$> marshall1 go
    where go = withCString (unpack name) . flip (Raw.statisticsMapAt s k)

statisticsValueGet :: (MonadIO m, MonadThrow m) 
                   => Statistics s -> SKey -> m Double
statisticsValueGet (Statistics s) (SKey k) = 
    realToFrac <$> marshall1 (Raw.statisticsValueGet s k)
