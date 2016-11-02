{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Statistics
(
    StatsTree (..),
    AMVTree (..),
    (>=>),
    fromStats,
    fromStatsMany,
    subStats
)
where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.DeepSeq
import Data.Text (Text)

import GHC.Generics

import Clingo.Internal.Types
import Clingo.Internal.Statistics

import System.IO.Unsafe

data StatsTree v
    = SValue v
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
fromStats :: (MonadIO m, MonadThrow m, NFData w) 
          => Statistics s -> (StatsTree Double -> Maybe w) -> m (Maybe w)
fromStats s f = head <$> fromStatsMany s [f]

-- | Like 'fromTree' but supporting multiple paths.
fromStatsMany :: (MonadIO m, MonadThrow m, NFData w)
              => Statistics s -> [StatsTree Double -> Maybe w] -> m [Maybe w]
fromStatsMany s fs = getTree s >>= \t -> return (force (fs <*> [t]))

-- | Get an entire subtree from the statistics. The entire subtree will be
-- evaluated before returning!
subStats :: (MonadIO m, MonadThrow m, NFData w)
         => Statistics s -> (StatsTree Double -> Maybe (StatsTree w))
         -> m (Maybe (StatsTree w))
subStats s f = force . f <$> getTree s
