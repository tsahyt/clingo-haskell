-- | A module providing direct, but memory managed and somewhat safe, access to
-- the Clingo statistics interface. The preferred way to interface with the
-- statistics is the tree based interface in 'Clingo.Statistics'. This interface
-- exists solely for users who want to provide their own abstraction, without
-- having to reimplement memory management for the raw versions in
-- 'Clingo.Raw.Statistics'.
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Internal.Statistics
(
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

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Text (Text, pack, unpack)

import Numeric.Natural

import Foreign
import Foreign.C

import qualified Clingo.Raw as Raw
import Clingo.Internal.Types
import Clingo.Internal.Utils

newtype SKey = SKey Word64
    deriving (Show, Eq, Ord)

statisticsRoot :: (MonadIO m, MonadThrow m) => Statistics s -> m SKey
statisticsRoot (Statistics s) = 
    SKey <$> marshal1 (Raw.statisticsRoot s)

newtype StatisticsType = StatisticsType Raw.StatisticsType

pattern StatsEmpty = StatisticsType Raw.StatsEmpty
pattern StatsValue = StatisticsType Raw.StatsValue
pattern StatsArray = StatisticsType Raw.StatsArray
pattern StatsMap = StatisticsType Raw.StatsMap

statisticsType :: (MonadIO m, MonadThrow m) 
               => Statistics s -> SKey -> m StatisticsType
statisticsType (Statistics s) (SKey k) = 
    StatisticsType <$> marshal1 (Raw.statisticsType s k)

statisticsArraySize :: (MonadIO m, MonadThrow m) 
                    => Statistics s -> SKey -> m Natural
statisticsArraySize (Statistics s) (SKey k) = 
    fromIntegral <$> marshal1 (Raw.statisticsArraySize s k)

statisticsArrayAt :: (MonadIO m, MonadThrow m)
                  => Statistics s -> SKey -> Natural -> m SKey
statisticsArrayAt (Statistics s) (SKey k) offset =
    SKey <$> marshal1 (Raw.statisticsArrayAt s k (fromIntegral offset))

statisticsMapSize :: (MonadIO m, MonadThrow m)
                  => Statistics s -> SKey -> m Natural
statisticsMapSize (Statistics s) (SKey k) =
    fromIntegral <$> marshal1 (Raw.statisticsMapSize s k)

statisticsMapSubkeyName :: (MonadIO m, MonadThrow m)
                        => Statistics s -> SKey -> Natural -> m Text
statisticsMapSubkeyName (Statistics s) (SKey k) offset = do
    cstr <- marshal1 (Raw.statisticsMapSubkeyName s k (fromIntegral offset))
    pack <$> liftIO (peekCString cstr)

statisticsMapAt :: (MonadIO m, MonadThrow m)
                => Statistics s -> SKey -> Text -> m SKey
statisticsMapAt (Statistics s) (SKey k) name =
    SKey <$> marshal1 go
    where go = withCString (unpack name) . flip (Raw.statisticsMapAt s k)

statisticsValueGet :: (MonadIO m, MonadThrow m) 
                   => Statistics s -> SKey -> m Double
statisticsValueGet (Statistics s) (SKey k) = 
    realToFrac <$> marshal1 (Raw.statisticsValueGet s k)
