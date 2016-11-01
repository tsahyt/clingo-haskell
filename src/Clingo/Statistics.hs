module Clingo.Statistics
(
    -- * Direct Interface
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
    SKey <$> marshall1 (Raw.statisticsRoot s)

newtype StatisticsType = StatisticsType Raw.StatisticsType

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
