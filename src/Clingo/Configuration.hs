{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Configuration
(
    -- * Tree interface
    ConfTree (..),
    AMVTree (..),
    (>=>),
    fromConfig,
    fromConfigMany,

    -- * Direct interface
    Configuration,
    ConfigurationType (..),
    CKey,
    configurationRoot,
    configurationType, 
    configurationDescription,
    
    -- ** Array Access
    configurationArraySize,
    configurationArrayAt,

    -- ** Map Access
    configurationMapSize,
    configurationMapSubkeyName,
    configurationMapAt,

    -- ** Value Access
    configurationValueGet,
    configurationValueIsAssigned,
    configurationValueSet
)
where

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Numeric.Natural
import Data.Word
import Data.Bits
import Data.Bifunctor
import Data.Text (Text, pack, unpack)
import Data.StateVar

import Foreign
import Foreign.C
import GHC.Generics

import qualified Clingo.Raw as Raw
import Clingo.Internal.Types
import Clingo.Internal.Utils

import System.IO.Unsafe

data ConfTree v
    = CValue v
    | CMap (Maybe v)   [(Text, ConfTree v)]
    | CArray (Maybe v) [(Int, ConfTree v)]
    | CBoth (Maybe v)  [((Text, Int), ConfTree v)]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData v => NFData (ConfTree v)

getTree :: (MonadIO m, MonadThrow m) 
        => Configuration s -> m (ConfTree (StateVar Text))
getTree s = configurationRoot s >>= liftIO . go
    where go k = unsafeInterleaveIO $ do
              t <- configurationType s k
              case t of
                  -- Both constructors
                  CType val True True -> do
                      len <- configurationArraySize s k
                      (nms, cs, os) <- goMap len k
                      return . CBoth (getVal val k) $
                          zip (zip nms (map fromIntegral os)) cs

                  -- Array constructor
                  CType val True False -> do
                      len <- configurationArraySize s k
                      let offsets = take (fromIntegral len) [0..]
                      cs  <- mapM (go <=< configurationArrayAt s k) offsets
                      return . CArray (getVal val k) $
                          zip (map fromIntegral offsets) cs

                  -- Map constructor
                  CType val False True -> do
                      len <- configurationMapSize s k
                      (nms, cs, _) <- goMap len k
                      return $ CMap (getVal val k) (zip nms cs)
                  
                  -- Only value
                  CType True _ _ -> return $ CValue (keyStateVar s k)

                  _ -> error "Unknown configuration type"

          getVal val k = if val then Just (keyStateVar s k) else Nothing

          goMap len k = do
              let offsets = take (fromIntegral len) [0..]
              nms <- mapM (configurationMapSubkeyName s k) offsets
              cs  <- mapM (go <=< configurationMapAt s k) nms
              return (nms, cs, offsets)

keyStateVar :: Configuration s -> CKey -> StateVar Text
keyStateVar c k = makeStateVar getV setV
    where getV = configurationValueGet c k
          setV = configurationValueSet c k

instance AMVTree ConfTree where
    atArray i (CArray _ a) = lookup i a
    atArray i (CBoth _ xs) = lookup i . map (first snd) $ xs
    atArray _ _ = Nothing

    atMap i (CMap _ m) = lookup i m
    atMap i (CBoth _ xs) = lookup i . map (first fst) $ xs
    atMap _ _ = Nothing

    value (CValue v) = Just v
    value (CArray (Just v) _) = Just v
    value (CBoth (Just v) _) = Just v
    value (CMap (Just v) _) = Just v
    value _ = Nothing

-- | Get a configuration option from the tree. If any lookup fails, the result
-- will be 'Nothing'. The tree will be traversed lazily, but the result is
-- evaluated before returning!
fromConfig :: (MonadIO m, MonadThrow m)
           => Configuration s 
           -> (ConfTree (StateVar Text) -> Maybe w) -> m (Maybe w)
fromConfig s f = head <$> fromConfigMany s [f]

-- | Like 'fromConfig' but supporting multiple paths.
fromConfigMany :: (MonadIO m, MonadThrow m)
               => Configuration s 
               -> [ConfTree (StateVar Text) -> Maybe w] -> m [Maybe w]
fromConfigMany s fs = getTree s >>= \t -> return (force fs <*> [t])

-- Direct Interface 
-- ----------------

newtype CKey = CKey Word32
    deriving (Show, Eq, Ord)

configurationRoot :: (MonadIO m, MonadThrow m) 
                  => Configuration s -> m CKey 
configurationRoot (Configuration c) = 
    CKey . fromIntegral <$> marshall1 (Raw.configurationRoot c)

data ConfigurationType = CType
    { hasValue :: Bool
    , hasArray :: Bool
    , hasMap   :: Bool }
    deriving (Show, Eq, Read, Ord)

fromRawConfigurationType :: Raw.ConfigurationType -> ConfigurationType
fromRawConfigurationType t = CType v a m
    where v = toBool $ t .&. Raw.ConfigValue
          a = toBool $ t .&. Raw.ConfigArray
          m = toBool $ t .&. Raw.ConfigMap

configurationType :: (MonadIO m, MonadThrow m) 
               => Configuration s -> CKey -> m ConfigurationType
configurationType (Configuration s) (CKey k) = 
    fromRawConfigurationType <$> 
        marshall1 (Raw.configurationType s (fromIntegral k))

configurationArraySize :: (MonadIO m, MonadThrow m) 
                    => Configuration s -> CKey -> m Natural
configurationArraySize (Configuration s) (CKey k) = 
    fromIntegral <$> marshall1 (Raw.configurationArraySize s (fromIntegral k))

configurationArrayAt :: (MonadIO m, MonadThrow m)
                  => Configuration s -> CKey -> Natural -> m CKey
configurationArrayAt (Configuration s) (CKey k) offset =
    CKey . fromIntegral <$> marshall1 
        (Raw.configurationArrayAt s (fromIntegral k) (fromIntegral offset))

configurationMapSize :: (MonadIO m, MonadThrow m)
                  => Configuration s -> CKey -> m Natural
configurationMapSize (Configuration s) (CKey k) =
    fromIntegral <$> marshall1 (Raw.configurationMapSize s (fromIntegral k))

configurationMapSubkeyName :: (MonadIO m, MonadThrow m)
                        => Configuration s -> CKey -> Natural -> m Text
configurationMapSubkeyName (Configuration s) (CKey k) offset = do
    cstr <- marshall1 (Raw.configurationMapSubkeyName s (fromIntegral k) 
                                                        (fromIntegral offset))
    pack <$> liftIO (peekCString cstr)

configurationMapAt :: (MonadIO m, MonadThrow m)
                => Configuration s -> CKey -> Text -> m CKey
configurationMapAt (Configuration s) (CKey k) name =
    CKey . fromIntegral <$> marshall1 go
    where go = withCString (unpack name) . 
               flip (Raw.configurationMapAt s (fromIntegral k))

configurationValueGet :: (MonadIO m) 
                      => Configuration s -> CKey -> m Text
configurationValueGet (Configuration s) (CKey k) = liftIO $ do
    len <- marshall1 (Raw.configurationValueGetSize s (fromIntegral k))
    allocaArray (fromIntegral len) $ \arr -> do
        marshall0 (Raw.configurationValueGet s (fromIntegral k) arr len)
        as <- peekArray (fromIntegral len) arr
        pure . pack . map castCCharToChar $ as

configurationDescription :: (MonadIO m, MonadThrow m)
                         => Configuration s -> CKey -> m Text
configurationDescription (Configuration c) (CKey k) = do
    s <- marshall1 (Raw.configurationDescription c (fromIntegral k))
    pack <$> liftIO (peekCString s)

configurationValueIsAssigned :: (MonadIO m, MonadThrow m)
                             => Configuration s -> CKey -> m Bool
configurationValueIsAssigned (Configuration c) (CKey k) =
    toBool <$> marshall1 (Raw.configurationValueIsAssigned c (fromIntegral k))

configurationValueSet :: (MonadIO m, MonadThrow m)
                      => Configuration s -> CKey -> Text -> m ()
configurationValueSet (Configuration s) (CKey k) v = marshall0 go
    where go = withCString (unpack v) $ \str ->
                   Raw.configurationValueSet s (fromIntegral k) str
