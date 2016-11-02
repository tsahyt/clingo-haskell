{-# LANGUAGE PatternSynonyms #-}
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
)
where

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Bifunctor
import Data.Text (Text)
import Data.StateVar

import GHC.Generics

import Clingo.Internal.Types
import Clingo.Internal.Configuration

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
