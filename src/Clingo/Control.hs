{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Clingo.Control
(
    withClingo,

    loadProgram,
    addProgram,
    ground,
    solve,

    version
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Text (Text)
import qualified Data.Text.Foreign as T

import Foreign
import Foreign.C
import Foreign.Marshal.Utils

import qualified Clingo.Raw as Raw
import Clingo.Internal.Utils
import Clingo.Internal.Types

withClingo :: (forall s. Clingo s -> IO r) -> IO r
withClingo action = alloca $ \ctrlPtr -> do
    allocRes <- Raw.controlNew nullPtr 0 nullFunPtr nullPtr 0 ctrlPtr
    if toBool allocRes
        then do
            ctrl <- peek ctrlPtr
            finally (action (Clingo ctrl)) (Raw.controlFree ctrl)
        else error "Could not initialize clingo!"

loadProgram :: (MonadIO m, MonadThrow m) => Clingo s -> FilePath -> m ()
loadProgram (Clingo ctrl) path =
    marshall0 (withCString path (Raw.controlLoad ctrl))

addProgram :: (MonadIO m, MonadThrow m) 
           => Clingo s -> Text -> [Text] -> Text -> m ()
addProgram (Clingo ctrl) name params code = marshall0 undefined

ground :: (MonadIO m, MonadThrow m) 
       => Clingo s 
       -> [Part s] 
       -> (Location -> Text -> [Symbol s] -> ([Symbol s] -> IO ()) -> IO ())
       -> m ()
ground (Clingo ctrl) parts extFun = marshall0 $
    withArrayLen (map rawPart parts) $ \len arr -> do
        groundCB <- wrapCBGround extFun
        Raw.controlGround ctrl arr (fromIntegral len) groundCB nullPtr

wrapCBGround :: MonadIO m
             => (Location -> Text -> [Symbol s] 
                          -> ([Symbol s] -> IO ()) -> IO ())
             -> m (FunPtr (Raw.CallbackGround ()))
wrapCBGround f = undefined

solve :: (MonadIO m, MonadThrow m) 
      => Clingo s 
      -> (Model s -> IO Bool)
      -> [SymbolicLiteral s] 
      -> m Raw.SolveResult
solve (Clingo ctrl) onModel assumptions = marshall1 go
    where go x = withArrayLen (map rawSymLit assumptions) $ \len arr -> do
                     modelCB <- wrapCBModel onModel
                     Raw.controlSolve ctrl modelCB nullPtr 
                                      arr (fromIntegral len) x

wrapCBModel :: MonadIO m 
            => (Model s -> IO Bool) 
            -> m (FunPtr (Raw.CallbackModel ()))
wrapCBModel f = liftIO $ Raw.mkCallbackModel go
    where go :: Raw.Model -> Ptr a -> Ptr Raw.CBool -> IO Raw.CBool
          go m _ r = reraiseIO $ poke r . fromBool =<< f (Model m)

version :: MonadIO m => m (Int, Int, Int)
version = do 
    (a,b,c) <- marshall3V Raw.version
    return (fromIntegral a, fromIntegral b, fromIntegral c)
