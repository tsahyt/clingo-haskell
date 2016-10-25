{-# LANGUAGE Rank2Types #-}
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

ground :: (MonadIO m, MonadThrow m) => Clingo s -> [Part] -> m ()
ground (Clingo ctrl) parts = marshall0 $
    withArrayLen (map rawPart parts) $ \len arr ->
        Raw.controlGround ctrl arr (fromIntegral len) nullFunPtr nullPtr

solve :: (MonadIO m, MonadThrow m) 
      => Clingo s -> [SymbolicLiteral] -> m Raw.SolveResult
solve (Clingo ctrl) assumptions = marshall1 go
    where go x = withArrayLen (map rawSymLit assumptions) $ 
                     \len arr -> Raw.controlSolve ctrl nullFunPtr nullPtr 
                                                  arr (fromIntegral len) x

version :: MonadIO m => m (Int, Int, Int)
version = do 
    (a,b,c) <- marshall3V Raw.version
    return (fromIntegral a, fromIntegral b, fromIntegral c)
