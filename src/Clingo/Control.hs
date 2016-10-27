{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Clingo.Control
(
    Clingo,
    withClingo,
    
    Part (..),

    loadProgram,
    addProgram,
    ground,
    interrupt,
    cleanup,
    solve,
    solveAsync,
    solveIterative,

    statistics,
    programBuilder,
    configuration,
    backend,
    symbolicAtoms,
    theoryAtoms,

    version
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Text (Text, pack, unpack)

import Foreign
import Foreign.C

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
addProgram (Clingo ctrl) name params code = marshall0 $ 
    withCString (unpack name) $ \n ->
        withCString (unpack code) $ \c -> do
            ptrs <- mapM (newCString . unpack) params
            withArrayLen ptrs $ \s ps ->
                Raw.controlAdd ctrl n ps (fromIntegral s) c

ground :: (MonadIO m, MonadThrow m) 
       => Clingo s 
       -> [Part s] 
       -> Maybe 
          (Location -> Text -> [Symbol s] -> ([Symbol s] -> IO ()) -> IO ())
       -> m ()
ground (Clingo ctrl) parts extFun = marshall0 $ do
    rparts <- mapM rawPart parts
    res <- withArrayLen rparts $ \len arr -> do
        groundCB <- maybe (pure nullFunPtr) wrapCBGround extFun
        Raw.controlGround ctrl arr (fromIntegral len) groundCB nullPtr
    mapM_ freeRawPart rparts
    return res

wrapCBGround :: MonadIO m
             => (Location -> Text -> [Symbol s] 
                          -> ([Symbol s] -> IO ()) -> IO ())
             -> m (FunPtr (Raw.CallbackGround ()))
wrapCBGround f = liftIO $ Raw.mkCallbackGround go
    where go :: Raw.CallbackGround ()
          go loc name arg args _ cbSym _ = reraiseIO $ do
              loc'  <- fromRawLocation <$> peek loc
              name' <- pack <$> peekCString name
              syms  <- map Symbol <$> peekArray (fromIntegral args) arg
              f loc' name' syms (unwrapCBSymbol $ Raw.getCallbackSymbol cbSym)

unwrapCBSymbol :: Raw.CallbackSymbol () -> ([Symbol s] -> IO ())
unwrapCBSymbol f syms =
    withArrayLen (map rawSymbol syms) $ \len arr -> 
        marshall0 (f arr (fromIntegral len) nullPtr)

interrupt :: MonadIO m => Clingo s -> m ()
interrupt (Clingo ctrl) = Raw.controlInterrupt ctrl

cleanup :: (MonadIO m, MonadThrow m) => Clingo s -> m ()
cleanup (Clingo ctrl) = marshall0 (Raw.controlCleanup ctrl)

solve :: (MonadIO m, MonadThrow m) 
      => Clingo s 
      -> Maybe (Model s -> IO Bool)
      -> [SymbolicLiteral s] 
      -> m SolveResult
solve (Clingo ctrl) onModel assumptions = fromRawSolveResult <$> marshall1 go
    where go x = withArrayLen (map rawSymLit assumptions) $ \len arr -> do
                     modelCB <- maybe (pure nullFunPtr) wrapCBModel onModel
                     Raw.controlSolve ctrl modelCB nullPtr 
                                      arr (fromIntegral len) x

solveAsync :: (MonadIO m, MonadThrow m)
           => Clingo s
           -> (Model s -> IO Bool)
           -> (SolveResult -> IO ())
           -> [SymbolicLiteral s]
           -> m (AsyncSolver s)
solveAsync (Clingo ctrl) onModel onFinish assumptions = 
    AsyncSolver <$> marshall1 go
    where go x = withArrayLen (map rawSymLit assumptions) $ \len arr -> do
                     modelCB <- wrapCBModel onModel
                     finishCB <- wrapCBFinish onFinish
                     Raw.controlSolveAsync ctrl modelCB nullPtr
                                                finishCB nullPtr
                                                arr (fromIntegral len) x

solveIterative :: (MonadIO m, MonadThrow m)
               => Clingo s
               -> [SymbolicLiteral s]
               -> m (IterSolver s)
solveIterative (Clingo ctrl) assumptions =
    IterSolver <$> marshall1 go
    where go x = withArrayLen (map rawSymLit assumptions) $ \len arr ->
                     Raw.controlSolveIter ctrl arr (fromIntegral len) x

wrapCBModel :: MonadIO m 
            => (Model s -> IO Bool) 
            -> m (FunPtr (Raw.CallbackModel ()))
wrapCBModel f = liftIO $ Raw.mkCallbackModel go
    where go :: Raw.Model -> Ptr a -> Ptr Raw.CBool -> IO Raw.CBool
          go m _ r = reraiseIO $ poke r . fromBool =<< f (Model m)

wrapCBFinish :: MonadIO m
             => (SolveResult -> IO ())
             -> m (FunPtr (Raw.CallbackFinish ()))
wrapCBFinish f = liftIO $ Raw.mkCallbackFinish go
    where go :: Raw.SolveResult -> Ptr () -> IO Raw.CBool
          go s _ = reraiseIO $ f (fromRawSolveResult s)

statistics :: (MonadIO m, MonadThrow m) => Clingo s -> m (Statistics s)
statistics (Clingo ctrl) = Statistics <$> marshall1 (Raw.controlStatistics ctrl)

programBuilder :: (MonadIO m, MonadThrow m) => Clingo s -> m (ProgramBuilder s)
programBuilder (Clingo ctrl) = ProgramBuilder <$> 
    marshall1 (Raw.controlProgramBuilder ctrl)

backend :: (MonadIO m, MonadThrow m) => Clingo s -> m (Backend s)
backend (Clingo ctrl) = Backend <$> marshall1 (Raw.controlBackend ctrl)

configuration :: (MonadIO m, MonadThrow m) => Clingo s -> m (Configuration s)
configuration (Clingo ctrl) = Configuration <$> 
    marshall1 (Raw.controlConfiguration ctrl)

symbolicAtoms :: (MonadIO m, MonadThrow m) => Clingo s -> m (SymbolicAtoms s)
symbolicAtoms (Clingo ctrl) = SymbolicAtoms <$> 
    marshall1 (Raw.controlSymbolicAtoms ctrl)

theoryAtoms :: (MonadIO m, MonadThrow m) => Clingo s -> m (TheoryAtoms s)
theoryAtoms (Clingo ctrl) = TheoryAtoms <$> 
    marshall1 (Raw.controlTheoryAtoms ctrl)

version :: MonadIO m => m (Int, Int, Int)
version = do 
    (a,b,c) <- marshall3V Raw.version
    return (fromIntegral a, fromIntegral b, fromIntegral c)
