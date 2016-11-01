{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Clingo.Control
(
    Clingo,
    ClingoSetting (..),
    defaultClingo,
    withDefaultClingo,
    withClingo,
    
    Part (..),

    loadProgram,
    addProgram,
    ground,
    interrupt,
    cleanup,
    registerPropagator,
    Continue (..),
    SymbolicLiteral (..),
    SolveResult (..),
    solve,
    solveAsync,
    solveIterative,

    statistics,
    programBuilder,
    configuration,
    backend,
    symbolicAtoms,
    theoryAtoms,

    assignExternal,
    releaseExternal,
    getConst,
    hasConst,
    useEnumAssumption,

    version
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Text (Text, pack, unpack)
import Data.Foldable

import Foreign
import Foreign.C

import Numeric.Natural

import qualified Clingo.Raw as Raw
import Clingo.Internal.Utils
import Clingo.Internal.Types

data ClingoSetting = ClingoSetting
    { clingoArgs   :: [String]
    , clingoLogger :: Maybe (ClingoWarning -> Text -> IO ())
    , msgLimit     :: Natural }

defaultClingo :: ClingoSetting
defaultClingo = ClingoSetting [] Nothing 0

withClingo :: (MonadIO m, MonadMask m) 
           => ClingoSetting
           -> (forall s. Clingo s -> m r) -> m r
withClingo settings action = do
    let argc = length (clingoArgs settings)
    argv <- liftIO $ mapM newCString (clingoArgs settings)
    ctrl <- marshall1 $ \x ->
        withArray argv $ \argvArr -> do
            logCB <- maybe (pure nullFunPtr) wrapCBLogger 
                         (clingoLogger settings)
            let argv' = case clingoArgs settings of
                            [] -> nullPtr
                            _  -> argvArr
            Raw.controlNew argv' (fromIntegral argc)
                           logCB nullPtr (fromIntegral . msgLimit $ settings) x
    finally (action (Clingo ctrl)) $ do
        Raw.controlFree ctrl
        liftIO $ mapM_ free argv

withDefaultClingo :: (MonadIO m, MonadMask m)
                  => (forall s. Clingo s -> m r) -> m r
withDefaultClingo = withClingo defaultClingo

loadProgram :: (MonadIO m, MonadThrow m) => Clingo s -> FilePath -> m ()
loadProgram (Clingo ctrl) path =
    marshall0 (withCString path (Raw.controlLoad ctrl))

addProgram :: (MonadIO m, MonadThrow m, Foldable t)
           => Clingo s -> Text -> t Text -> Text -> m ()
addProgram (Clingo ctrl) name params code = marshall0 $ 
    withCString (unpack name) $ \n ->
        withCString (unpack code) $ \c -> do
            ptrs <- mapM (newCString . unpack) (toList params)
            withArrayLen ptrs $ \s ps ->
                Raw.controlAdd ctrl n ps (fromIntegral s) c

data Part s = Part
    { partName   :: Text
    , partParams :: [Symbol s] }

rawPart :: Part s -> IO Raw.Part
rawPart p = Raw.Part <$> newCString (unpack (partName p))
                     <*> newArray (map rawSymbol . partParams $ p)
                     <*> pure (fromIntegral (length . partParams $ p))

freeRawPart :: Raw.Part -> IO ()
freeRawPart p = do
    free (Raw.partName p)
    free (Raw.partParams p)

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

data Continue = Continue | Stop
    deriving (Eq, Show, Ord, Read, Enum, Bounded)

continueBool :: Continue -> Bool
continueBool Continue = True
continueBool Stop = False

solve :: (MonadIO m, MonadThrow m) 
      => Clingo s 
      -> Maybe (Model s -> IO Continue)
      -> [SymbolicLiteral s] 
      -> m SolveResult
solve (Clingo ctrl) onModel assumptions = fromRawSolveResult <$> marshall1 go
    where go x = withArrayLen (map rawSymLit assumptions) $ \len arr -> do
                     modelCB <- maybe (pure nullFunPtr) wrapCBModel onModel
                     Raw.controlSolve ctrl modelCB nullPtr 
                                      arr (fromIntegral len) x

solveAsync :: (MonadIO m, MonadThrow m)
           => Clingo s
           -> (Model s -> IO Continue)
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
            => (Model s -> IO Continue) 
            -> m (FunPtr (Raw.CallbackModel ()))
wrapCBModel f = liftIO $ Raw.mkCallbackModel go
    where go :: Raw.Model -> Ptr a -> Ptr Raw.CBool -> IO Raw.CBool
          go m _ r = reraiseIO $ 
              poke r . fromBool . continueBool =<< f (Model m)

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

useEnumAssumption :: (MonadIO m, MonadThrow m) => Clingo s -> Bool -> m ()
useEnumAssumption (Clingo ctrl) b = marshall0 $
    Raw.controlUseEnumAssumption ctrl (fromBool b)

assignExternal :: (MonadIO m, MonadThrow m) 
               => Clingo s -> Symbol s -> TruthValue -> m ()
assignExternal (Clingo ctrl) s t = marshall0 $
    Raw.controlAssignExternal ctrl (rawSymbol s) (rawTruthValue t)

releaseExternal :: (MonadIO m, MonadThrow m) 
                => Clingo s -> Symbol s -> m ()
releaseExternal (Clingo ctrl) s = marshall0 $
    Raw.controlReleaseExternal ctrl (rawSymbol s)

getConst :: (MonadIO m, MonadThrow m) => Clingo s -> Text -> m (Symbol s)
getConst (Clingo ctrl) name = Symbol <$> marshall1 go
    where go x = withCString (unpack name) $ \cstr -> 
                     Raw.controlGetConst ctrl cstr x

hasConst :: (MonadIO m, MonadThrow m) => Clingo s -> Text -> m Bool
hasConst (Clingo ctrl) name = toBool <$> marshall1 go
    where go x = withCString (unpack name) $ \cstr ->
                     Raw.controlHasConst ctrl cstr x

registerPropagator :: (MonadIO m, MonadThrow m) 
                   => Clingo s -> Propagator s -> Bool -> m ()
registerPropagator (Clingo ctrl) prop sequ = do
    prop' <- rawPropagator prop
    res <- liftIO $ with prop' $ \ptr ->
               Raw.controlRegisterPropagator ctrl ptr nullPtr (fromBool sequ)
    checkAndThrow res

version :: MonadIO m => m (Int, Int, Int)
version = do 
    (a,b,c) <- marshall3V Raw.version
    return (fromIntegral a, fromIntegral b, fromIntegral c)
