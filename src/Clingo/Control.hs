{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module Clingo.Control
(
    IOSym,
    ClingoT,
    Clingo,
    liftC,
    mapClingoT,
    ClingoWarning,
    warningString,
    ClingoSetting (..),
    defaultClingo,
    withDefaultClingo,
    withClingo,
    
    Part (..),

    loadProgram,
    addProgram,
    SymbolInjection,
    ground,
    interrupt,
    cleanup,
    registerPropagator,
    registerUnsafePropagator,
    Continue (..),
    SolveResult (..),
    exhausted,
    Solver,
    solve,
    withSolver,
    SolveMode,
    pattern SolveModeAsync,
    pattern SolveModeYield,

    statistics,
    programBuilder,
    configuration,
    backend,
    symbolicAtoms,
    theoryAtoms,

    TruthValue,
    pattern TruthTrue,
    pattern TruthFalse,
    pattern TruthFree,
    negateTruth,
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
import Data.List.NonEmpty (NonEmpty)
import Data.Foldable

import Foreign
import Foreign.C

import qualified Clingo.Raw as Raw
import Clingo.Internal.Utils
import Clingo.Internal.Symbol
import Clingo.Internal.Types
import Clingo.Solving (solverClose)
import Clingo.Symbol (PureSymbol(..), toPureSymbol, unpureSymbol)
import Clingo.Propagation (Propagator, propagatorToIO)

-- | Default settings for clingo. This is like calling clingo with no arguments,
-- and no logger.
defaultClingo :: ClingoSetting
defaultClingo = ClingoSetting [] Nothing 0

-- | The entry point into a computation utilizing clingo. Inside, a handle to
-- the clingo solver is available, which can not leave scope. By the same
-- mechanism, derived handles cannot be passed out either.
withClingo ::
       (MonadMask m, MonadIO m)
    => ClingoSetting
    -> (forall s. ClingoT m s r)
    -> m r
withClingo settings action = do
    ctrl <- mkClingo settings
    finally (runClingoT ctrl action) (freeClingo ctrl)

-- | Equal to @withClingo defaultClingo@
withDefaultClingo :: (MonadIO m, MonadMask m) => (forall s. ClingoT m s r) -> m r
withDefaultClingo = withClingo defaultClingo

-- | Load a logic program from a file.
loadProgram :: (MonadThrow m, MonadIO m) => FilePath -> ClingoT m s ()
loadProgram path = askC >>= \ctrl ->
    marshal0 (withCString path (Raw.controlLoad ctrl))

-- | Add an ungrounded logic program to the solver as a 'Text'. This function
-- can be used in order to utilize clingo's parser. See 'parseProgram' for when
-- you want to modify the AST before adding it.
addProgram ::
       (MonadIO m, MonadThrow m, Foldable t)
    => Text -- ^ Part Name
    -> t Text -- ^ Part Arguments
    -> Text -- ^ Program Code
    -> ClingoT m s ()
addProgram name params code =
    askC >>= \ctrl ->
        marshal0 $
        withCString (unpack name) $ \n ->
            withCString (unpack code) $ \c -> do
                ptrs <- mapM (newCString . unpack) (toList params)
                withArrayLen ptrs $ \s ps ->
                    Raw.controlAdd ctrl n ps (fromIntegral s) c

-- | A 'Part' is one building block of a logic program in clingo. Parts can be
-- grounded separately and can have arguments, which need to be initialized with
-- the solver.
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

type SymbolInjection
     = Location -> Text -> [PureSymbol] -> IO (Either Text (NonEmpty PureSymbol))

-- | Ground logic program parts. A callback can be provided to inject symbols
-- when needed.
ground ::
       (MonadThrow m, MonadIO m)
    => [Part s] -- ^ Parts to be grounded
    -> Maybe SymbolInjection -- ^ Callback for injecting symbols
    -> ClingoT m s ()
ground parts extFun =
    askC >>= \ctrl ->
        marshal0 $ do
            rparts <- mapM rawPart parts
            res <-
                withArrayLen rparts $ \len arr -> do
                    groundCB <- maybe (pure nullFunPtr) wrapCBGround extFun
                    Raw.controlGround
                        ctrl
                        arr
                        (fromIntegral len)
                        groundCB
                        nullPtr
            mapM_ freeRawPart rparts
            return res

wrapCBGround ::
       MonadIO m
    => SymbolInjection
    -> m (FunPtr (Raw.CallbackGround ()))
wrapCBGround f = liftIO $ Raw.mkCallbackGround go
  where
    go :: Raw.CallbackGround ()
    go loc name arg args _ cbSym cbSymD =
        reraiseIO $ do
            loc' <- fromRawLocation =<< peek loc
            name' <- pack <$> peekCString name
            syms <-
                mapM (fmap toPureSymbol . pureSymbol) =<<
                peekArray (fromIntegral args) arg
            res <- f loc' name' syms
            case res of
                Left err -> throwM $ ClingoException ErrorRuntime (unpack err)
                Right newSyms ->
                    let inject =
                            unwrapCBSymbol (Raw.getCallbackSymbol cbSym) cbSymD
                     in inject (toList newSyms)

unwrapCBSymbol :: Raw.CallbackSymbol a -> Ptr a -> ([PureSymbol] -> IO ())
unwrapCBSymbol f d syms = do
    syms' <- iosym $ mapM (fmap rawSymbol . unpureSymbol) syms
    withArrayLen syms' $ \len arr -> marshal0 (f arr (fromIntegral len) d)

-- | Interrupt the current solve call.
interrupt :: MonadIO m => ClingoT m s ()
interrupt = Raw.controlInterrupt =<< askC

-- | Clean up the domains of clingo's grounding component using the solving
-- component's top level assignment.
--
-- This function removes atoms from domains that are false and marks atoms as
-- facts that are true.  With multi-shot solving, this can result in smaller
-- groundings because less rules have to be instantiated and more
-- simplifications can be applied.
cleanup :: (MonadThrow m, MonadIO m) => ClingoT m s ()
cleanup = marshal0 . Raw.controlCleanup =<< askC

-- | A datatype that can be used to indicate whether solving shall continue or
-- not.
data Continue = Continue | Stop
    deriving (Eq, Show, Ord, Read, Enum, Bounded)

continueBool :: Continue -> Bool
continueBool Continue = True
continueBool Stop = False

-- | Solve the currently grounded logic program enumerating its models. Takes an
-- optional event callback. Since Clingo 5.2, the callback is no longer the only
-- way to interact with models. The callback can still be used to obtain the
-- same functionality as before. It will be called with 'Nothing' when there is
-- no more model.
--
-- Furthermore, asynchronous solving and iterative solving is also controlled
-- from this function. See "Clingo.Solving" for more details.
--
-- The 'Solver' must be closed explicitly after use. See 'withSolver' for a
-- bracketed version.
solve ::
       (MonadThrow m, MonadIO m)
    => SolveMode
    -> [AspifLiteral s]
    -> Maybe (Maybe (Model s) -> IOSym s Continue)
    -> ClingoT m s (Solver s)
solve mode assumptions onEvent = do
    ctrl <- askC
    Solver <$> marshal1 (go ctrl)
  where
    go ctrl x =
        withArrayLen (map rawAspifLiteral assumptions) $ \len arr -> do
            eventCB <- maybe (pure nullFunPtr) wrapCBEvent onEvent
            Raw.controlSolve
                ctrl
                (rawSolveMode mode)
                arr
                (fromIntegral len)
                eventCB
                nullPtr
                x

withSolver ::
       (MonadMask m, MonadThrow m, MonadIO m)
    => [AspifLiteral s]
    -> (forall s1. Solver s1 -> IOSym s1 r)
    -> ClingoT m s r
withSolver assumptions f = do
    x <- solve SolveModeYield assumptions Nothing
    Clingo (liftIO . iosym $ (f x)) `finally` solverClose x

wrapCBEvent :: MonadIO m
            => (Maybe (Model s) -> IOSym s Continue) 
            -> m (FunPtr (Raw.CallbackEvent ()))
wrapCBEvent f = liftIO $ Raw.mkCallbackEvent go
    where go :: Raw.SolveEvent 
             -> Ptr Raw.Model 
             -> Ptr a 
             -> Ptr Raw.CBool 
             -> IO Raw.CBool
          go ev m _ r = reraiseIO $ do
              m' <- case ev of
                        Raw.SolveEventModel  -> Just . Model <$> peek m
                        Raw.SolveEventFinish -> pure Nothing
                        _ -> error "wrapCBEvent: Invalid solve event"
              poke r . fromBool. continueBool =<< iosym (f m')

-- | Obtain statistics handle. See 'Clingo.Statistics'.
statistics :: (MonadThrow m, MonadIO m) => ClingoT m s (Statistics s)
statistics = fmap Statistics . marshal1 . Raw.controlStatistics =<< askC

-- | Obtain program builder handle. See 'Clingo.ProgramBuilding'.
programBuilder :: (MonadThrow m, MonadIO m) => ClingoT m s (ProgramBuilder s)
programBuilder =
    fmap ProgramBuilder . marshal1 . Raw.controlProgramBuilder =<< askC

-- | Obtain backend handle. See 'Clingo.ProgramBuilding'.
backend :: (MonadThrow m, MonadIO m) => ClingoT m s (Backend s)
backend = fmap Backend . marshal1 . Raw.controlBackend =<< askC

-- | Obtain configuration handle. See 'Clingo.Configuration'.
configuration :: (MonadThrow m, MonadIO m) => ClingoT m s (Configuration s)
configuration =
    fmap Configuration . marshal1 . Raw.controlConfiguration =<< askC

-- | Obtain symbolic atoms handle. See 'Clingo.Inspection.SymbolicAtoms'.
symbolicAtoms :: (MonadThrow m, MonadIO m) => ClingoT m s (SymbolicAtoms s)
symbolicAtoms =
    fmap SymbolicAtoms . marshal1 . Raw.controlSymbolicAtoms =<< askC

-- | Obtain theory atoms handle. See 'Clingo.Inspection.TheoryAtoms'.
theoryAtoms :: (MonadThrow m, MonadIO m) => ClingoT m s (TheoryAtoms s)
theoryAtoms = fmap TheoryAtoms . marshal1 . Raw.controlTheoryAtoms =<< askC

-- | Configure how learnt constraints are handled during enumeration.
-- 
-- If the enumeration assumption is enabled, then all information learnt from
-- the solver's various enumeration modes is removed after a solve call. This
-- includes enumeration of cautious or brave consequences, enumeration of
-- answer sets with or without projection, or finding optimal models, as well
-- as clauses added with clingo_solve_control_add_clause().
useEnumAssumption :: (MonadThrow m, MonadIO m) => Bool -> ClingoT m s ()
useEnumAssumption b = askC >>= \ctrl -> 
    marshal0 $ Raw.controlUseEnumAssumption ctrl (fromBool b)

-- | Assign a truth value to an external atom.
-- 
-- If the atom does not exist or is not external, this is a noop.
assignExternal ::
       (MonadThrow m, MonadIO m)
    => AspifLiteral s
    -> TruthValue
    -> ClingoT m s ()
assignExternal s t =
    askC >>= \ctrl ->
        marshal0 $
        Raw.controlAssignExternal ctrl (rawAspifLiteral s) (rawTruthValue t)

-- | Release an external atom.
-- 
-- After this call, an external atom is no longer external and subject to
-- program simplifications.  If the atom does not exist or is not external,
-- this is a noop.
releaseExternal :: (MonadThrow m, MonadIO m) => AspifLiteral s -> ClingoT m s ()
releaseExternal s =
    askC >>= \ctrl ->
        marshal0 $ Raw.controlReleaseExternal ctrl (rawAspifLiteral s)

-- | Get the symbol for a constant definition @#const name = symbol@.
getConst :: (MonadThrow m, MonadIO m) => Text -> ClingoT m s (Symbol s)
getConst name = askC >>= \ctrl -> pureSymbol =<< marshal1 (go ctrl)
  where
    go ctrl x =
        withCString (unpack name) $ \cstr -> Raw.controlGetConst ctrl cstr x

-- | Check if there is a constant definition for the given constant.
hasConst :: (MonadThrow m, MonadIO m) => Text -> ClingoT m s Bool
hasConst name = askC >>= \ctrl -> toBool <$> marshal1 (go ctrl)
  where
    go ctrl x =
        withCString (unpack name) $ \cstr -> Raw.controlHasConst ctrl cstr x

-- | Register a custom propagator with the solver.
--
-- If the sequential flag is set to true, the propagator is called
-- sequentially when solving with multiple threads.
-- 
-- See the 'Clingo.Propagation' module for more information.
registerPropagator ::
       (MonadThrow m, MonadIO m) => Bool -> Propagator s -> ClingoT m s ()
registerPropagator sequ prop = do
    ctrl <- askC
    prop' <- rawPropagator . propagatorToIO $ prop
    res <-
        liftIO $
        with prop' $ \ptr ->
            Raw.controlRegisterPropagator ctrl ptr nullPtr (fromBool sequ)
    checkAndThrow res

-- | Like 'registerPropagator' but allows using 'IOPropagator's from
-- 'Clingo.Internal.Propagation'. This function is unsafe!
registerUnsafePropagator ::
       (MonadThrow m, MonadIO m) => Bool -> IOPropagator s -> ClingoT m s ()
registerUnsafePropagator sequ prop = do
    ctrl <- askC
    prop' <- rawPropagator prop
    res <-
        liftIO $
        with prop' $ \ptr ->
            Raw.controlRegisterPropagator ctrl ptr nullPtr (fromBool sequ)
    checkAndThrow res

-- | Get clingo version.
version :: MonadIO m => m (Int, Int, Int)
version = do 
    (a,b,c) <- marshal3V Raw.version
    return (fromIntegral a, fromIntegral b, fromIntegral c)
