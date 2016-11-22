-- | Functions for handling symbols and signatures with clingo.
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Symbol
(
    Clingo,
    ClingoWarning,
    warningString,
    Symbol,
    Location,

    -- * Symbol types
    SymbolType (..),
    symbolType,
    pattern SymInfimum,
    pattern SymNumber,
    pattern SymString,
    pattern SymFunction,
    pattern SymSupremum,

    -- ** Function symbols
    FunctionSymbol,
    functionSymbol,

    -- * Signature creation/inspection
    Signature,
    signatureCreate,
    prettySymbol,
    parseTerm,

    signatureArity,
    signatureHash,
    signatureName,

    -- * Symbol creation
    createFunction,
    createId,
    createInfimum,
    createNumber,
    createString,
    createSupremum,
    
    -- * Symbol inspection
    symbolArguments,
    symbolGetArg,
    symbolHash,
    symbolName,
    symbolNumber,
    symbolString
)
where

import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.Text (Text, pack, unpack)
import Numeric.Natural
import Foreign.C
import Foreign

import Clingo.Internal.Types
import Clingo.Internal.Utils
import qualified Clingo.Raw as Raw

import System.IO.Unsafe

newtype SymbolType = SymbolType Raw.SymbolType
    deriving Eq

pattern SymInfimum = SymbolType Raw.SymInfimum
pattern SymNumber = SymbolType Raw.SymNumber
pattern SymString = SymbolType Raw.SymString
pattern SymFunction = SymbolType Raw.SymFunction
pattern SymSupremum = SymbolType Raw.SymSupremum

-- | Get the type of a symbol
symbolType :: Symbol s -> SymbolType
symbolType = SymbolType . Raw.symbolType . rawSymbol

newtype FunctionSymbol s = FuncSym { unFuncSym :: Symbol s }
    deriving (Eq, Ord)

-- | Obtain a 'FunctionSymbol' if possible.
functionSymbol :: Symbol s -> Maybe (FunctionSymbol s)
functionSymbol s = case symbolType s of
    SymFunction -> Just $ FuncSym s
    _ -> Nothing

instance Signed (FunctionSymbol s) where
    positive s = unsafePerformIO $ 
        toBool <$> marshall1 (Raw.symbolIsPositive . rawSymbol . unFuncSym $ s)
    negative s = unsafePerformIO $ 
        toBool <$> marshall1 (Raw.symbolIsNegative . rawSymbol . unFuncSym $ s)

-- | Parse a term in string form. This does not return an AST Term!
parseTerm :: (MonadIO m, MonadThrow m)
          => Clingo s
          -> Text                                       -- ^ Term as 'Text'
          -> Maybe (ClingoWarning -> Text -> IO ())     -- ^ Logger callback
          -> Natural                                    -- ^ Callback limit
          -> m (Symbol s)
parseTerm _ t logger limit = Symbol <$> marshall1 go
    where go x = withCString (unpack t) $ \cstr -> do
                     logCB <- maybe (pure nullFunPtr) wrapCBLogger logger
                     Raw.parseTerm cstr logCB nullPtr (fromIntegral limit) x

-- | Get the name of a signature.
signatureName :: Signature s -> Text
signatureName s = unsafePerformIO $
    pack <$> (peekCString . Raw.signatureName . rawSignature $ s)

-- | Get the arity of a signature.
signatureArity :: Signature s -> Natural
signatureArity = fromIntegral . Raw.signatureArity . rawSignature

-- | Hash a signature.
signatureHash :: Signature s -> Integer
signatureHash = fromIntegral . Raw.symbolHash . rawSignature

-- | Create a new signature with the solver.
signatureCreate :: (MonadIO m, MonadThrow m)
                => Clingo s 
                -> Text                     -- ^ Name
                -> Natural                  -- ^ Arity
                -> Bool                     -- ^ Positive
                -> m (Signature s)
signatureCreate _ name arity pos = Signature <$> marshall1 go
    where go x = withCString (unpack name) $ \cstr ->
                     Raw.signatureCreate cstr (fromIntegral arity) 
                                         (fromBool pos) x

-- | Create a number symbol.
createNumber :: (Integral a, MonadIO m) => Clingo s -> a -> m (Symbol s)
createNumber _ a = Symbol <$> 
    marshall1V (Raw.symbolCreateNumber (fromIntegral a))

-- | Create a supremum symbol, @#sup@.
createSupremum :: MonadIO m => Clingo s -> m (Symbol s)
createSupremum _ = Symbol <$> 
    marshall1V Raw.symbolCreateSupremum

-- | Create a infimum symbol, @#inf@.
createInfimum :: MonadIO m => Clingo s -> m (Symbol s)
createInfimum _ = Symbol <$> 
    marshall1V Raw.symbolCreateInfimum

-- | Construct a symbol representing a string.
createString :: (MonadIO m, MonadThrow m) => Clingo s -> Text -> m (Symbol s)
createString _ str = Symbol <$> marshall1 go
    where go = withCString (unpack str) . flip Raw.symbolCreateString

-- | Construct a symbol representing a function or tuple.
createFunction :: (MonadIO m, MonadThrow m) 
               => Clingo s
               -> Text          -- ^ Function name
               -> [Symbol s]    -- ^ Arguments
               -> Bool          -- ^ Positive sign
               -> m (Symbol s)
createFunction _ name args pos = Symbol <$> marshall1 go 
    where go x = withCString (unpack name) $ \cstr -> 
                     withArrayLen (map rawSymbol args) $ \len syms -> 
                         Raw.symbolCreateFunction cstr syms 
                             (fromIntegral len) (fromBool pos) x

-- | Construct a symbol representing an id.
createId :: (MonadIO m, MonadThrow m) 
         => Clingo s -> Text -> Bool -> m (Symbol s)
createId c t = createFunction c t []

-- | Hash a symbol
symbolHash :: Symbol s -> Integer
symbolHash = fromIntegral . Raw.symbolHash . rawSymbol

-- | Obtain number from symbol. Will fail for invalid symbol types.
symbolNumber :: (MonadIO m, MonadThrow m) => Symbol s -> m (Maybe Integer)
symbolNumber s = fmap fromIntegral <$> 
    marshall1RT (Raw.symbolNumber (rawSymbol s))

-- | Obtain the name of a symbol when possible.
symbolName :: (MonadIO m, MonadThrow m) => Symbol s -> m (Maybe Text)
symbolName s = do
    x <- marshall1RT (Raw.symbolName (rawSymbol s))
    case x of
        Nothing   -> return Nothing
        Just cstr -> liftIO $ (Just . pack) <$> peekCString cstr

-- | Obtain the string from a suitable symbol.
symbolString :: (MonadIO m, MonadThrow m) => Symbol s -> m (Maybe Text)
symbolString s = do
    x <- marshall1RT (Raw.symbolString (rawSymbol s))
    case x of
        Nothing   -> return Nothing
        Just cstr -> liftIO $ (Just . pack) <$> peekCString cstr

-- | Obtain the arguments of a symbol. May be empty.
symbolArguments :: (MonadIO m, MonadThrow m) => Symbol s -> m [Symbol s]
symbolArguments s = map Symbol <$> 
    marshall1A (Raw.symbolArguments (rawSymbol s))

-- | Obtain the n-th argument of a symbol.
symbolGetArg :: (MonadIO m, MonadThrow m) => Symbol s -> Int 
             -> m (Maybe (Symbol s))
symbolGetArg s i = do
    args <- symbolArguments s
    if length args <= i then return Nothing
                        else return . Just $ args !! i

-- | Pretty print a symbol into a 'Text'.
prettySymbol :: (MonadIO m, MonadThrow m) => Symbol s -> m Text
prettySymbol s = do
    len <- marshall1 (Raw.symbolToStringSize (rawSymbol s))
    str <- liftIO $ allocaArray (fromIntegral len) $ \ptr -> do
        b <- Raw.symbolToString (rawSymbol s) ptr len
        x <- peekCString ptr
        checkAndThrow b
        return x
    return (pack str)
