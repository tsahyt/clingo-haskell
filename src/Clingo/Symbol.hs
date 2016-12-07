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
parseTerm :: Text                                       -- ^ Term as 'Text'
          -> Maybe (ClingoWarning -> Text -> IO ())     -- ^ Logger callback
          -> Natural                                    -- ^ Callback limit
          -> Clingo s (Symbol s)
parseTerm t logger limit = Symbol <$> marshall1 go
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
signatureCreate :: Text                     -- ^ Name
                -> Natural                  -- ^ Arity
                -> Bool                     -- ^ Positive
                -> Clingo s (Signature s)
signatureCreate name arity pos = Signature <$> marshall1 go
    where go x = withCString (unpack name) $ \cstr ->
                     Raw.signatureCreate cstr (fromIntegral arity) 
                                         (fromBool pos) x

-- | Create a number symbol.
createNumber :: (Integral a) => a -> Clingo s (Symbol s)
createNumber a = Symbol <$> 
    marshall1V (Raw.symbolCreateNumber (fromIntegral a))

-- | Create a supremum symbol, @#sup@.
createSupremum :: Clingo s (Symbol s)
createSupremum = Symbol <$> 
    marshall1V Raw.symbolCreateSupremum

-- | Create a infimum symbol, @#inf@.
createInfimum :: Clingo s (Symbol s)
createInfimum = Symbol <$> 
    marshall1V Raw.symbolCreateInfimum

-- | Construct a symbol representing a string.
createString :: Text -> Clingo s (Symbol s)
createString str = Symbol <$> marshall1 go
    where go = withCString (unpack str) . flip Raw.symbolCreateString

-- | Construct a symbol representing a function or tuple.
createFunction :: Text          -- ^ Function name
               -> [Symbol s]    -- ^ Arguments
               -> Bool          -- ^ Positive sign
               -> Clingo s (Symbol s)
createFunction name args pos = Symbol <$> marshall1 go 
    where go x = withCString (unpack name) $ \cstr -> 
                     withArrayLen (map rawSymbol args) $ \len syms -> 
                         Raw.symbolCreateFunction cstr syms 
                             (fromIntegral len) (fromBool pos) x

-- | Construct a symbol representing an id.
createId :: Text -> Bool -> Clingo s (Symbol s)
createId t = createFunction t []

-- | Hash a symbol
symbolHash :: Symbol s -> Integer
symbolHash = fromIntegral . Raw.symbolHash . rawSymbol

-- | Obtain number from symbol. Will fail for invalid symbol types.
symbolNumber :: Symbol s -> Clingo s (Maybe Integer)
symbolNumber s = fmap fromIntegral <$> 
    marshall1RT (Raw.symbolNumber (rawSymbol s))

-- | Obtain the name of a symbol when possible.
symbolName :: Symbol s -> Clingo s (Maybe Text)
symbolName s = do
    x <- marshall1RT (Raw.symbolName (rawSymbol s))
    case x of
        Nothing   -> return Nothing
        Just cstr -> liftIO $ (Just . pack) <$> peekCString cstr

-- | Obtain the string from a suitable symbol.
symbolString :: Symbol s -> Clingo s (Maybe Text)
symbolString s = do
    x <- marshall1RT (Raw.symbolString (rawSymbol s))
    case x of
        Nothing   -> return Nothing
        Just cstr -> liftIO $ (Just . pack) <$> peekCString cstr

-- | Obtain the arguments of a symbol. May be empty.
symbolArguments :: Symbol s -> Clingo s [Symbol s]
symbolArguments s = map Symbol <$> 
    marshall1A (Raw.symbolArguments (rawSymbol s))

-- | Obtain the n-th argument of a symbol.
symbolGetArg :: Symbol s -> Int -> Clingo s (Maybe (Symbol s))
symbolGetArg s i = do
    args <- symbolArguments s
    if length args <= i then return Nothing
                        else return . Just $ args !! i

-- | Pretty print a symbol into a 'Text'.
prettySymbol :: Symbol s -> Clingo s Text
prettySymbol s = do
    len <- marshall1 (Raw.symbolToStringSize (rawSymbol s))
    str <- liftIO $ allocaArray (fromIntegral len) $ \ptr -> do
        b <- Raw.symbolToString (rawSymbol s) ptr len
        x <- peekCString ptr
        checkAndThrow b
        return x
    return (pack str)
