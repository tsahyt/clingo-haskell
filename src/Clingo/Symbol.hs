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

import Data.Text (Text, pack, unpack)
import Numeric.Natural
import Foreign.C
import Foreign

import Clingo.Internal.Types
import Clingo.Internal.Utils
import Clingo.Internal.Symbol
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
symbolType = SymbolType . symType

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
parseTerm t logger limit = pureSymbol =<< marshall1 go
    where go x = withCString (unpack t) $ \cstr -> do
                     logCB <- maybe (pure nullFunPtr) wrapCBLogger logger
                     Raw.parseTerm cstr logCB nullPtr (fromIntegral limit) x

-- | Get the name of a signature.
signatureName :: Signature s -> Text
signatureName = sigName

-- | Get the arity of a signature.
signatureArity :: Signature s -> Natural
signatureArity = sigArity

-- | Hash a signature.
signatureHash :: Signature s -> Integer
signatureHash = sigHash

-- | Create a new signature with the solver.
signatureCreate :: Text                     -- ^ Name
                -> Natural                  -- ^ Arity
                -> Bool                     -- ^ Positive
                -> Clingo s (Signature s)
signatureCreate name arity pos = pureSignature =<< marshall1 go
    where go x = withCString (unpack name) $ \cstr ->
                     Raw.signatureCreate cstr (fromIntegral arity) 
                                         (fromBool pos) x

-- | Create a number symbol.
createNumber :: (Integral a) => a -> Clingo s (Symbol s)
createNumber a = pureSymbol =<< 
    marshall1V (Raw.symbolCreateNumber (fromIntegral a))

-- | Create a supremum symbol, @#sup@.
createSupremum :: Clingo s (Symbol s)
createSupremum = pureSymbol =<< marshall1V Raw.symbolCreateSupremum

-- | Create a infimum symbol, @#inf@.
createInfimum :: Clingo s (Symbol s)
createInfimum = pureSymbol =<< marshall1V Raw.symbolCreateInfimum

-- | Construct a symbol representing a string.
createString :: Text -> Clingo s (Symbol s)
createString str = pureSymbol =<< marshall1 go
    where go = withCString (unpack str) . flip Raw.symbolCreateString

-- | Construct a symbol representing a function or tuple.
createFunction :: Text          -- ^ Function name
               -> [Symbol s]    -- ^ Arguments
               -> Bool          -- ^ Positive sign
               -> Clingo s (Symbol s)
createFunction name args pos = pureSymbol =<< marshall1 go 
    where go x = withCString (unpack name) $ \cstr -> 
                     withArrayLen (map rawSymbol args) $ \len syms -> 
                         Raw.symbolCreateFunction cstr syms 
                             (fromIntegral len) (fromBool pos) x

-- | Construct a symbol representing an id.
createId :: Text -> Bool -> Clingo s (Symbol s)
createId t = createFunction t []

-- | Hash a symbol
symbolHash :: Symbol s -> Integer
symbolHash = symHash

-- | Obtain number from symbol. Will fail for invalid symbol types.
symbolNumber :: Symbol s -> Maybe Integer
symbolNumber = symNum

-- | Obtain the name of a symbol when possible.
symbolName :: Symbol s -> Maybe Text
symbolName = symName

-- | Obtain the string from a suitable symbol.
symbolString :: Symbol s -> Maybe Text
symbolString = symString

-- | Obtain the arguments of a symbol. May be empty.
symbolArguments :: Symbol s -> [Symbol s]
symbolArguments = symArgs

-- | Obtain the n-th argument of a symbol.
symbolGetArg :: Symbol s -> Int -> Maybe (Symbol s)
symbolGetArg s i =
    let args = symbolArguments s in
    if length args <= i then Nothing
                        else Just $ args !! i

-- | Pretty print a symbol into a 'Text'.
prettySymbol :: Symbol s -> Text
prettySymbol = symPretty
