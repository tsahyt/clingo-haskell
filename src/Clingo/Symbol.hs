-- | Functions for handling symbols and signatures with clingo.
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Clingo.Symbol
(
    Clingo,
    ClingoWarning,
    warningString,
    Symbol,
    Location,
    Signed (..),

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

    -- * Signature parsing/inspection
    Signature,
    parseTerm,

    signatureArity,
    signatureHash,
    signatureName,

    -- * Symbol and signature creation
    MonadSymbol (..),
    createId,
    
    -- * Symbol inspection
    symbolArguments,
    symbolGetArg,
    symbolHash,
    symbolName,
    symbolNumber,
    symbolString,
    prettySymbol,

    -- * Pure types
    PureSymbol (..),
    unpureSymbol,
    toPureSymbol,
    PureSignature (..),
    unpureSignature,
    toPureSignature
)
where

import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Data.Text.Lazy (fromStrict)
import Numeric.Natural
import Foreign.C
import Foreign

import Text.PrettyPrint.Leijen.Text hiding ((<$>))

import GHC.Generics

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

-- | Construct a symbol representing an id.
createId :: MonadSymbol m => Text -> Bool -> m s (Symbol s)
createId t = createFunction t []

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

-- | 'PureSymbol' represents a completely pure Haskell alternative to the
-- handled 'Symbol' type of the clingo library.
data PureSymbol
    = PureInfimum
    | PureSupremum
    | PureNumber Integer
    | PureFunction Text [PureSymbol] Bool
    | PureString Text
    deriving (Eq, Show, Ord, Generic)

instance Pretty PureSymbol where
    pretty c = case c of
        PureInfimum -> text "inf"
        PureSupremum -> text "sup"
        PureNumber x -> pretty x
        PureFunction x vs s -> s' <+> text (fromStrict x) 
                                   <> if null vs then mempty else vs'
            where s'  = if s then empty else text "not"
                  vs' = tupled (map pretty vs)
        PureString s -> text (fromStrict s)

-- | Create a 'Symbol' in the solver from a 'PureSymbol'
unpureSymbol :: (Monad (m s), MonadSymbol m) => PureSymbol -> m s (Symbol s)
unpureSymbol PureInfimum = createInfimum
unpureSymbol PureSupremum = createSupremum
unpureSymbol (PureNumber i) = createNumber i
unpureSymbol (PureFunction f as b) = 
    flip (createFunction f) b =<< mapM unpureSymbol as
unpureSymbol (PureString t) = createString t

toPureSymbol :: Symbol s -> PureSymbol
toPureSymbol s = case symType s of
    Raw.SymInfimum -> PureInfimum
    Raw.SymSupremum -> PureSupremum
    Raw.SymString -> PureString (fromJust $ symbolString s)
    Raw.SymFunction -> PureFunction 
        (fromJust $ symbolName s) 
        (map toPureSymbol $ symbolArguments s) 
        (fromJust (positive <$> functionSymbol s))
    Raw.SymNumber -> PureNumber (fromJust $ symbolNumber s)
    _ -> error "Unknown symbol type"

-- | 'PureSignature' represents a completely pure Haskell alternative to the
-- handled 'Signature' type of the clingo library.
data PureSignature = PureSignature Text Natural Bool
    deriving (Eq, Show, Ord, Generic)

instance Pretty PureSignature where
    pretty (PureSignature s a b) = 
        b' <+> text (fromStrict s) <> char '/' 
            <> pretty (fromIntegral a :: Integer)
        where b' = if b then empty else text "not"

-- | Create a 'Signature' in the solver from a 'PureSignature'
unpureSignature :: MonadSymbol m => PureSignature -> m s (Signature s)
unpureSignature (PureSignature t a b) = createSignature t a b

toPureSignature :: Signature s -> PureSignature
toPureSignature s = PureSignature 
    (signatureName s) (signatureArity s) (positive s)
