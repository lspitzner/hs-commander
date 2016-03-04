{-# LANGUAGE
    KindSignatures,
    DataKinds,
    TypeOperators,
    FlexibleInstances,
    UndecidableInstances,
    ScopedTypeVariables #-}

module Commander.Params (
    Flag(..),
    Value(..)
) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Text.Read (readMaybe)
import Commander.Commands (ToParam(..), ParamFlags(..), ParamHelp(..))

--
-- Flag and Value types to encode information we want.
--
data Flag (flags :: [Symbol]) (help :: Symbol) a = Flag a

instance FromText a => ToParam (Flag flags help a) where
    toParam str = fmap Flag (fromText str)
instance KnownSymbols flags => ParamFlags (Flag flags help a) where
    paramFlags _ = Just $ symbolVals (Proxy :: Proxy flags)
instance KnownSymbol help => ParamHelp (Flag flags help a) where
    paramHelp  _ = symbolVal (Proxy :: Proxy help)

data Value (help :: Symbol) a = Value a

instance FromText a => ToParam (Value help a) where
    toParam  str = fmap Value (fromText str)
instance ParamFlags (Value help a) where
    paramFlags _ = Nothing
instance KnownSymbol help => ParamHelp (Value help a) where
    paramHelp  _ = symbolVal (Proxy :: Proxy help)

--
-- Typeclass to extract details from flag/value type, or construct the
-- type from some string. This allows custom types to be used instead of
-- the above.
--

class FromText a where
    fromText :: String -> Either String a

instance {-# OVERLAPPABLE #-} Read a => FromText a where
    fromText str = case readMaybe str of
        Nothing -> Left "string could not be cast to required type"
        Just a  -> Right a

--
-- Extract array of strings from [Symbol]
--
class KnownSymbols (s :: [Symbol]) where
    symbolVals :: proxy s -> [String]
instance (KnownSymbol s, KnownSymbols ss) => KnownSymbols (s ': ss) where
    symbolVals _ = symbolVal (Proxy :: Proxy s) : symbolVals (Proxy :: Proxy ss)
instance KnownSymbols '[] where
    symbolVals _ = []