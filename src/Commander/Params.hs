-- |
-- Module: Commander.Params
--
-- This module provides a couple of basic function parameter types to be
-- used in 'Commander.Commands' function definitions. By implementing the
-- same typeclasses, one can create their own custom types to use instead
-- (or as well as) if they prefer.
--
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

-- | Use this type in a function embedded in a 'Command' in order to
-- require a flag. The type signature for this lists the flags that we
-- want to match against, the associated help text, and the output type
-- we want the flag to be cast to.
data Flag (flags :: [Symbol]) (help :: Symbol) a = Flag a

instance FromText a => ToParam (Flag flags help (Maybe a)) where
    toParam (Just str) = fmap (Flag . Just) (fromText str)
    toParam Nothing = Right (Flag Nothing)
instance ToParam (Flag flags help Bool) where
    toParam (Just "") = Right (Flag True)
    toParam (Just _) = Left "boolean string does not expect to have a value"
    toParam Nothing = Right (Flag False)
instance {-# OVERLAPPABLE #-} FromText a => ToParam (Flag flags help a) where
    toParam (Just str) = fmap Flag (fromText str)
    toParam Nothing = Left "flag expected but not found"
instance KnownSymbols flags => ParamFlags (Flag flags help a) where
    paramFlags _ = Just $ symbolVals (Proxy :: Proxy flags)
instance KnownSymbol help => ParamHelp (Flag flags help a) where
    paramHelp _ = symbolVal (Proxy :: Proxy help)

-- | Use this type in a function embedded in a 'Command' in order to
-- require a value. The type signature for this contains the associated
-- help text for the command, and the type we expect the value to be cast
-- to.
data Value (help :: Symbol) a = Value a

instance FromText a => ToParam (Value help a) where
    toParam (Just str) = fmap Value (fromText str)
    toParam Nothing = Left "value expected but none found"
instance ParamFlags (Value help a) where
    paramFlags _ = Nothing
instance KnownSymbol help => ParamHelp (Value help a) where
    paramHelp _ = symbolVal (Proxy :: Proxy help)

--
-- Typeclass to extract details from flag/value type, or construct the
-- type from some string. This allows custom types to be used instead of
-- the above.
--

class FromText a where
    fromText :: String -> Either String a

instance FromText String where
    fromText = Right

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