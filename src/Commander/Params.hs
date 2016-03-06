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

    -- * Function Parameter Types
    Flag(..),
    Value(..),

    -- * Casting from String
    FromString(..)

) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Text.Read (readMaybe)
import Commander.Commands (ToParam(..), ParamFlags(..), ParamHelp(..))

-- | Use this type in a function embedded in a 'Command' in order to
-- require a flag. The type signature for this lists the flags that we
-- want to match against, the associated help text, and the output type
-- we want the flag to be cast to.
--
-- @Flag@s of @Maybe@ or @Bool@ types have special handling:
-- if the flag doesnt exist for either of these types, we'll be
-- handed back a @False@/@Nothing@ rather than get an error,
-- else if the flag does exist we'll get back a @True@/@Just val@.
-- @Bool@ flags are expected to be provided an empty string as the value;
-- if you care about the value but want it to be optional, use @Maybe@.
data Flag (flags :: [Symbol]) (help :: Symbol) a = Flag a

instance FromString a => ToParam (Flag flags help (Maybe a)) where
    toParam (Just str) = fmap (Flag . Just) (fromString str)
    toParam Nothing = Right (Flag Nothing)
instance ToParam (Flag flags help Bool) where
    toParam (Just "") = Right (Flag True)
    toParam (Just _) = Left "boolean string does not expect to have a value"
    toParam Nothing = Right (Flag False)
instance {-# OVERLAPPABLE #-} FromString a => ToParam (Flag flags help a) where
    toParam (Just str) = fmap Flag (fromString str)
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

instance FromString a => ToParam (Value help a) where
    toParam (Just str) = fmap Value (fromString str)
    toParam Nothing = Left "value expected but none found"
instance ParamFlags (Value help a) where
    paramFlags _ = Nothing
instance KnownSymbol help => ParamHelp (Value help a) where
    paramHelp _ = symbolVal (Proxy :: Proxy help)

-- | Typeclass used by 'Flag' and 'Value' to convert the provided string to
-- the desired haskell type. Anything that satisfies @Read@ will satisfy this,
-- but we can override the @Read@ behaviour as we see fit on a per type basis
-- by explicitly implementing this.
class FromString a where
    fromString :: String -> Either String a

instance FromString String where
    fromString = Right

instance {-# OVERLAPPABLE #-} Read a => FromString a where
    fromString str = case readMaybe str of
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