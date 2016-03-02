{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    UndecidableInstances,
    ScopedTypeVariables,
    TypeFamilies,
    ConstraintKinds,
    ExistentialQuantification #-}

module Commander.Commands (

    --
    -- What can we inject?
    --
    IsParameter,
    ToParam(..),
    ParamFlags(..),
    ParamHelp(..),

    Fn(..),
    injectParams,
    extractParams,
    Parameter(..),

    --
    -- Our output structure and state monad:
    --
    Command(..),
    Commands,

    --
    -- Functions that work in the Commands monad:
    --
    emptyCommand,
    commands,
    command,
    help,
    run

) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Control.Monad.State as State
import Data.Map (Map)
import Data.Proxy (Proxy(..))

--
-- Make things an instance of this to allow them to be parameters
-- to the injectable functions.
--
type IsParameter a = (ToParam a, ParamFlags a, ParamHelp a)

class ToParam a where
    toParam :: String -> Maybe a

class ParamFlags a where
    paramFlags :: proxy a -> Maybe [String]
    paramFlags _ = Nothing

class ParamHelp a where
    paramHelp  :: proxy a -> String
    paramHelp _ = ""

--
-- Given some args that satisfy IsParameter, attempt to run a function
-- and output its result. We use a type family to tie our fn and out params.
--

injectParams :: Map String String -> [String] -> Fn out -> Maybe out
injectParams flags vals fnWrapper = case fnWrapper of Fn fn -> injectParameters flags vals fn

type family FnOut fn where
    FnOut (a -> b) = FnOut b
    FnOut a = a

class FnOut fn ~ out => InjectParameters fn out where
    injectParameters :: Map String String -> [String] -> fn -> Maybe out

instance (IsParameter a, InjectParameters b out) => InjectParameters (a -> b) out where
    injectParameters flags vals fn = case paramFlags (Proxy :: Proxy a) of
        Just fs -> injectFlag fs
        Nothing -> injectValue
      where
        injectFlag fs = do
            str <- Maybe.listToMaybe $ Maybe.catMaybes $ fmap (flip Map.lookup flags) fs
            param <- toParam str
            injectParameters flags vals (fn param)
        injectValue = do
            str <- Maybe.listToMaybe vals
            param <- toParam str
            injectParameters flags (tail vals) (fn param)

instance {-# OVERLAPPABLE #-} FnOut out ~ out => InjectParameters out out where
    injectParameters _ _ output = Just output

--
-- In addition to passing parameters in, we just want to get some details out
-- for help etc.
--
data Parameter = Parameter
    { parameterFlags :: Maybe [String]
    , parameterHelp  :: String
    } deriving (Show, Eq)

extractParams :: Fn out -> [Parameter]
extractParams fnWrapper = case fnWrapper of Fn (_ :: a) -> extractParameters (Proxy :: Proxy a) (Proxy :: Proxy out)

class FnOut fn ~ out => ExtractParameters fn out where
    extractParameters :: proxy fn -> proxy out -> [Parameter]

instance (IsParameter a, ExtractParameters b out) => ExtractParameters (a -> b) out where
    extractParameters _ _ = param : extractParameters (Proxy :: Proxy b) (Proxy :: Proxy out)
      where param = Parameter (paramFlags proxya) (paramHelp proxya)
            proxya = Proxy :: Proxy a

instance {-# OVERLAPPABLE #-} FnOut out ~ out => ExtractParameters out out where
    extractParameters _ _ = []

--
-- Now, let's define a data type to put this stuff into.
--

data Fn out = forall fn. (ExtractParameters fn out, InjectParameters fn out) => Fn fn

instance Show (Fn out) where
    show _ = "<<injectableFunc>>"

data Command out = Command
    { cmdChildren :: Map String (Command out)
    , cmdHelp     :: String
    , cmdFunc     :: Maybe (Fn out)
    } deriving Show

type Commands out = State.State (Command out)

--
-- And functions to populate this state
--

emptyCommand :: Command out
emptyCommand = Command Map.empty "" Nothing

commands :: Commands out () -> Command out
commands m = State.execState m emptyCommand

command :: String -> Commands out () -> Commands out ()
command name m = State.modify $ \c -> c { cmdChildren = Map.insert name (commands m) (cmdChildren c) }

help :: String -> Commands out ()
help txt = State.modify $ \c -> c { cmdHelp = txt }

run :: (ExtractParameters fn out, InjectParameters fn out) => fn -> Commands out ()
run fn = State.modify $ \c -> c { cmdFunc = Just (Fn fn) }


