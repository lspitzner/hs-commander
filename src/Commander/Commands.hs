{-# LANGUAGE
    MultiParamTypeClasses,
    FunctionalDependencies,
    FlexibleInstances,
    IncoherentInstances,
    UndecidableInstances,
    ScopedTypeVariables #-}

module Commander.Commands (

    --
    -- What can we inject?
    --
    IsParameter(..),

    --
    -- Our output structure and state monad:
    --
    Command(..),
    Commands(..),

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
class IsParameter a where
    paramFlags :: proxy a -> Maybe [String]
    paramFlags _ = Nothing

    paramHelp  :: proxy a -> String
    paramHelp _ = ""

    toParam    :: String -> Maybe a

--
-- Given some args that satisfy IsParameter, attempt to run a function
-- and output its result.
--

class InjectParameters fn out | fn -> out where
    inject :: Map String String -> [String] -> fn -> Maybe out

instance (IsParameter a, InjectParameters b out) => InjectParameters (a -> b) out where
    inject flags vals fn = case paramFlags (Proxy :: Proxy a) of
        Just fs -> injectFlag fs
        Nothing -> injectValue
      where
        injectFlag fs = do
            str <- Maybe.listToMaybe $ Maybe.catMaybes $ fmap (flip Map.lookup flags) fs
            param <- toParam str
            inject flags vals (fn param)
        injectValue = do
            str <- Maybe.listToMaybe vals
            param <- toParam str
            inject flags (tail vals) (fn param)

instance {-# OVERLAPPABLE #-} InjectParameters out out where
    inject _ _ output = Just output

--
-- Now, let's define a data type to put this stuff into.
--

data Fn out = forall fn. InjectParameters fn out => Fn fn

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

run :: forall fn out. InjectParameters fn out => fn -> Commands out ()
run fn = State.modify $ \c -> c { cmdFunc = Just (Fn fn) }