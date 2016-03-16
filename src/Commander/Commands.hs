-- |
-- Module: Commander.Commands
--
-- This module contains the core types and functions for working with them.
--
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    UndecidableInstances,
    ScopedTypeVariables,
    TypeFamilies,
    ConstraintKinds,
    ExistentialQuantification #-}

module Commander.Commands (

    -- * Creating new Commands
    -- $creatingcommands

    -- ** Core types
    Command(..),
    Commands,
    CommandError(..),

    -- ** Attaching values to Commands
    -- $attachingvalues
    commands,
    command,
    help,
    run,

    -- * Extracting and running Commands
    -- $runningcommands
    evalCommand,
    getCommand,

    -- * Function parameters
    -- $functionparameters

    -- ** Creating new function parameters
    IsParameter,
    ToParam(..),
    ParamFlags(..),
    ParamHelp(..),

    -- ** Working with function parameters
    Fn(..),
    injectParams,
    extractParams,
    Parameter(..)

) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Control.Monad.State as State
import Data.Map (Map)
import Data.Proxy (Proxy(..))

-- | A tuple of typeclasses that must all be implemented for function parameter
-- types in order that they can be used in the functions attached to commands.
-- 'ToParam' is the only mandatory requirement ('ParamFlags' and 'ParamHelp'
-- have default defitions), however it is recommended that you implement
-- 'ParamHelp' in all cases, and you must implement 'ParamFlags' if you want
-- your new type to match against provided flags. An example custom parameter
-- implementation:
--
-- > data Verbose = Verbose Bool
-- >
-- > instance ParamFlags Verbose where
-- >     paramFlags _ = Just ["v", "verbose"]
-- > instance ParamHelp Verbose where
-- >     paramHelp _ = "Make the command more verbose"
-- > instance ToParam Verbose where
-- >     toParam (Just _) = Right (Verbose True)
-- >     toParam Nothing = Right (Verbose False)
--
-- Here, we define a @Verbose@ type that will equal @Verbose True@ if the "v"
-- or "verbose" flag is used in the command, or @Verbose False@ otherwise.
--
-- To extract the value, all we have to do is use @(Verbose b)@ in our commands
-- function now, where @b@ will be either @True@ or @False@.
--
-- See 'Commander.Params' for the definitions of the provided 'Value' and 'Flag'
-- parameter types.
--
type IsParameter a = (ToParam a, ParamFlags a, ParamHelp a)

-- | Describe how to turn the @String@ parameter given into our custom type.
-- The input may be @Nothing@ if the flag/value is not provided, else it will
-- be @Just str@ where @str@ is the input string.
class ToParam a where
    toParam :: Maybe String -> Either String a

-- | Should the parameter match against flags? If so, return @Just [flags]@
-- from this. If the param should be a value instead, return @Nothing@.
class ParamFlags a where
    paramFlags :: proxy a -> Maybe [String]
    paramFlags _ = Nothing

-- | Return a piece of help text describing what the parameter means.
class ParamHelp a where
    paramHelp  :: proxy a -> String
    paramHelp _ = ""

-- | given our @Fn out@ type, containing some function that will return @out@ on
-- successful execution, attempt to run the function by injecting a list of values
-- and a map of flags to it. This will either return a 'CommandError' denoting
-- what failed, or the output from running the function.
injectParams :: [String] -> Map String String -> Fn out -> Either CommandError out
injectParams vals flags fnWrapper = case fnWrapper of Fn fn _ -> injectParameters vals flags fn

type family FnOut fn where
    FnOut (a -> b) = FnOut b
    FnOut a = a

class FnOut fn ~ out => InjectParameters fn out where
    injectParameters :: [String] -> Map String String -> fn -> Either CommandError out

instance (IsParameter a, InjectParameters b out) => InjectParameters (a -> b) out where
    injectParameters vals flags fn = case paramFlags (Proxy :: Proxy a) of
        -- the thing looks like a flag, but doesnt actually have any!
        Just [] -> Left ErrParamHasNoFlags
        -- the thing does have flags.
        Just fs -> injectFlag fs
        -- the thing is a value.
        Nothing -> injectValue
      where
        injectFlag :: [String] -> Either CommandError out
        injectFlag fs = do
            let (flag, mVal)
                    = Maybe.fromMaybe (head fs, Nothing)
                    $ Maybe.listToMaybe
                    $ filter (Maybe.isJust . snd)
                    $ fmap (\f -> (f, Map.lookup f flags)) fs
            param <- mapLeft (ErrCastingFlag flag) $ toParam mVal
            injectParameters vals flags (fn param)
        injectValue :: Either CommandError out
        injectValue = do
            val <- toEither ErrNotEnoughValues $ Maybe.listToMaybe vals
            param <- mapLeft ErrCastingValue $ toParam (Just val)
            injectParameters (tail vals) flags (fn param)

instance {-# OVERLAPPABLE #-} FnOut out ~ out => InjectParameters out out where
    injectParameters [] _ output = Right output
    injectParameters vs _ _ = Left (ErrTooManyValues vs)

-- | A type containing information about a function parameter.
data Parameter = Parameter
    { parameterFlags :: Maybe [String]
    , parameterHelp  :: String
    } deriving (Show, Eq)

-- | Run against our @Fn out@ wrapped function, this will return a list of 'Parameter' details
-- for each parameter in the contained function.
extractParams :: Fn out -> [Parameter]
extractParams fnWrapper = case fnWrapper of Fn _ params -> params

class HasParameters fn where
    getParameters :: proxy fn -> [Parameter]

instance (IsParameter a, HasParameters b) => HasParameters (a -> b) where
    getParameters _ = param : getParameters (Proxy :: Proxy b)
      where param = Parameter (paramFlags proxya) (paramHelp proxya)
            proxya = Proxy :: Proxy a

instance {-# OVERLAPPABLE #-} FnOut out ~ out => HasParameters out where
    getParameters _ = []

-- | Our existential 'Fn' type is used for hiding away the details of some provided
-- function. Any function that satisfies the 'IsParameter' tuple of type classes can
-- be wrapped in this.
data Fn out = forall fn. InjectParameters fn out
           => Fn fn [Parameter]

instance Show (Fn out) where
    show _ = "<<injectableFunc>>"

-- | This is the type returned from using the 'commands' function along with helpers like
-- 'command' and 'run' to build up a nested structure of commands. We can manually traverse
-- it by looking through the 'cmdChildren' Map to acess nested commands, or inspecting the
-- 'cmdHelp' and 'cmdFunc' properties of the current command. This makes it easy to do things
-- like autocomplete commands, or print out help etc.
data Command out = Command
    { cmdChildren :: Map String (Command out)
    , cmdHelp     :: String
    , cmdFunc     :: Maybe (Fn out)
    } deriving Show

-- | You probably won't ever need to interact with this type; it is just a @State@ monad on our
-- 'Command' type in order that we can use monadic notation to build up our nested structure.
type Commands out = State.State (Command out)

-- $attachingvalues
--
-- These functions allow us to build up our nested command structure.
--

emptyCommand :: Command out
emptyCommand = Command Map.empty "" Nothing

-- | Given a 'Commands' type as its only argument, this resolves it to a 'Command' object, ready
-- to make use of. This is basically the entry point to defining our commands, inside which we
-- can use the functions below to populate our structure.
commands :: Commands out () -> Command out
commands m = State.execState m emptyCommand

-- | Nest a command with some name inside the current command.
command :: String -> Commands out () -> Commands out ()
command name m = State.modify $ \c -> c { cmdChildren = Map.insert name (commands m) (cmdChildren c) }

-- | Attach help to the current command.
help :: String -> Commands out ()
help txt = State.modify $ \c -> c { cmdHelp = txt }

-- | Attach a function which will be tried if the current command is matched. The parameters
-- to the function must satisfy the 'IsParameter' typeclasses, which will automatically make
-- the function satisfy the @ExtractParameters@ and @InjectParameters@ typeclasses.
run
  :: forall fn out
   . (HasParameters fn, InjectParameters fn out)
  => fn
  -> Commands out ()
run fn = State.modify
       $ \c -> c {
           cmdFunc = Just (Fn fn (getParameters (Proxy :: Proxy fn)))
         }

-- $runningcommands
--
-- The below are helpers for simpler interaction with our 'Command' object.
--

-- | Attempt to run a function inside a 'Command' object, using the first argument (a
-- list of strings) to first navigate to the relevant subcommand and then have any
-- remainder used as values to be passed to the command, and the second argument as
-- a map of flags to be passed to the command.
evalCommand :: [String] -> Map String String -> Command out -> Either CommandError out
evalCommand path flags cmd = eval
  where
    -- eval cmd, trying to do nav step if fails:
    eval = case cmdFunc cmd of
        Nothing -> nav
        Just fn -> injectParams path flags fn `catchEither` nav
    -- navigate, complaining if we can't:
    nav = do
        (newPath, newCmd) <- stepIntoCommand path cmd
        evalCommand newPath flags newCmd

-- | Attempt to get hold of the nested 'Command' at the path provided inside a provided
-- 'Command' object.
getCommand :: [String] -> Command out -> Either CommandError (Command out)
getCommand [] cmd = Right cmd
getCommand path cmd = do
    (newPath, newCmd) <- stepIntoCommand path cmd
    getCommand newPath newCmd

stepIntoCommand :: [String] -> Command out -> Either CommandError ([String],Command out)
stepIntoCommand path cmd = do
    crumb <- toEither (ErrNotEnoughPath childKeys) $ Maybe.listToMaybe path
    newCmd <- toEither (ErrPathNotFound childKeys crumb) $ Map.lookup crumb children
    return (tail path, newCmd)
  where
    children = cmdChildren cmd
    childKeys = Map.keys children

-- | A collection of the errors that can be encountered upon trying to get and run
-- a 'Command'
data CommandError
    -- | If a parameter's 'ParamFlags' instance returns @Just []@, complain:
    = ErrParamHasNoFlags
    -- | More input values are provided than the function requires. Provides the list
    -- of remaining values.
    | ErrTooManyValues [String]
    -- | Not enough input values are provided to the function, so it can't run.
    | ErrNotEnoughValues
    -- | We didn't find any function with the given path. Provides the possible
    -- path pieces that could have been supplied to go one level deeper.
    | ErrNotEnoughPath [String]
    -- | We didn't find a path corresponding to some string. Returns the possible
    -- paths that could have been taken from that location, and the failing string.
    | ErrPathNotFound [String] String
    -- | We tried converting the flag (provided as the first param) to the type asked
    -- for, and failed for some reason (provided as the second param).
    | ErrCastingFlag String String
    -- | We tried converting some value to the type asked and failed with the reason
    -- provided.
    | ErrCastingValue String
    deriving (Eq,Show)

--
-- Util bits for internal use
--

toEither :: a -> Maybe b -> Either a b
toEither _ (Just b) = Right b
toEither a Nothing = Left a

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft fn (Left a)  = Left (fn a)
mapLeft _ (Right b) = Right b

catchEither :: Either a b -> Either a b -> Either a b
catchEither (Left a) (Left _) = Left a
catchEither (Left _) b = b
catchEither a _ = a

-- $creatingcommands
--
-- These types and functions are involved in building up a @Command out@ object, where @out@ is
-- the output type of the functions attached to the different command paths.
--

-- $functionparameters
--
-- Functions are wrapped up inside an existential 'Fn' type in order that we can hide away their
-- implementation details and satisfy the type system. In order for a function to be wrappable
-- inside this type, you need only actually satisfy the 'IsParameter' tuple of typeclasses
-- for the types of any of the arguments to the function. Of these, only the 'ToParam'
-- class is actually mandatory.
--