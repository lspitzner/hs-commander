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
    CommandError(..),

    --
    -- Functions that work in the Commands monad:
    --
    emptyCommand,
    commands,
    command,
    help,
    run,

    --
    -- Functions that make use of the Command struct
    --
    evalCommand,
    getCommand

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
    toParam :: Maybe String -> Either String a

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

injectParams :: [String] -> Map String String -> Fn out -> Either CommandError out
injectParams vals flags fnWrapper = case fnWrapper of Fn fn -> injectParameters vals flags fn

type family FnOut fn where
    FnOut (a -> b) = FnOut b
    FnOut a = a

class FnOut fn ~ out => InjectParameters fn out where
    injectParameters :: [String] -> Map String String -> fn -> Either CommandError out

instance (IsParameter a, InjectParameters b out) => InjectParameters (a -> b) out where
    injectParameters vals flags fn = case paramFlags (Proxy :: Proxy a) of
        -- the thing looks like a flag, but doesnt actually have any!
        Just [] -> Left (ErrFlagNotFound [])
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

--
-- Making use of our command object
--

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

--
-- When things go wrong..
--

data CommandError
    = ErrFlagNotFound [String]
    | ErrTooManyValues [String]
    | ErrNotEnoughValues
    | ErrNotEnoughPath [String]       -- possible path pieces we expect to see but didn't
    | ErrPathNotFound [String] String -- possible path pieces, path piece provided.
    | ErrCastingFlag String String    -- flag that had issues, reason for issue
    | ErrCastingValue String          -- reason for issue
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
