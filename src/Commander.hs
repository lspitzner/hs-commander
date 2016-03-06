-- |
-- Module: Commander
--
-- re-exports everything defined in @Commander.Params@ and @Commander.Commands@
-- for convenience.
--
module Commander (

    -- * Example Usage
    -- $usage

    module Commander.Params,
    module Commander.Commands
) where

import Commander.Params
import Commander.Commands

-- $usage
--
-- The below header gives us the language extensions and imports we need
-- for basic usage of 'Commander':
--
-- > {-# LANGUAGE DataKinds, ScopedTypeVariables #-}
-- >
-- > module Main where
-- >
-- > import qualified Data.Map as Map
-- > import Commander.Params   (Flag(..), Value(..))
-- > import Commander.Commands (Command(..), commands, command, help, run, evalCommand)
--
-- The next step is to define the various commands that we care to match
-- against, which looks something like this:
--
-- > someCommands :: Command (IO ())
-- > someCommands = commands $ do
-- >
-- >     command "repeat" $ do
-- >
-- >         help "Repeat a string n times"
-- >
-- >         run $
-- >             \(Value str :: Value "value to repeat" String)
-- >              (Flag n :: Flag '["n"] "times to repeat" Int) ->
-- >              sequence_ $ replicate n (putStrLn str)
-- >
-- >     command "calculate" $ do
-- >
-- >         help "perform calculations"
-- >
-- >         command "add" $ do
-- >
-- >             help "add two numbers"
-- >
-- >             run $
-- >                 \(Value n1 :: Value "number 1" Int)
-- >                  (Value n2 :: Value "number 2" Int)
-- >                  (Flag verbose :: Flag '["v", "verbose"] "verbose mode" Bool) ->
-- >                  if verbose
-- >                     then putStrLn $ (show n1) ++ " + " ++ (show n2) ++ " = " ++ (show $ n1 + n2)
-- >                     else putStrLn (show $ n1 + n2)
-- >
-- >         command "multiply" $ do
-- >
-- >             help "multiply two numbers"
-- >
-- >             run $
-- >                 \(Value n1 :: Value "number 1" Int)
-- >                  (Value n2 :: Value "number 2" Int) ->
-- >                  putStrLn $ (show n1) ++ " x " ++ (show n2) ++ " = " ++ (show $ n1 * n2)
-- >
-- >     command "login" $ do
-- >
-- >         help "pretend authentication"
-- >
-- >         run $
-- >             \(Value username :: Value "Username" String)
-- >              (Flag mPassword :: Flag '["p", "password"] "Password" (Maybe String)) -> do
-- >              pass <- case mPassword of
-- >                 Just password -> return password
-- >                 Nothing -> getLine
-- >              putStrLn $ "logging in with username=" ++ username ++ " password=" ++ pass
--
-- Commands can be arbitrary nested, making it super easy to define subcommands as far
-- down as you like.
--
-- Using a couple of pre-baked parameter types from 'Commander.Params', namely 'Value' and 'Flag',
-- we define in the function signature itself additional /values/ and /flags/ that we expect,
-- each fully typed and with help text (the 'Flag' type in addition state which flags it will
-- try to match against).
--
-- If you prefer alternate behaviour to the provided types, it is easy to create your own custom
-- alternatives; it's simply a case of making your custom types instances of a few very basic
-- typeclasses.
--
-- The return types of the provided functions must match, so that we know what we're getting back
-- when we try executing a command; this is the only parameter needed by the 'Command' type.
--
-- Making use of the above, one could do the following:
--
-- > runCommand :: [String] -> [(String,String)] -> Command (IO ()) -> IO ()
-- > runCommand vals flags cmds = case evalCommand vals (Map.fromList flags) cmds of
-- >     Left err -> putStrLn ("Error: " ++ show err)
-- >     Right res -> res
-- >
-- > main :: IO ()
-- > main = do
-- >
-- >     putStrLn "\nRepeat Command:"
-- >     runCommand ["repeat", "hello there"] [("n","2")] someCommands
-- >
-- >     putStrLn "\nAdd numbers:"
-- >     runCommand ["calculate", "add", "12", "13"] [] someCommands
-- >
-- >     putStrLn "\nAdd numbers (verbosely):"
-- >     runCommand ["calculate", "add", "12", "13"] [("verbose", "")] someCommands
-- >
-- >     putStrLn "\nMultiply numbers:"
-- >     runCommand ["calculate", "multiply", "12", "13"] [] someCommands
-- >
-- >     putStrLn "\nPretend Auth (password provided):"
-- >     runCommand ["login", "james"] [("p", "lemons")] someCommands
-- >
-- >     putStrLn "\nPretend Auth (please type a random string):"
-- >     runCommand ["login", "james"] [] someCommands
--
-- Where we first define a function that makes use of 'evalCommand' to match the provided
-- flags and values against a specific command and either run it or return an error, and
-- either prints the error or runs the resulting IO action.
--
-- This library has no opinion on how a text based command is parsed into a path list and
-- 'Map' of flags, and so the user is free to select an approach that works best for them.
--












