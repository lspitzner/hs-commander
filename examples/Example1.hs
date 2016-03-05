{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, DataKinds, TypeOperators, TypeFamilies, ScopedTypeVariables #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Commander.Params   as Params
import Commander.Commands as Commands

--
-- build up a Command (IO ()) object. The IO () is based
-- on the output of the functions used in the commands.
--
someCommands :: Command (IO ())
someCommands = commands $ do

    command "repeat" $ do

        help "Repeat a string n times"

        run $
            \(Value str :: Value "value to repeat" String)
             (Flag n :: Flag '["n"] "times to repeat" Int) ->
             sequence_ $ replicate n (putStrLn str)

    command "calculate" $ do

        help "perform calculations"

        command "add" $ do

            help "add two numbers"

            run $
                \(Value n1 :: Value "number 1" Int)
                 (Value n2 :: Value "number 2" Int)
                 (Flag verbose :: Flag '["v", "verbose"] "verbose mode" Bool) ->
                 if verbose
                    then putStrLn $ (show n1) ++ " + " ++ (show n2) ++ " = " ++ (show $ n1 + n2)
                    else putStrLn (show $ n1 + n2)

        command "multiply" $ do

            help "multiply two numbers"

            run $
                \(Value n1 :: Value "number 1" Int)
                 (Value n2 :: Value "number 2" Int) ->
                 putStrLn $ (show n1) ++ " x " ++ (show n2) ++ " = " ++ (show $ n1 * n2)

    command "login" $ do

        help "pretend authentication"

        run $
            \(Value username :: Value "Username" String)
             (Flag mPassword :: Flag '["p", "password"] "Password" (Maybe String)) -> do
             pass <- case mPassword of
                Just password -> return password
                Nothing -> getLine
             putStrLn $ "logging in with username=" ++ username ++ " password=" ++ pass



--
-- run an IO () command, printing the error or command output:
--
runCommand :: [String] -> [(String,String)] -> Command (IO ()) -> IO ()
runCommand vals flags cmds = case evalCommand vals (Map.fromList flags) cmds of
    Left err -> putStrLn ("Error: " ++ show err)
    Right res -> res

--
-- run some commands. Here, we basically pass in strings denoting a path and flags.
-- Commander maps these to output commands if possible, performing whatever type
-- converstion is required to conform to the flags and values these commands expect
-- in a completely safe way.
--
main :: IO ()
main = do

    putStrLn "\nRepeat Command:"
    runCommand ["repeat", "hello there"] [("n","2")] someCommands

    putStrLn "\nAdd numbers:"
    runCommand ["calculate", "add", "12", "13"] [] someCommands

    putStrLn "\nAdd numbers (verbosely):"
    runCommand ["calculate", "add", "12", "13"] [("verbose", "")] someCommands

    putStrLn "\nMultiply numbers:"
    runCommand ["calculate", "multiply", "12", "13"] [] someCommands

    putStrLn "\nPretend Auth (password provided):"
    runCommand ["login", "james"] [("p", "lemons")] someCommands

    putStrLn "\nPretend Auth (please type a random string):"
    runCommand ["login", "james"] [] someCommands
