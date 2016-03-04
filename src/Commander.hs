{-# LANGUAGE IncoherentInstances, FunctionalDependencies, GADTs, RankNTypes, DataKinds, TypeOperators, KindSignatures, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}

module Commander (
    module Params,
    module Commands
) where

import Commander.Params   as Params
import Commander.Commands as Commands


----
---- testing lark
----
--import qualified Data.Map as Map

--test = commands $ do

--    command "hello" $ do

--        help "This is the hello command"

--        command "no" (return ())

--        command "wee" $ do
--            help "This is the weeeee command."
--            run $
--                \(Flag a :: Flag '["c"] "count"  Int)
--                 (Flag s :: Flag '["s"] "string" Char) ->
--                  putStrLn ("one! " ++ [s] ++ show a)

--    command "bye" $ do

--        help "This is the bye command"

--        command "woop" (return ())


--testFn1 = \(Flag a :: Flag   '["c"] "count" Int)
--           (Flag s :: Flag  '["s"] "string" Char) ->
--           putStrLn ("one! " ++ [s] ++ show a)
--testFn2 = \(Value a :: Value  "some value1" Int)
--           (Value b :: Value  "some value2" Int) ->
--           putStrLn ("two! " ++ show a ++ show b)

--testFn3 = \(Flag a  :: Flag  '["c"] "count" Int)
--           (Value b :: Value  "some value2" Int) ->
--           putStrLn ("three! " ++ show a ++ show b)

--runTest vals flags fn = case injectParams (Map.fromList flags) vals (Fn fn) of
--    Left err -> putStrLn ("Error: " ++ show err)
--    Right res -> res