{-# LANGUAGE IncoherentInstances, FunctionalDependencies, GADTs, RankNTypes, DataKinds, TypeOperators, KindSignatures, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}

module Commander (
    module Params,
    module Commands
) where

import Commander.Params   as Params
import Commander.Commands as Commands


--
-- testing lark
--
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


--testFn1 = \(Flag a :: Flag Int  '["c"] "count")
--           (Flag s :: Flag Char '["s"] "string") ->
--           putStrLn ("one! " ++ [s] ++ show a)
--testFn2 = \(Value a :: Value Int "some value1")
--           (Value b :: Value Int "some value2") ->
--           putStrLn ("two! " ++ show a ++ show b)

--testFn3 = \(Flag a  :: Flag Int '["c"] "count")
--           (Value b :: Value Int "some value2") ->
--           putStrLn ("three! " ++ show a ++ show b)

