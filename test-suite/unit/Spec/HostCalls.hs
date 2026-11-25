{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.HostCalls
  ( hostTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

hostTests :: TestTree
hostTests =
  testGroup
    "HOST"
    [ rejectAtLine
        "spec-host-3-1"
        3
        [r|
        module main (g)
        source Py from "c.py" where
          --' @rsize 2
          shift
        shift :: Real -> [Real] -> [Real]
        g :: Int -> Int
        |]
    , accept
        "spec-host-3-2"
        [r|
        module main (g)
        type Shifter = [Real] -> [Real]
        source Py from "c.py" where
          --' @rsize 1
          shift
        shift :: Real -> Shifter
        g :: Int -> Int
        |]
    , reject
        "spec-host-3-3"
        [r|
        module main (g)
        effect IO
        source Py from "c.py" where
          --' @rsize 1
          mk
        mk :: Int -> <IO> (Int -> Int)
        g :: Int -> Int
        |]
    , reject
        "spec-host-3-4"
        [r|
        module main (g)
        class Shift a where
          shift :: a -> [a] -> [a]
        instance Shift Real where
          source Py from "c.py" where
            --' @rsize 2
            shift
        g :: Int -> Int
        |]
    ]
