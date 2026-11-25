{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Interfaces
  ( ifaceTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

ifaceTests :: TestTree
ifaceTests =
  testGroup
    "IFACE"
    [ reject
        "spec-iface-6-1"
        [r|
        module main (foo)
        fmt :: Int -> Str
        --' @render --raw=fmt
        foo :: Int -> Int
        foo x = x
        |]
    , accept
        "spec-iface-6-2"
        [r|
        module main (foo)
        fmt :: Int -> Str
        --' @render --rawest=fmt
        foo :: Int -> Int
        foo x = x
        |]
    , reject
        "spec-iface-6-3"
        [r|
        module main (foo)
        fmt :: Int -> Str
        --' @with -r/--raw=fmt
        foo :: Int -> Int
        foo x = x
        |]
    , accept
        "spec-iface-6-4"
        [r|
        module main (foo)
        fmt :: Int -> Str
        --' @with -r/--rawest=fmt
        foo :: Int -> Int
        foo x = x
        |]
    ]
