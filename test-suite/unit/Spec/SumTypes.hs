{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.SumTypes
  ( sumTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

sumTests :: TestTree
sumTests =
  testGroup
    "SUM"
    [ rejectNaming
        "spec-sum-2-1"
        ["Constructor", "X", "unique"]
        []
        [r|
        data A = X
        data B = X | Y
        f :: B -> Int
        f b = 1
        |]
    , accept
        "spec-sum-2-2"
        [r|
        data A = X
        data B = Z | Y
        f :: B -> Int
        f b = 1
        |]
    ]
