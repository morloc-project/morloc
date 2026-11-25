{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Diagnostics
  ( diagTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

diagTests :: TestTree
diagTests =
  testGroup
    "DIAG"
    [ rejectSaying
        "spec-diag-1-1"
        "inside an instance of"
        [r|
        module main (bar)
        class Foo a where
          foo :: a -> a
        instance Foo Int where
          foo :: Int -> Int
          foo x = x
        bar :: Int -> Int
        bar x = foo x
        |]
    ]
