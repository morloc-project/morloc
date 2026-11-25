{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.BaseTypes
  ( baseTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

baseTests :: TestTree
baseTests =
  testGroup
    "BASE"
    [ rejectUser
        "spec-base-3-2"
        [r|
        module main (f)
          (&&) :: Bool -> Bool -> Bool
          (&&) x y = x
          f :: Bool -> Bool
          f x = x && x
        |]
    , accept
        "spec-base-3-3"
        [r|
        module main (f)
          (&.) :: Bool -> Bool -> Bool
          (&.) x y = x
          f :: Bool -> Bool
          f x = x &. x
        |]
    ]
