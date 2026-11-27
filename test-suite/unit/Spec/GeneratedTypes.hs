{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.GeneratedTypes
  ( gen14Tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

gen14Tests :: TestTree
gen14Tests =
  testGroup
    "GEN-14"
    [ reject "spec-gen-14-5" [r|
        module main (P)
        record P where
          x :: Int
        record Py => P = "struct"
        |]
    , accept "spec-gen-14-6" [r|
        module main (P)
        record P where
          x :: Int
        record Py => P = "OrderedDict"
        |]
    , reject "spec-gen-14-7" [r|
        module main (O)
        object O where
          x :: Int
        object Py => O = "struct"
        |]
    , accept "spec-gen-14-8" [r|
        module main (O)
        object O where
          x :: Int
        object Py => O = "Obj"
        |]
    ]
