{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Effects
  ( effTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

effTests :: TestTree
effTests =
  testGroup
    "EFF"
    [ rejectSaying
        "spec-eff-13-1"
        "Inescapable effect"
        [r|
        module main (f)
        effect IO
        source Py from "lib.py" ("tick")
        tick :: Int -> <IO> Int
        class Run a where
          run :: a -> Int
        instance Run (Int -> <IO> Int) where
          run g = 1
        f :: Int -> Int
        f x = run tick
        |]
    , accept
        "spec-eff-13-2"
        [r|
        module main (f)
        effect IO
        source Py from "lib.py" ("tick")
        tick :: Int -> <IO> Int
        class Run a where
          run :: a -> <IO> Int
        instance Run (Int -> <IO> Int) where
          run g = g 1
        f :: Int -> <IO> Int
        f x = run tick
        |]
    , rejectSaying
        "spec-eff-13-3"
        "Inescapable effect"
        [r|
        module main (f)
        effect IO
        source Py from "lib.py" ("tick", "run_cb" as runCb)
        tick :: Int -> <IO> Int
        runCb :: (Int -> <IO> Int) -> Int
        f :: Int -> Int
        f x = runCb tick
        |]
    , rejectSaying
        "spec-eff-13-4"
        "Inescapable effect"
        [r|
        module main (f)
        effect IO
        source Py from "lib.py" ("tick")
        tick :: Int -> <IO> Int
        class Run a where
          run :: a -> Int
        instance Run (<IO> Int) where
          run g = 1
        f :: Int -> Int
        f x = run (tick x)
        |]
    , accept
        "spec-eff-13-5"
        [r|
        module main (f)
        effect IO
        source Py from "lib.py" ("tick")
        tick :: Int -> <IO> Int
        class Run a where
          run :: a -> <IO> Int
        instance Run (<IO> Int) where
          run g = g
        f :: Int -> <IO> Int
        f x = run (tick x)
        |]
    , rejectSaying
        "spec-eff-13-6"
        "Inescapable effect"
        [r|
        module main (f)
        effect IO
        source Py from "lib.py" ("tick")
        tick :: Int -> <IO> Int
        class Run a where
          run :: a -> Int
        instance Run (Int -> <IO> Int)
        f :: Int -> Int
        f x = run tick
        |]
    , rejectSaying
        "an undeclared effect in an instance head is rejected"
        "Undeclared effect"
        [r|
        module main (f)
        class Run a where
          run :: a -> Int
        instance Run (Int -> <Bogus> Int) where
          run g = 1
        f :: Int -> Int
        f x = 1
        |]
    ]
