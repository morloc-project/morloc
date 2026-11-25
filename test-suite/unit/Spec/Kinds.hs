{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Kinds
  ( kindTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

kindTests :: TestTree
kindTests =
  testGroup
    "KIND"
    [ accept
        "spec-kind-6-2"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type MyTable n = Tbl n {x = Int}
        f :: MyTable -> Int
        |]
    , accept
        "spec-kind-6-3"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type MyTable n = Tbl n {x = Int}
        f :: MyTable 3 -> Int
        |]
    , accept
        "spec-kind-6-1"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        type V (n :: Nat) a = Vector n a
        f :: V Int -> Int
        |]
    , reject
        "spec-kind-2-1"
        [r|
        module main (f)
        type R = Singleton "x" Int
        f :: R -> Int
        f x = 1
        |]
    , accept
        "spec-kind-2-2"
        [r|
        module main (f)
        type R = {x = Int}
        f :: R -> Int
        f x = 1
        |]
    , reject
        "spec-kind-2-3"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        f :: Vector -> Int
        f x = 1
        |]
    , accept
        "spec-kind-2-4"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        f :: Vector Int -> Int
        f x = 1
        |]
    , accept
        "spec-kind-2-5"
        [r|
        module main (f)
        type P a = {x = a}
        f :: P Int -> Int
        f x = 1
        |]
    , reject
        "spec-kind-2-6"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        f :: [Vector] -> Int
        f x = 1
        |]
    , reject
        "spec-kind-2-7"
        [r|
        module main (f)
        f :: {x = Singleton "y" Int} -> Int
        f x = 1
        |]
    , reject
        "spec-kind-2-8"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        type V = Vector
        f :: V -> Int
        f x = 1
        |]
    , reject
        "spec-kind-2-9"
        [r|
        module main (f)
        class Sh a where
          sh :: a -> Singleton "x" Int
        f :: Int -> Int
        f x = 1
        |]
    , reject
        "spec-kind-5-4"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type X = Tbl Int
        f :: X -> Int
        f x = 1
        |]
    , reject
        "spec-kind-5-1"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        f :: Tbl 3 4 -> Int
        f x = 1
        |]
    , reject
        "spec-kind-5-2"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        f :: Tbl Int -> Int
        f x = 1
        |]
    , accept
        "spec-kind-5-3"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        f :: Tbl 3 {x = Int} -> Int
        f x = 1
        |]
    , reject
        "spec-kind-4-1"
        [r|
        module main (g)
        newtype Buffer (n :: Nat) a = List a
        f :: Buffer (3 - 10) Int -> Int
        g :: Int -> Int
        g x = x
        |]
    , reject
        "spec-tab-3-1"
        [r|
        module main (oops)
        newtype Table (n :: Nat) (r :: Rec) = Int
        getCol :: f@Str -> Table n r -> [ProjectField r f]
        mk :: Table 2 {x = Int, y = Str}
        oops = getCol "z" mk
        |]
    , accept
        "spec-tab-3-2"
        [r|
        module main (ok)
        newtype Table (n :: Nat) (r :: Rec) = Int
        getCol :: f@Str -> Table n r -> [ProjectField r f]
        mk :: Table 2 {x = Int, y = Str}
        ok = getCol "x" mk
        |]
    , accept
        "spec-kind-4-2"
        [r|
        module main (g)
        newtype Buffer (n :: Nat) a = List a
        f :: Buffer (10 - 3) Int -> Int
        g :: Int -> Int
        g x = x
        |]
    , accept
        "spec-kind-4-3"
        [r|
        module main (ok, empty)
        newtype Vector (n :: Nat) a = List a
        roll :: w@Int -> Vector n Real -> Vector (n - w + 1) Real
        src :: Vector 10 Real
        ok = roll 3 src
        empty = roll 11 src
        |]
    , reject
        "spec-kind-4-4"
        [r|
        module main (bad)
        newtype Vector (n :: Nat) a = List a
        roll :: w@Int -> Vector n Real -> Vector (n - w + 1) Real
        src :: Vector 10 Real
        bad = roll 12 src
        |]
    , accept
        "spec-kind-4-5"
        [r|
        module main (bad)
        newtype Vector (n :: Nat) a = List a
        roll :: w@Int -> Vector n Real -> Vector (n - w + 1) Real
        src :: Vector 10 Real
        bad = roll (-2) src
        |]
    , reject
        "spec-kind-4-6"
        [r|
        module main (g)
        newtype Vector (n :: Nat) a = List a
        f :: Vector (n - (n + 1)) Int -> Int
        g :: Int -> Int
        g x = x
        |]
    , accept
        "spec-kind-4-7"
        [r|
        module main (g)
        newtype Buffer (n :: Nat) a = List a
        f :: Buffer ((3 - 10) + 20) Int -> Int
        g :: Int -> Int
        g x = x
        |]
    , reject
        "spec-kind-4-8"
        [r|
        module main (g)
        newtype Buffer (n :: Nat) a = List a
        f :: Buffer (7 / 0) Int -> Int
        g :: Int -> Int
        g x = x
        |]
    ]
