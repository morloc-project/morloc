{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Application
  ( appTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

appTests :: TestTree
appTests =
  testGroup
    "APP"
    [ acceptTypecheck
        "spec-app-1-1"
        [r|
        module main (go)
        source Py from "lib.py" ("make_adder" as mkAdder)
        mkAdder :: Int -> Int -> Int
        class Mk a where
          mk :: Int -> a
        instance Mk (Int -> Int) where
          mk n = mkAdder n
        go :: Int -> Int
        go x = (mk 1) x
        |]
    , rejectSaying
        "spec-app-1-4"
        "POLY-7"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        g :: V 3 -> Int
        run :: Int -> (V n -> Int)
        run k x = g x
        |]
    , accept
        "spec-app-1-5"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: Int -> (V n -> Int)
        run :: Int -> V 3 -> Int
        run k xs = f k xs
        |]
    , accept
        "spec-app-1-6"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        type F (n :: Nat) = V n -> Int
        f :: Int -> F n
        run :: Int -> V 3 -> Int
        run k xs = f k xs
        |]
    , rejectSaying
        "spec-app-1-7"
        "POLY-7"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        type F (n :: Nat) = V n -> Int
        g :: V 3 -> Int
        run :: Int -> F n
        run k x = g x
        |]
    , accept
        "spec-app-1-8"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        g :: V n -> Int
        run :: Int -> (V n -> Int)
        run k x = g x
        |]
    , accept
        "spec-app-1-9"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: Int -> (n@Int -> V n)
        run :: Int -> V 3
        run k = f k 3
        |]
    , rejectSaying
        "spec-app-1-10"
        "binds nothing"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: (n@Int -> V n) -> Int
        run :: Int -> Int
        run k = 1
        |]
    , accept
        "spec-app-1-11"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: Int -> (n@Int -> (m@Int -> V m))
        run :: Int -> V 4
        run k = f k 3 4
        |]
    , rejectSaying
        "spec-app-1-12"
        "binds nothing"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: ?n@Int -> V n
        run :: Int -> Int
        run k = 1
        |]
    , rejectSaying
        "spec-app-1-13"
        "binds nothing"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: <IO> n@Int -> V n
        run :: Int -> Int
        run k = 1
        |]
    , rejectSaying
        "spec-app-1-14"
        "given twice"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: n@Int -> n@Int -> V n
        run :: Int -> Int
        run k = 1
        |]
    , rejectSaying
        "spec-app-1-15"
        "binds nothing"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: Eq (n@Int) => n@Int -> V n
        run :: Int -> Int
        run k = 1
        |]
    , rejectSaying
        "spec-app-1-16"
        "binds nothing"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        type W = n@Int
        run :: Int -> Int
        run k = 1
        |]
    , rejectSaying
        "spec-app-1-17"
        "srcLocLine = 5, srcLocCol = 24"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        g :: m@Int -> V m
        h :: Int -> V (m@Int)
        run :: Int -> Int
        run k = 1
        |]
    , rejectSaying
        "spec-app-1-18"
        "binds nothing"
        [r|
        module main (x)
        newtype V (n :: Nat) = Int
        x :: n@Int
        x = 3
        |]
    , rejectSaying
        "spec-app-1-19"
        "binds nothing"
        [r|
        module main (f)
        newtype V (n :: Nat) = Int
        f :: Int -> (Int -> n@Int)
        |]
    , rejectSaying
        "spec-app-1-20"
        "binds nothing"
        [r|
        module main (f)
        import root-py
        type Py => W = n@Int
        f :: Int -> Int
        |]
    , acceptTypecheck
        "spec-app-4-1"
        [r|
        module main (go)
        source Py from "lib.py" ("make_adder" as mkAdder)
        mkAdder :: Int -> Int -> Int
        class Mk a where
          mk :: Int -> a
        instance Mk (Int -> Int) where
          mk n = mkAdder n
        go :: Int -> Int
        go x = mk 1 x
        |]
    , rejectSaying
        "spec-app-4-2"
        "Invalid function application"
        [r|
        module main (go)
        apply1 :: (Int -> a) -> a
        apply1 f = f 1 2
        go :: Int -> Int
        go x = x
        |]
    , rejectSaying
        "spec-app-4-3"
        "Invalid function application"
        [r|
        module main (go)
        effect IO
        source Py from "lib.py" ("later")
        later :: Int -> <IO> a
        go :: Int -> <IO> Int
        go x = later 1 2
        |]
    ]
