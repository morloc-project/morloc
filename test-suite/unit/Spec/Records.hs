{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Records
  ( recTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

recTests :: TestTree
recTests =
  testGroup
    "REC"
    [ rejectNaming
        "spec-rec-5-2"
        ["y"]
        []
        [r|
        module main (g)
        record R = R {x :: Int, z :: Str}
        f t = (.x t, .y t)
        g :: R -> (Int, Str)
        g r = f r
        |]
    , rejectNaming
        "spec-rec-5-3"
        ["y"]
        []
        [r|
        module main (g)
        record I = I {x :: Int}
        record O = O {a :: I}
        f t = (.a.x t, .a.y t)
        g :: O -> (Int, Int)
        g o = f o
        |]
    , accept
        "spec-rec-5-4"
        [r|
        module main (g)
        record R = R {x :: Int, z :: Str}
        f t = (.x t, .z t)
        g :: R -> (Int, Str)
        g r = f r
        |]
    , accept
        "spec-rec-5-5"
        [r|
        module main (g)
        record P a = P {v :: a, w :: Int}
        f t = .v t
        g :: P Str -> Str
        g p = f p
        |]
    , rejectNaming
        "spec-rec-5-6"
        ["Str", "Int"]
        []
        [r|
        module main (g)
        f t = (.x t, .y t)
        record R = R {x :: Int, y :: Str}
        g :: R -> (Str, Int)
        g r = f r
        |]
    , rejectNaming
        "spec-rec-5-8"
        ["y"]
        []
        [r|
        module main (g)
        record R = R {x :: Int}
        f t = .y t
        g :: Int -> Int
        g n = f {x = n}
        |]
    , accept
        "spec-rec-5-7"
        [r|
        module main (g)
        record R = R {k0 :: Int, k1 :: Int, k2 :: Int, k3 :: Int, k4 :: Int, k5 :: Int, k6 :: Int, k7 :: Int, k8 :: Int, k9 :: Int, k10 :: Int, k11 :: Int, k12 :: Int, k13 :: Int, k14 :: Int, k15 :: Int}
        f t = [.k0 t, .k1 t, .k2 t, .k3 t, .k4 t, .k5 t, .k6 t, .k7 t, .k8 t, .k9 t, .k10 t, .k11 t, .k12 t, .k13 t, .k14 t, .k15 t]
        g :: R -> [Int]
        g r = f r
        |]
    ]
