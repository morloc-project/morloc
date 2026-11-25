{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Collections
  ( collTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

collTests :: TestTree
collTests =
  testGroup
    "COLL"
    [ rejectNaming
        "spec-coll-7-1"
        ["arity"]
        []
        [r|
        module main (g)
        f t = .2 t
        g :: (Int, Str) -> Int
        g p = f p
        |]
    , rejectUser
        "spec-coll-7-2"
        [r|
        module main (g)
        f t = .0 t
        g :: (Int, Str) -> Str
        g p = f p
        |]
    , accept
        "spec-coll-7-3"
        [r|
        module main (g)
        f t = .0 t
        g :: (Int, Str) -> Int
        g p = f p
        |]
    , rejectNaming
        "spec-coll-7-4"
        ["Infinite"]
        []
        [r|
        module main (f)
        f t = [.0 t, t]
        |]
    , rejectUser
        "spec-coll-7-5"
        [r|
        module main (f)
        f t = (.0 t, t 1)
        |]
    , rejectUser
        "spec-coll-7-6"
        [r|
        module main (g)
        h t = (.0 = "a") t
        g :: (Int, Int) -> (Int, Int)
        g p = h p
        |]
    , accept
        "spec-coll-7-7"
        [r|
        module main (g)
        h t = .1 ((.0 = 5) t)
        g :: (Int, Str) -> Str
        g p = h p
        |]
    , accept
        "spec-coll-7-8"
        [r|
        module main (f)
        type Pair a = (a, a)
        f :: Pair Int -> Int
        f p = .0 p
        |]
    , rejectUser
        "spec-coll-7-9"
        [r|
        module main (g)
        f t = (.0 t, .1 t)
        g :: (Int, Str) -> (Str, Int)
        g p = f p
        |]
    , accept
        "spec-coll-7-10"
        [r|
        module main (g)
        f t = [.0 t, .1 t, .2 t, .3 t, .4 t, .5 t, .6 t, .7 t, .8 t, .9 t, .10 t, .11 t, .12 t, .13 t, .14 t, .15 t]
        g :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> [Int]
        g p = f p
        |]
    , rejectNaming
        "spec-coll-7-11"
        ["tuple"]
        []
        [r|
        module main (g)
        f t = .0 t
        g :: [Int] -> Int
        g p = f p
        |]
    , rejectNaming
        "spec-coll-7-12"
        ["arity"]
        []
        [r|
        module main (g)
        f t = .3 t
        g :: Int
        g = f (1, 2, 3)
        |]
    , rejectUser
        "spec-coll-7-13"
        [r|
        module main (g)
        h t = (.0 = "a") t
        g :: (Int, Int) -> (Str, Int)
        g p = h p
        |]
    ]
