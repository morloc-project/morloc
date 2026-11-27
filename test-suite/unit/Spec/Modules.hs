{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Modules
  ( modTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

modTests :: TestTree
modTests =
  testGroup
    "MOD"
    [ acceptFixture "spec-mod-1-1" "mod-1/siblings.loc"
    , rejectFixture "spec-mod-1-2" False "has the name of the local module" "mod-1/clash.loc"
    , rejectFixture "spec-mod-1-3" False "import .helper" "mod-1/bare.loc"
    , acceptFixture "spec-mod-1-4" "mod-1/headless.loc"
    , acceptFixture "spec-mod-1-6" "mod-1/unused.loc"
    , acceptFixture "spec-mod-5-1" "mod-5/shadow.loc"
    , rejectFixture "spec-mod-5-2" False "is reached as both" "mod-5/twice.loc"
    , acceptFixture "spec-mod-5-3" "mod-5/once.loc"
    , acceptFixture "spec-mod-5-4" "mod-5/both.loc"
    , rejectFixture "spec-mod-5-5" False "rename the program's module" "mod-5/selfname.loc"
    , acceptFixture "spec-mod-6-1" "mod-6/main.loc"
    , acceptFixture "spec-mod-7-1" "mod-7/system.loc"
    , acceptFixture "spec-mod-7-2" "mod-7/local.loc"
    , rejectFixture "spec-mod-7-3" False "import .mloc" "mod-7/missing.loc"
    , rejectFixture "spec-mod-7-4" True "import .mloc" "mod-7/missing.loc"
    , rejectFixture "spec-mod-7-5" False "without its library" "mod-7b/spelled.loc"
    , acceptFixture "spec-mod-7-6" "mod-7b/plain.loc"
    , rejectFixture "spec-mod-7-7" False "failed to import module" "mod-7b/fallback.loc"
    , acceptFixture "spec-mod-7-8" "mod-7b/library.loc"
    , accept
        "spec-mod-14-1"
        [r|
        module lib (mkP)
          type P = Int
          mkP :: Int -> P
        module main (g)
          import .lib (mkP)
          type P = Str
          g :: Int -> Int
          g x = mkP x
        |]
    , reject
        "spec-mod-14-2"
        [r|
        module lib (mkP)
          type P = Int
          mkP :: Int -> P
        module main (g)
          import .lib (mkP)
          type P = Str
          g :: Int -> Str
          g x = mkP x
        |]
    , reject
        "spec-mod-15-1"
        [r|
        module main (f)
          f :: Foo -> Foo
        |]
    , accept
        "spec-mod-15-2"
        [r|
        module main (f)
          type Foo = Int
          f :: Foo -> Foo
        |]
    , reject
        "spec-mod-16-1"
        [r|
        module lib (P, mkP)
          type P = Int
          mkP :: Int -> P
        module main (f)
          import .lib (P, mkP)
          type P = Str
          f :: Int -> P
          f x = mkP x
        |]
    , accept
        "spec-mod-16-2"
        [r|
        module lib (P, mkP)
          type P = Int
          mkP :: Int -> P
        module main (f)
          import .lib (mkP)
          type P = Str
          f :: Int -> Int
          f x = mkP x
        |]
    , reject
        "spec-rec-4-1"
        [r|
        module main (g)
          record Q where
            num :: Int
            den :: Int
          class Same a where
            same :: a -> a -> Bool
          instance Same Q where
            same a b = True
          g :: Int -> Bool
          g n = same {num = n, den = 2} {num = n, den = 4}
        |]
    , accept
        "spec-rec-4-2"
        [r|
        module main (g)
          record Q where
            num :: Int
            den :: Int
          class Same a where
            same :: a -> a -> Bool
          instance Same Q where
            same a b = True
          g :: Int -> Bool
          g n = same ({num = n, den = 2} :: Q) {num = n, den = 4}
        |]
    ]
