{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Annotations
  ( annTests
  , instanceAnnotationTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

annTests :: TestTree
annTests =
  testGroup
    "ANN"
    [ labelAccepts "spec-ann-8-1" logYaml "g :: Int -> Int\ng x = l@f x\n"
    , labelRejects "spec-ann-8-2" logYaml "g :: Int -> Int\ng x = l@k\n"
    , labelRejects "spec-ann-8-3" logYaml "g :: Int -> Int\ng x = l@(f x)\n"
    , labelAccepts "spec-ann-11-1" logYaml "h :: (Int -> Int) -> Int -> Int\nh q x = l@q x\ng :: Int -> Int\ng x = h f x\n"
    , labelRejects "spec-ann-11-2" cacheYaml "h :: (Int -> Int) -> Int -> Int\nh q x = l@q x\ng :: Int -> Int\ng x = h f x\n"
    , labelAccepts "spec-ann-10-2" logYaml "s :: <IO> Int\ns = io 1\ng :: Int -> <IO> Int\ng x = l@s\n"
    , labelAccepts "spec-ann-10-3" cacheYaml "s :: <IO> Int\ns = io 1\ng :: Int -> <IO> Int\ng x = l@s\n"
    , labelAccepts "spec-ann-11-3" cacheYaml "g :: Int -> <IO> Int\ng x = l@s where\n  s = io x\n"
    ]
  where
    logYaml = "labeled-groups:\n  l: { log: true }\n"
    cacheYaml = "labeled-groups:\n  l: { cache: true }\n"

-- An annotation inside an instance method's body is an ordinary annotation.
instanceAnnotationTests :: TestTree
instanceAnnotationTests =
  testGroup
    "CLS annotations"
    [ acceptTypecheck
        "spec-cls-2-5"
        [r|
        module main (f)
        class Size a where
          size :: a -> Int
        newtype Box = Int
        instance Size Int where
          size x = 1
        instance Size Box where
          size b = (size :: Int -> Int) (unpack b)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        f :: Box -> Int
        f b = size b
        |]
    , accept
        "spec-cls-2-7"
        [r|
        module main (combine)
        class Cat a where
          cat :: a -> a -> a
        record Foo = Bar { n :: Str }
        instance Cat Foo where
          cat a b = b
        record Scan where
          q :: Foo
        combine :: Foo -> Foo -> Scan
        combine x y = {q = cat x y}
        |]
    ]
