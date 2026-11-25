{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Intrinsics
  ( intrTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

intrTests :: TestTree
intrTests =
  testGroup
    "INTR"
    [ rejectSaying
        "spec-intr-9-1"
        "non-functional"
        [r|
        module main (f)
        f :: Int -> Try Str Int
        f x = @try x 1
        |]
    , accept
        "spec-intr-9-5"
        [r|
        module main (f)
        f :: Int -> Try Str Int
        f x = @try x
        |]
    , rejectSaying
        "spec-intr-9-2"
        "non-functional"
        [r|
        module main (f)
        effect IO
        f :: OStream Int -> [Int] -> <IO> (Try Str ())
        f o xs = @write 3 o xs 7
        |]
    , accept
        "spec-intr-9-6"
        [r|
        module main (f)
        effect IO
        f :: OStream Int -> [Int] -> <IO> (Try Str ())
        f o xs = @write 3 o xs
        |]
    , acceptTypecheck
        "spec-intr-9-3"
        [r|
        module main (f)
        f :: Int -> Int
        f x = @throw "boom" x
        |]
    , rejectSaying
        "spec-intr-9-4"
        "non-functional"
        [r|
        module main (f)
        f :: Int -> Str
        f x = @version x
        |]
    ]
