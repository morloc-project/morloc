{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Streams
  ( strmTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

strmTests :: TestTree
strmTests =
  testGroup
    "STRM"
    [ accept
        "spec-strm-4-1"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        q :: ([Int] -> <IO> ()) -> <IO> ()
        foo :: Bool -> <IO> (Collect Int)
        foo b = ? b = @collect p : @collect q
        |]
    , reject
        "spec-strm-4-2"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        q :: ([Int] -> <IO> ()) -> <IO> ()
        foo :: <IO> (Collect Int)
        foo = do
          _ <- @collect p
          @collect q
        |]
    , accept
        "spec-strm-4-3"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        q :: ([Int] -> <IO> ()) -> <IO> ()
        foo :: <IO> (Collect Int)
        foo = do
          @collect (\k -> do { _ <- p k; q k })
        |]
    , reject
        "spec-strm-4-4"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        foo :: <IO> (Collect Int)
        foo =
          let c = @collect p
          in c
        |]
    , accept
        "spec-strm-4-5"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        foo :: <IO> (Collect Int)
        foo =
          let c = p
          in @collect c
        |]
    , reject
        "spec-strm-4-6"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        nothing :: <IO> ()
        foo :: Bool -> <IO> (Collect Int)
        foo b = ? b = @collect p : nothing
        |]
    , accept
        "spec-strm-4-7"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        nothing :: <IO> ()
        foo :: Bool -> <IO> (Collect Int)
        foo b = ? b = @collect p : @collect (\k -> nothing)
        |]
    , accept
        "spec-strm-4-8"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        foo :: Bool -> <IO> (Collect Int)
        foo b = ? b = @collect p : @throw "no output"
        |]
    , accept
        "spec-strm-4-9"
        [r|
        module main (foo, bar)
        effect IO
        p :: Int -> ([Int] -> <IO> ()) -> <IO> ()
        foo :: Int -> Int -> <IO> (Collect Int)
        foo x = \y -> @collect (p y)
        bar :: Int -> Int -> <IO> (Collect Int)
        bar x y = (\z -> @collect (p z)) y
        |]
    , reject
        "spec-strm-4-10"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        twice :: <IO> a -> <IO> a
        foo :: <IO> (Collect Int)
        foo = twice (@collect p)
        |]
    , accept
        "spec-strm-4-11"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        twice :: (([Int] -> <IO> ()) -> <IO> ()) -> ([Int] -> <IO> ()) -> <IO> ()
        foo :: <IO> (Collect Int)
        foo = @collect (twice p)
        |]
    , reject
        "spec-strm-4-12"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        foo :: <IO> (Collect Int)
        foo = go
          where
          go = @collect p
        |]
    , accept
        "spec-strm-4-13"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        foo :: <IO> (Collect Int)
        foo = @collect go
          where
          go = p
        |]
    , reject
        "spec-strm-7-1"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        bar :: <IO> (Collect Int)
        bar = @collect p
        foo :: <IO> (Collect Int)
        foo = bar
        |]
    , accept
        "spec-strm-7-2"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        foo :: <IO> (Collect Int)
        foo = @collect p
        |]
    , reject
        "spec-strm-7-3"
        [r|
        module main (foo)
        effect IO
        source Py from "f.py" ("g")
        g :: <IO> (Collect Int)
        foo :: <IO> (Collect Int)
        foo = g
        |]
    , reject
        "spec-strm-7-4"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        foo = @collect p
        |]
    , reject
        "spec-strm-7-5"
        [r|
        module main (foo)
        effect IO
        nothing :: <IO> ()
        foo :: <IO> (Collect a)
        foo = @collect (\k -> nothing)
        |]
    , accept
        "spec-strm-7-6"
        [r|
        module main (foo)
        effect IO
        nothing :: <IO> ()
        foo :: <IO> (Collect Int)
        foo = @collect (\k -> nothing)
        |]
    , reject
        "spec-strm-7-7"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        h :: [Int] -> <IO> (Collect Int)
        --' Stream the numbers
        --' @with -x/--x=h
        foo :: <IO> (Collect Int)
        foo = @collect p
        |]
    , accept
        "spec-strm-7-8"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        h :: [Int] -> Int
        --' Stream the numbers
        --' @with -x/--x=h
        foo :: <IO> (Collect Int)
        foo = @collect p
        |]
    , reject
        "spec-strm-7-9"
        [r|
        module main (foo)
        effect IO
        class Out a where
          out :: a -> <IO> (Collect Int)
        p :: ([Int] -> <IO> ()) -> <IO> ()
        foo :: <IO> (Collect Int)
        foo = @collect p
        |]
    , reject
        "spec-strm-7-11"
        [r|
        module main (foo)
        effect IO
        type Out = Collect Int
        source Py from "f.py" ("g")
        g :: <IO> Out
        foo :: <IO> Out
        foo = g
        |]
    , reject
        "spec-strm-4-14"
        [r|
        module main (foo)
        effect IO
        class Run a where
          run :: a -> <IO> ()
        p :: ([Int] -> <IO> ()) -> <IO> ()
        instance Run Int where
          run x = do
            _ <- @collect p
            ()
        foo :: Int -> <IO> Int
        foo x = do
          _ <- run x
          x
        |]
    , accept
        "spec-strm-8-1"
        [r|
        module main (foo)
        effect IO
        foo :: <IO> (Collect Int)
        foo = @stdout (\o -> @write 0 o [1, 2])
        |]
    , reject
        "spec-strm-8-2"
        [r|
        module main (foo)
        effect IO
        foo :: <IO> (Collect Int)
        foo = @stdout (\o -> do
          _ <- @write 0 o [1, 2]
          @close o)
        |]
    , accept
        "spec-strm-8-3"
        [r|
        module main (foo)
        effect IO
        foo :: <IO> (Collect Int)
        foo = @stdout (\o -> do
          _ <- @write 0 o [1, 2]
          @write 0 o [3])
        |]
    , reject
        "spec-strm-8-4"
        [r|
        module main (foo)
        effect IO
        p :: ([Int] -> <IO> ()) -> <IO> ()
        foo :: <IO> (Collect Int)
        foo = do
          _ <- @stdout (\o -> @write 0 o [1, 2])
          @collect p
        |]
    , accept
        "spec-strm-8-5"
        [r|
        module main (foo)
        effect IO
        drain :: OStream Int -> <IO> ()
        foo :: <IO> (Collect Int)
        foo = @stdout drain
        |]
    , reject
        "spec-strm-8-6"
        [r|
        module main (foo)
        effect IO
        foo :: <IO> ()
        foo = do
          o <- @stdout :: <IO> OStream Int
          _ <- @write 0 o [1, 2]
          @close o
        |]
    ]
