{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Bindings
  ( bindTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

bindTests :: TestTree
bindTests =
  testGroup
    "BIND"
    [ accept
        "spec-bind-2-1"
        [r|
        module main (f, g, h)
        add :: Int -> Int -> Int
        sub :: Int -> Int -> Int
        isZero :: Int -> Bool
        f :: Int -> Bool
        f x = ev x
          where
            ev n = ? isZero n = True : od (sub n 1)
            od n = ? isZero n = False : ev (sub n 1)
        g :: Int -> Int
        g x = d
          where
            d = x
        h :: Int -> Int
        h x = down x
          where
            down n = ? isZero n = 0 : add x (down (sub n 1))
        |]
    , rejectNaming
        "spec-bind-7-1"
        ["alpha", "beta"]
        []
        [r|
        module main (f)
        add :: Int -> Int -> Int
        alpha = add beta 1
        beta = add alpha 1
        f :: Int
        f = alpha
        |]
    , rejectNaming
        "spec-bind-7-2"
        ["alpha", "beta"]
        []
        [r|
        module main (f)
        add :: Int -> Int -> Int
        f :: Int -> Int
        f x = alpha
          where
            alpha = add beta 1
            beta = add alpha 1
        |]
    , rejectNaming
        "spec-bind-7-3"
        ["ys"]
        []
        [r|
        module main (f)
        map :: (a -> b) -> [a] -> [b]
        f :: [Int] -> [[Int]]
        f xs = ys
          where
            ys = map go xs
            go y = ys
        |]
    , rejectNaming
        "spec-bind-7-4"
        ["foo", "bar"]
        []
        [r|
        module main (bar)
        add :: Int -> Int -> Int
        class Foo a where
          foo :: a
        instance Foo Int where
          foo = add bar 1
        bar :: Int
        bar = foo
        |]
    , accept
        "spec-bind-7-5"
        [r|
        module main (loop)
        effect IO
        act :: <IO> ()
        loop :: <IO> ()
        loop = do
          act
          loop
        |]
    , rejectNaming
        "spec-bind-7-6"
        ["alpha", "beta"]
        []
        [r|
        module main (f)
        source Py from "lib.py" ("alpha")
        add :: Int -> Int -> Int
        alpha :: Int
        alpha = add beta 1
        beta :: Int
        beta = add alpha 1
        f :: Int
        f = alpha
        |]
    , accept
        "spec-bind-7-7"
        [r|
        module main (guardedRec)
        add :: Int -> Int -> Int
        sub :: Int -> Int -> Int
        isZero :: Int -> Bool
        guardedRec :: Int -> Int
        guardedRec n
          ? isZero n = 0
          : rest
          where
            rest = add n (guardedRec (sub n 1))
        |]
    , rejectNaming
        "spec-bind-7-8"
        ["vee", "aitch"]
        []
        [r|
        module main (aitch)
        add :: Int -> Int -> Int
        isZero :: Int -> Bool
        vee :: Int
        vee = add 1 (aitch 3)
        aitch :: Int -> Int
        aitch n = ? isZero n = 0 : vee
        |]
    , rejectNaming
        "spec-bind-7-9"
        ["loop", "w"]
        []
        [r|
        module main (loop)
        escapable effect E
        consume :: <E> () -> Int
        put :: Int -> <E> ()
        loop :: <E> ()
        loop = do
            put w
            loop
          where
            w = consume loop
        |]
    , rejectNaming
        "spec-bind-7-10"
        ["f"]
        []
        [r|
        module main (f)
        mk :: (Int -> Int) -> Int -> Int
        f :: Int -> Int
        f = mk f
        |]
    , accept
        "spec-bind-7-11"
        [r|
        module main (foo, wrap)
        add :: Int -> Int -> Int
        sub :: Int -> Int -> Int
        isZero :: Int -> Bool
        foo :: Int -> Int
        foo = bar
        bar :: Int -> Int
        bar n = ? isZero n = 0 : add 1 (foo (sub n 1))
        wrap :: Int -> Int
        wrap = step
          where
            step n = ? isZero n = 0 : wrap (sub n 1)
        |]
    , accept
        "spec-bind-7-12"
        [r|
        module main (countDown)
        sub :: Int -> Int -> Int
        isZero :: Int -> Bool
        tick :: Int -> Int
        countDown :: Int -> Int
        countDown = (\k n -> ? isZero n = k : countDown (sub n 1)) (tick 1)
        |]
    , rejectNaming
        "spec-bind-7-13"
        ["vee"]
        []
        [r|
        module main (vee)
        add :: Int -> Int -> Int
        f :: Int -> Int -> Int
        f x = \y -> add vee y
        vee :: Int
        vee = f 1 2
        |]
    , accept
        "spec-bind-7-14"
        [r|
        module main (step)
        sub :: Int -> Int -> Int
        isZero :: Int -> Bool
        step :: Int -> Int -> Int
        step k n = ? isZero n = k : h (sub n 1)
        h :: Int -> Int
        h = step 1
        |]
    , accept
        "spec-bind-7-15"
        [r|
        module main (h)
        source Py from "x.py" ("g")
        g :: Int -> Int -> Int
        g x y = h y
        h :: Int -> Int
        h = g 1
        |]
    , rejectAtLine
        "spec-bind-7-17"
        4
        [r|
        module main (f)
        add :: Int -> Int -> Int
        alpha = add beta 1
        beta = add alpha 1
        f :: Int
        f = alpha
        |]
    , rejectNaming
        "spec-bind-7-16"
        ["vee"]
        []
        [r|
        module main (step)
        add :: Int -> Int -> Int
        step :: Int -> Int -> Int
        step k n = add vee (add k n)
        vee :: Int
        vee = step 1 2
        |]
    ]
