{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Newtypes
  ( newtTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

newtTests :: TestTree
newtTests =
  testGroup
    "NEWT"
    [ rejectSaying
        "spec-native-1-1"
        "out of the declaration's order"
        [r|
        module main (f)
        newtype P a b = (a, b)
        type Cpp => (P a b) = "std::tuple<$2,$1>" b a
        f :: P Int Real -> Real
        |]
    , rejectSaying
        "spec-native-1-2"
        "out of the declaration's order"
        [r|
        module main (f)
        newtype P a b = (a, b)
        type Cpp => (P a b) = "std::tuple<$1,$2>" b b
        f :: P Int Real -> Real
        |]
    , rejectSaying
        "spec-native-1-3"
        "but its list has"
        [r|
        module main (f)
        newtype P a b = (a, b)
        type Cpp => (P a b) = "std::tuple<$0,$2>" a b
        f :: P Int Real -> Real
        |]
    , reject
        "spec-native-1-9"
        [r|
        module main (f)
        record P a where
          x :: a
        record Py => P a = "PObj<$1>"
        f :: P Int -> P Int
        |]
    , accept
        "spec-native-1-10"
        [r|
        module main (f)
        record P a where
          x :: a
        record Py => P a = "PObj<$1>" a
        f :: P Int -> P Int
        |]
    , reject
        "spec-native-1-11"
        [r|
        module main (f)
        record P (n :: Nat) a where
          x :: a
        record Py => P (n :: Nat) a = "PObj<$2>" n a
        f :: P 3 Int -> P 3 Int
        |]
    , accept
        "spec-native-1-12"
        [r|
        module main (f)
        record P (n :: Nat) a where
          x :: a
        record Py => P (n :: Nat) a = "PObj<$1>" n a
        f :: P 3 Int -> P 3 Int
        |]
    , reject
        "spec-native-1-13"
        [r|
        module main (f)
        record P a where
          x :: a
        record Py => P a = "PObj<$0>" a
        f :: P Int -> P Int
        |]
    , accept
        "spec-native-1-14"
        [r|
        module main (f)
        record P a where
          x :: a
        record Py => P a = "PObj" a
        f :: P Int -> P Int
        |]
    , rejectSaying
        "spec-native-1-7"
        "never a Nat"
        [r|
        module main (f)
        newtype F (n :: Nat) = [Int]
        type Cpp => F (n :: Nat) = "std::array<int,$1>" n
        f :: F 3 -> Int
        |]
    , rejectSaying
        "spec-native-1-6"
        "out of the declaration's order"
        [r|
        module main (f)
        newtype P a b = (a, b)
        type Cpp => (P a b) = "std::tuple<$1,$2>" a b
        type Cpp => (P a Real) = "std::tuple<$1,$2>" Real a
        f :: P Int Real -> Real
        |]
    , rejectSaying
        "spec-native-1-4"
        "out of the declaration's order"
        [r|
        module main (f)
        newtype N (n :: Nat) a b = (a, b)
        type Cpp => (N (n :: Nat) a b) = "std::tuple<$2,$1>" b a
        f :: N 3 Int Real -> Real
        |]
    , rejectUser
        "spec-native-10-2"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Frozen a = [a]
        type Py => (Frozen a) = "tuple" a
        instance Packable [a] (Frozen a) where
          source Py from "frozen.py" ("freeze" as pack)
        f :: Frozen Int -> Int
        |]
    , rejectUser
        "spec-native-10-3"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Frozen a = [a]
        type Py => (Frozen a) = "tuple" a
        instance Packable [a] (Frozen a) where
          source Py from "frozen.py" ("thaw" as unpack)
        f :: Frozen Int -> Int
        |]
    , accept
        "spec-native-10-4"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Frozen a = [a]
        type Py => (Frozen a) = "tuple" a
        instance Packable [a] (Frozen a) where
          source Py from "frozen.py" ("freeze" as pack)
        instance Packable [a] (Frozen a) where
          source Py from "frozen.py" ("thaw" as unpack)
        f :: Frozen Int -> Int
        |]
    , accept
        "spec-newt-3-1"
        [r|
        module main (f)
        newtype Buffer (n :: Nat) a = List a
        f :: Buffer 3 Int -> Int
        |]
    , rejectUser
        "spec-newt-9-1"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Foo = Int
        instance Packable Str Foo
        f :: Foo -> Int
        |]
    , accept
        "spec-newt-9-2"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Foo = Int
        instance Packable Int Foo
        f :: Foo -> Int
        |]
    , rejectUser
        "spec-newt-9-3"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Box a
        instance Packable [a] (Box a)
        f :: Box Int -> Int
        |]
    , accept
        "spec-newt-9-4"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype MyMap a b = [(a, b)]
        instance Packable [(Str, v)] (MyMap Str v)
        f :: MyMap Str Int -> Int
        |]
    , accept
        "spec-newt-9-5"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype V (n :: Nat) a = List a
        newtype Mat (m :: Nat) (n :: Nat) a = ((Int, Int), V (m * n) a)
        instance Packable ((Int, Int), V (d1 * d2) a) (Mat d1 d2 a)
        f :: Mat 2 3 Int -> Int
        |]
    , accept
        "spec-newt-8-1"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Gff3 a = a deriving (Packable)
        f :: Gff3 Int -> Int
        |]
    , reject
        "spec-newt-8-2"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Buf (n :: Nat) a = List a deriving (Packable)
        f :: Buf 3 Int -> Int
        |]
    , reject
        "spec-newt-8-3"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype V (n :: Nat) a = List a
        newtype Mat (m :: Nat) (n :: Nat) a = ((Int, Int), V (m * n) a) deriving (Packable)
        f :: Mat 2 3 Int -> Int
        |]
    , accept
        "spec-newt-8-4"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Pt = (Int, Int) deriving Packable
        f :: Pt -> Int
        |]
    , reject
        "spec-newt-8-5"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        data Color = Red | Green deriving (Packable)
        f :: Color -> Int
        |]
    , accept
        "spec-newt-9-6"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype V (n :: Nat) a = List a
        newtype Mat (m :: Nat) (n :: Nat) a = ((Int, Int), V (m * n) a)
        instance Packable ((Int, Int), V (d2 * d1) a) (Mat d1 d2 a)
        f :: Mat 2 3 Int -> Int
        |]
    , rejectUser
        "spec-newt-9-7"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        data Color = Red | Green
        instance Packable Int Color
        f :: Color -> Int
        |]
    , accept
        "spec-newt-7-7"
        [r|
        module main (f)
        class Packable a where
          pack :: a -> Int
        newtype Name = Str
        f :: Name -> Int
        |]
    , reject
        "spec-newt-8-7"
        [r|
        module main (f)
        newtype Name = Str deriving (Packable)
        f :: Name -> Int
        |]
    , accept
        "spec-newt-7-9"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Cell = (Int, Int) deriving (Packable)
        f :: Cell -> (Int, Int)
        f c = unpack c
        |]
    ]
