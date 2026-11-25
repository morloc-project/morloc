{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Deriving
  ( derivTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Text.RawString.QQ
import Spec.Common

derivTests :: TestTree
derivTests =
  testGroup
    "DERIV"
    [ accept
        "spec-deriv-6-1"
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        record P where
          a :: Int
          b :: Str
        mkP :: Int -> P
        f :: Int -> (Bool, Bool, Bool, Bool, Bool, Bool)
        f n = (mkP n == mkP 1, mkP n != mkP 1, mkP n < mkP 1, mkP n <= mkP 1, mkP n > mkP 1, mkP n >= mkP 1)
        |]
    , rejectSaying
        "spec-deriv-6-13"
        "several instances match"
        [r|
        module main (lt)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        record Pair a b where
          p1 :: a
          p2 :: b
        instance Eq (Pair Int b) where
          (==) x y = True
          (!=) x y = False
        instance Eq (Pair a Int) where
          (==) x y = True
          (!=) x y = False
        lt :: Pair Int Int -> Pair Int Int -> Bool
        lt x y = x < y
        |]
    , rejectSaying
        "spec-cls-23-4"
        "may match once they are known"
        [r|
        module main (ltBox)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        record Box a where
          val :: a
        instance Eq (Box Int) where
          (==) x y = True
          (!=) x y = False
        ltBox :: Box a -> Box a -> Bool
        ltBox x y = x < y
        |]
    , accept
        "spec-cls-23-5"
        [r|
        module main (ltBox)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        record Box a where
          val :: a
        instance Eq (Box Int) where
          (==) x y = True
          (!=) x y = False
        ltBox :: Ord (Box a) => Box a -> Box a -> Bool
        ltBox x y = x < y
        |]
    , rejectNaming
        "spec-deriv-6-14"
        ["Eq"]
        []
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        type Id a = a
        f :: Id a -> Id a -> Bool
        f x y = x == y
        |]
    , rejectNaming
        "spec-deriv-6-15"
        ["Ord", "Eq"]
        []
        [r|
        module main (lt)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        record Pair a b where
          p1 :: a
          p2 :: b
        instance Eq (Pair Int b) where
          (==) x y = True
          (!=) x y = False
        instance Eq (Pair a Int) where
          (==) x y = True
          (!=) x y = False
        record Wrap a where
          w :: a
        instance Ord a => Ord (Wrap a) where
          (<) x y = True
          (<=) x y = True
          (>) x y = True
          (>=) x y = True
        lt :: Wrap (Pair Int Int) -> Wrap (Pair Int Int) -> Bool
        lt x y = x < y
        |]
    , rejectNaming
        "spec-deriv-6-2"
        ["Ord"]
        []
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        record P where
          a :: Int
        instance Eq P where
          (==) x y = True
          (!=) x y = False
        mkP :: Int -> P
        f :: Int -> Bool
        f n = mkP n < mkP 1
        |]
    , rejectNaming
        "spec-deriv-6-3"
        ["Ord"]
        []
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        record P where
          a :: Int
        instance Eq P where
          (==) x y = True
          (!=) x y = False
        type Q = P
        mkQ :: Int -> Q
        f :: Int -> Bool
        f n = mkQ n < mkQ 1
        |]
    , accept
        "spec-deriv-6-4"
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        record P where
          a :: Int
        instance Eq P where
          (==) x y = True
          (!=) x y = False
        mkP :: Int -> P
        f :: Int -> Bool
        f n = mkP n == mkP 1
        |]
    , accept
        "spec-deriv-6-5"
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        record Box a where
          v :: a
        instance Eq (Box Int) where
          (==) x y = True
          (!=) x y = False
        mkBox :: a -> Box a
        f :: Str -> (Bool, Bool)
        f s = (mkBox s == mkBox "a", mkBox s < mkBox "a")
        |]
    , accept
        "spec-deriv-6-6"
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        record Box a where
          v :: a
        mkBox :: a -> Box a
        eqBox :: Box a -> Box a -> Bool
        eqBox x y = x == y
        f :: Int -> Bool
        f n = eqBox (mkBox n) (mkBox 1)
        |]
    , accept
        "spec-deriv-6-8"
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        newtype Meters = Real
        mkM :: Real -> Meters
        f :: Real -> (Bool, Bool)
        f x = (mkM x == mkM 1.0, mkM x < mkM 1.0)
        |]
    , accept
        "spec-deriv-6-9"
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        newtype Z = Real deriving (Eq)
        mkZ :: Real -> Z
        f :: Real -> Bool
        f x = mkZ x < mkZ 1.0
        |]
    , rejectNaming
        "spec-deriv-6-10"
        ["Ord"]
        []
        [r|
        module main (f)
        class Ord a where
          (<=) :: a -> a -> Bool
        record P where
          a :: Int
        mkP :: Int -> P
        f :: Int -> Bool
        f n = mkP n <= mkP 1
        |]
    , rejectNaming
        "spec-deriv-6-7"
        ["Eq"]
        []
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        eqA :: a -> a -> Bool
        eqA x y = x == y
        f :: Int -> Bool
        f n = eqA n 1
        |]
    , rejectNaming
        "spec-deriv-6-11"
        ["Ord"]
        []
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        record Box a where
          v :: a
        instance Eq (Box a) where
          (==) x y = True
          (!=) x y = False
        instance Eq (Box Int) where
          (!=) x y = False
        mkBox :: a -> Box a
        f :: Int -> Bool
        f n = mkBox n < mkBox 1
        |]
    , accept
        "spec-deriv-1-1"
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        class Show a where
          show :: a -> Str
        class Hashable a where
          hash :: a -> Str
        newtype Name = Str deriving (Eq, Ord, Show, Hashable)
        data Shape = Circle Real | Dot deriving (Eq, Ord, Show)
        record Point deriving (Eq, Show) where
          x :: Int
          y :: Int
        f :: Name -> Shape -> Point -> (Bool, Bool, Str)
        f n s p = (n == n, s <= s, show p)
        |]
    , reject
        "spec-deriv-1-2"
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        class Show a where
          show :: a -> Str
        class Hashable a where
          hash :: a -> Str
        newtype Name = Str deriving (Functor)
        f :: Name -> Int
        |]
    , rejectUser
        "spec-deriv-1-3"
        [r|
        module main (f)
        newtype Name = Str deriving (Eq)
        f :: Name -> Int
        |]
    , rejectUser
        "spec-deriv-5-1"
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        class Show a where
          show :: a -> Str
        class Hashable a where
          hash :: a -> Str
        newtype Name = Str deriving (Eq)
        instance Eq Name
        f :: Name -> Int
        |]
    , accept
        "spec-deriv-5-2"
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
          (!=) :: a -> a -> Bool
        class Ord a where
          (<) :: a -> a -> Bool
          (<=) :: a -> a -> Bool
          (>) :: a -> a -> Bool
          (>=) :: a -> a -> Bool
        class Show a where
          show :: a -> Str
        class Hashable a where
          hash :: a -> Str
        newtype Name = Str deriving (Show)
        instance Eq Name
        f :: Name -> Int
        |]
    ]
