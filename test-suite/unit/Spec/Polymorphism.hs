{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Polymorphism
  ( poly1Tests
  , poly8Tests
  , poly7Tests
  ) where

import Morloc (typecheck)
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM
import qualified Data.Text as MT
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Text.RawString.QQ
import Spec.Common

poly1Tests :: TestTree
poly1Tests =
  testGroup
    "POLY-1"
    [ rejectSaying
        "spec-poly-1-1"
        "which the body must accept at every value"
        [r|
        module main (q)
          type Table (n :: Nat) (r :: Rec)
          mk :: Int -> Table n {x = Int, y = Str}
          q :: Int -> Table m r
          q k = mk k
        |]
    , accept
        "spec-poly-1-2"
        [r|
        module main (q)
          type Table (n :: Nat) (r :: Rec)
          mk :: Int -> Table n {x = Int, y = Str}
          q :: Int -> Table m {x = Int, y = Str}
          q k = mk k
        |]
    , accept
        "spec-poly-1-3"
        [r|
        module main (q)
          type Table (n :: Nat) (r :: Rec)
          nrows :: Table n s -> Int
          q :: Table m r -> Int
          q t = nrows t
        |]
    , rejectSaying
        "spec-poly-1-4"
        "which the body must accept at every value"
        [r|
        module main (q)
          type Tag (s :: Str)
          mk :: Int -> Tag "a"
          q :: Int -> Tag s
          q k = mk k
        |]
    , accept
        "spec-poly-1-5"
        [r|
        module main (q)
          type Table (n :: Nat) (r :: Rec)
          idT :: Table n s -> Table n s
          q :: Table m r -> Table m r
          q t = idT t
        |]
    , rejectSaying
        "spec-poly-1-6"
        "which the body must accept at every value"
        [r|
        module main (q)
          type Table (n :: Nat) (r :: Rec)
          q :: Table m r -> Table m s
          q t = t
        |]
    , rejectSaying
        "spec-poly-1-7"
        "which the body must accept at every value"
        [r|
        module main (q)
          type Table (n :: Nat) (r :: Rec)
          both :: Table n (u + {b = Int}) -> Table n u -> Int
          mk :: Table n s -> Table n {a = Int}
          q :: Table m r -> Int
          q t = both t (mk t)
        |]
    , rejectSaying
        "spec-poly-1-9"
        "which the body must accept at every value"
        [r|
        module main (q)
          type Table (n :: Nat) (r :: Rec)
          two :: Table n (u + w) -> Table n (u + v) -> Table n w -> Table n v -> Int
          q :: Table m r -> Table m s -> Table m {} -> Int
          q a b e = two a b e e
        |]
    , rejectSaying
        "spec-poly-1-10"
        "cannot show that this equation holds"
        [r|
        module main (q)
          type Table (n :: Nat) (r :: Rec)
          getB :: Table n (u + {b = Int}) -> Int
          q :: Table m r -> Int
          q t = getB t
        |]
    , rejectSaying
        "spec-poly-1-8"
        "needs"
        [r|
        module main (q)
          type Tag (s :: Str)
          class Lbl a where
            lbl :: a -> Str
          instance Lbl (Tag "a") where
            lbl t = "a"
          q :: Tag s -> Str
          q t = lbl t
        |]
    ]

poly8Tests :: TestTree
poly8Tests =
  testGroup
    "POLY-8, POLY-5, NUM-5, NUM-10, EFF-5"
    [ accept
        "spec-poly-8-1"
        [r|
        module main (f)
          class Show a where
            show :: a -> Str
          instance Show Int where
            show x = "i"
          instance Show Str where
            show x = "s"
          map :: (a -> b) -> [a] -> [b]
          showAll = map show
          f :: Int -> ([Str], [Str])
          f x = (showAll [x], showAll ["a"])
        |]
    , accept
        "spec-num-5-1"
        [r|
        module main (f)
          class Integral a where
            add :: a -> a -> a
          instance Integral I64, Integral Real where
            add x y = x
          x = 5
          f :: I64 -> Real -> (I64, Real)
          f a b = (add a x, add b x)
        |]
    , accept
        "spec-num-5-2"
        [r|
        module main (f)
          class Integral a where
            add :: a -> a -> a
          instance Integral Int, Integral Real where
            add x y = x
          inc z = add z 100
          f :: Int -> Real -> (Int, Real)
          f a b = (inc a, inc b)
        |]
    , accept
        "spec-poly-8-2"
        [r|
        module main (f)
          type V (n :: Nat)
          tl :: V (n + 1) -> V n
          isEmpty :: V n -> Bool
          one :: Int
          ifelse :: Bool -> a -> a -> a
          count v = ifelse (isEmpty v) one (count (tl v))
          f :: V 3 -> Int
          f v = count v
        |]
    , reject
        "spec-poly-8-3"
        [r|
        module main (use)
          newtype V (n :: Nat) = Int
          mk :: Int -> V n
          dot :: V n -> V n -> Int
          bar v = dot (mk 1) v
          use :: V n -> Int
          use v = bar v
        |]
    , accept
        "spec-poly-5-2"
        [r|
        module main (use)
          newtype V (n :: Nat) = Int
          mk :: Int -> V n
          len :: V n -> Int
          plus :: Int -> Int -> Int
          bar k = (g 1, g 2)
            where
              g j = mk (plus j k)
          use :: Int -> Int
          use k = len (.0 (bar k))
        |]
    , accept
        "spec-num-5-3"
        [r|
        module main (s)
          class Show a where
            show :: a -> Str
          instance Show Int where
            show x = "i"
          showIt = show 5
          s :: Str
          s = showIt
        |]
    , accept
        "spec-num-5-4"
        [r|
        module main (f)
          class Integral a where
            add :: a -> a -> a
          instance Integral Int where
            add x y = x
          add5 :: Integral a => a -> a
          add5 = add 5
          f :: Int -> Int
          f x = add5 x
        |]
    , reject
        "spec-num-5-5"
        [r|
        module main (f)
          class Integral a where
            add :: a -> a -> a
          instance Integral Int where
            add x y = x
          pair :: a -> b -> (a, b)
          bad :: b -> (a, b)
          bad = pair 5
          f :: Int -> (Int, Int)
          f x = bad x
        |]
    , accept
        "spec-num-10-2"
        [r|
        module main (y)
          class Integral a where
            zero :: a
          instance Integral Int where
            zero = 0
          ignore :: a -> Int
          y :: Int
          y = ignore zero
        |]
    , reject
        "spec-num-10-3"
        [r|
        module main (y)
          class Integral a where
            zero :: a
          class Show a where
            show :: a -> Str
          instance Integral Int where
            zero = 0
          instance Show Int where
            show v = "0"
          y :: Str
          y = show zero
        |]
    , accept
        "spec-num-10-4"
        [r|
        module main (y)
          class Integral a where
            add :: a -> a -> a
          instance Integral Int where
            add x y = x
          f :: ?Int -> Int -> Bool
          y :: Bool
          y = f e e
            where
              e = add 3 4
        |]
    , testCase "spec-num-10-5" $ do
        -- an unsigned export defaults its numeric variables: a command on Real
        config <- emptyConfig
        ((x, msgs), _) <- MM.runMorlocMonad Nothing 0 config defaultValue . typecheck Nothing . Code $
          [r|
          module main (foo)
            class Integral a where
              add :: a -> a -> a
            class Integral a => Numeric a where
              half :: a -> a
            instance Integral Real where
              add x y = x
            instance Numeric Real where
              half x = x
            y = 40.0
            foo x = add x y
          |]
        case x of
          Left e -> assertFailure ("expected acceptance, got: " <> show e)
          Right _ -> assertBool ("foo skipped: " <> show msgs) (not (any (MT.isInfixOf "skipping generic export 'foo'") msgs))
    , reject
        "spec-poly-8-6"
        [r|
        module main (g)
          ifelse :: Bool -> a -> a -> a
          c :: Bool
          f x = ifelse c x (f "s")
          g :: Int -> Int
          g y = f y
        |]
    , reject
        "spec-poly-8-7"
        [r|
        module main (g)
          class Integral a where
            add :: a -> a -> a
          instance Integral Int where
            add x y = x
          opt :: a -> ?a
          h :: b -> b -> b
          f x = h 5 (opt x)
          g :: Str -> ?Str
          g s = f s
        |]
    , reject
        "spec-eff-5-1"
        [r|
        module main (g)
          effect IO
          noisy :: Int -> <IO> Int
          add :: Int -> Int -> Int
          ifelse :: Bool -> a -> a -> a
          c :: Bool
          go m = ifelse c (noisy m) (noisy (add 1 (go m)))
          g :: Int -> <IO> Int
          g x = go x
        |]
    , accept
        "spec-eff-5-2"
        [r|
        module main (g)
          effect IO
          noisy :: Int -> <IO> Int
          dec :: Int -> Int
          ifelse :: Bool -> a -> a -> a
          c :: Bool
          g :: Int -> <IO> Int
          g n = go n
            where
              go m = ifelse c (noisy m) (step m)
              step :: Int -> <IO> Int
              step m = do
                noisy m
                go (dec m)
        |]
    , accept
        "spec-poly-5-1"
        [r|
        module main (f)
          pairs n s = (dup n, dup s)
            where
              dup y = [y, y]
          f :: Int -> Str -> ([Int], [Str])
          f n s = pairs n s
        |]
    , accept
        "spec-poly-8-4"
        [r|
        module main (f)
          class Integral a where
            add :: a -> a -> a
          instance Integral Int where
            add x y = x
          twice g x = g (g x)
          incTwice = twice (add 1)
          f :: Int -> Int
          f x = incTwice x
        |]
    , accept
        "spec-poly-8-5"
        [r|
        module main (f)
          class Integral a where
            add :: a -> a -> a
          instance Integral Int, Integral Real where
            add x y = x
          ifelse :: Bool -> a -> a -> a
          evens n = ifelse n [] (odds n)
          odds n = ifelse n [1] (evens n)
          f :: Bool -> ([Int], [Real])
          f b = (evens b, odds b)
        |]
    , accept
        "spec-num-5-6"
        [r|
        module main (h)
          class Integral a where
            add :: a -> a -> a
          class Integral a => Numeric a where
            half :: a -> a
          instance Integral Real where
            add x y = x
          instance Numeric Real where
            half x = x
          f :: Numeric a => a -> a
          f y = add y z
            where
              z = 1.0
          h :: Real
          h = f 2.0
        |]
    ]

poly7Tests :: TestTree
poly7Tests =
  testGroup
    "POLY-7"
    [ reject
        "spec-poly-7-1"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        h :: Vector (m + 1) Int -> Int
        f :: Vector n Int -> Int
        f x = h x
        |]
    , accept
        "spec-poly-7-2"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        h :: Vector (m + 1) Int -> Int
        f :: Vector (n + 1) Int -> Int
        f x = h x
        |]
    , reject
        "spec-poly-7-3"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        h :: Vector (m + 1) Int -> Int
        f :: Vector (n * n) Int -> Int
        f x = h x
        |]
    , accept
        "spec-poly-7-4"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        g :: Vector (m + 1) Int -> Vector m Int
        f :: Vector n Int -> Vector (n - 1) Int
        f x = g x
        |]
    , reject
        "spec-poly-7-5"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        take3 :: Vector n Int -> Vector (n - 3) Int
        len :: Vector k Int -> Int
        f :: Vector n Int -> Int
        f x = len (take3 x)
        |]
    , accept
        "spec-poly-7-6"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        roll :: w@Int -> Vector n Real -> Vector (n - w + 1) Real
        f :: w@Int -> Vector n Real -> Vector (n - w + 1) Real
        f w v = roll w v
        |]
    , accept
        "spec-poly-7-8"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        half :: Vector n Int -> Vector (n / 2) Int
        len :: Vector k Int -> Int
        f :: Vector n Int -> Vector (n / 2) Int
        f x = half x
        g :: Vector n Int -> Int
        g x = len (half x)
        append :: Vector a Int -> Vector b Int -> Vector (a + b) Int
        h :: Vector n Int -> Int
        h x = len (append (half x) x)
        |]
    , reject
        "spec-kind-4-9"
        [r|
        module main (x)
        newtype T1 (d :: Nat) a = List a
        make :: n@Int -> d@Int -> T1 (n / d) Real
        x = make 12 0
        |]
    , reject
        "spec-kind-4-11"
        [r|
        module main (x)
        newtype T1 (d :: Nat) a = List a
        make :: n@Int -> d@Int -> T1 (n / d) Real
        x k = make k 0
        |]
    , reject
        "spec-kind-4-14"
        [r|
        module main (x)
        newtype Vector (n :: Nat) a = List a
        sq :: Vector (m * m) Int -> Vector (m - 3) Int
        v :: Vector 4 Int
        x = sq v
        |]
    , accept
        "spec-kind-4-15"
        [r|
        module main (x)
        newtype Vector (n :: Nat) a = List a
        sq :: Vector (m * m) Int -> Vector (m - 1) Int
        v :: Vector 4 Int
        x = sq v
        |]
    , reject
        "spec-kind-4-16"
        [r|
        module main (g)
        newtype Vector (n :: Nat) a = List a
        newtype Tbl (r :: Rec) = Int
        cols1 :: Tbl r -> Vector (Size r - 1) Int
        len :: Vector k Int -> Int
        g :: Tbl r -> Int
        g t = len (cols1 t)
        |]
    , accept
        "spec-kind-4-17"
        [r|
        module main (g, h)
        newtype Vector (n :: Nat) a = List a
        newtype Tbl (r :: Rec) = Int
        cols1 :: Tbl r -> Vector (Size r - 1) Int
        grow :: Tbl r -> Vector (Size r + 1) Int
        len :: Vector k Int -> Int
        mk :: Tbl {x = Int, y = Str}
        h = len (cols1 mk)
        g :: Tbl r -> Int
        g t = len (grow t)
        |]
    , accept
        "spec-kind-4-18"
        [r|
        module main (prim, cols1, h)
        newtype Vector (n :: Nat) a = List a
        newtype Tbl (r :: Rec) = Int
        prim :: Tbl r -> Vector (Size r - 1) Int
        len :: Vector k Int -> Int
        cols1 :: Tbl r -> Vector (Size r - 1) Int
        cols1 t = prim t
        h t = len (prim t)
        |]
    , accept
        "spec-kind-4-19"
        [r|
        module main (cols1)
        newtype Vector (n :: Nat) a = List a
        newtype Tbl (r :: Rec) = Int
        allCols :: Tbl r -> Vector (Size r) Int
        dropOne :: Vector (n + 1) Int -> Vector n Int
        cols1 :: Tbl r -> Vector (Size r - 1) Int
        cols1 t = dropOne (allCols t)
        |]
    , reject
        "spec-kind-4-10"
        [r|
        module main (x)
        newtype T1 (d :: Nat) a = List a
        offset :: k@Int -> T1 n Real -> T1 (n + k) Real
        src :: T1 10 Real
        x = offset (-11) src
        |]
    , accept
        "spec-kind-4-12"
        [r|
        module main (x)
        newtype T1 (d :: Nat) a = List a
        offset :: k@Int -> T1 n Real -> T1 (n + k) Real
        src :: T1 10 Real
        x = offset (-4) src
        |]
    , reject
        "spec-poly-7-9"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        mk :: n@Int -> Vector n Real
        len :: Vector k Real -> Int
        f :: k@Int -> Int
        f k = len (mk k)
        |]
    , accept
        "spec-poly-7-10"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        mk :: n@Int -> Vector n Real
        len :: Vector k Real -> Int
        f :: k@Int -> Vector k Real -> Int
        f k v = len (mk k)
        |]
    , reject
        "spec-poly-7-11"
        [r|
        module main (foo)
        newtype T1 (d :: Nat) a = List a
        bar :: T1 (n + 10) Real -> Int
        foo :: h@Int -> T1 (h + 10) Real -> Int
        foo h x = bar x
        |]
    , accept
        "spec-poly-7-12"
        [r|
        module main (f, h)
        newtype Vector (n :: Nat) a = List a
        square :: n@Int -> Vector (n * n) Real
        len :: Vector k Real -> Int
        f :: n@Int -> Int
        f n = len (square n)
        outer :: Vector n Real -> Vector m Real -> Vector (n * m) Real
        h :: k@Int -> Vector k Real -> Vector (k * k) Real
        h k v = outer v v
        |]
    , accept
        "spec-kind-1-1"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        class C a where
          c :: a -> Str
        instance C (Vector n a) where
          c x = "v"
        f :: k@Int -> Vector k Real -> Str
        f k v = c v
        |]
    , accept
        "spec-kind-1-2"
        [r|
        module main (y)
        newtype Vector (n :: Nat) a = List a
        class Mk a where
          mk :: n@Int -> Vector (n + 5) a
        instance Mk Real where
          mk n = mk n
        y = (mk (-3) :: Vector 2 Real)
        |]
    , rejectAtLine
        "spec-poly-7-13"
        9
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        h :: Vector (m + 1) Int -> Int
        f :: Vector n Int -> Int
        f x = g x
          where
            g :: Vector k Int -> Int
            g y = h y
        |]
    , rejectAtLine
        "spec-poly-7-14"
        6
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        h :: Vector (m + 1) Int -> Int
        f :: Vector n Int -> Int
        f x = g x
          where
            g :: Vector (k + 1) Int -> Int
            g y = h y
        |]
    , accept
        "spec-poly-7-15"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        len :: Vector k Int -> Int
        add :: Int -> Int -> Int
        f :: Vector n Int -> Vector 3 Int -> Int
        f x z = add (g x) (g z)
          where
            g :: Vector k Int -> Int
            g y = len y
        |]
    , rejectAtLine
        "spec-poly-7-16"
        7
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        take3 :: Vector n Int -> Vector (n - 3) Int
        len :: Vector k Int -> Int
        f :: Vector n Int -> Int
        f x = len (take3 x)
        |]
    , accept
        "spec-poly-4-1"
        [r|
        module main (f)
        class Dbl a where
          dbl :: a -> a
        instance Dbl Int where
          dbl x = x
        f :: Int -> Int
        f x = g x
          where
            g :: Dbl a => a -> a
            g y = dbl y
        |]
    , accept
        "spec-poly-4-2"
        [r|
        module main (f, h)
        f :: Int -> Int
        f x = g x
          where
            g :: a -> a
            g y = y
        h :: a -> Int
        h x = k x
          where
            k :: a -> Int
            k y = 1
        |]
    , reject
        "spec-poly-4-4"
        [r|
        module main (f)
        apply :: (Int -> <e> Int) -> Int -> <e> Int
        f :: (Int -> <e> Int) -> Int -> <e> Int
        f k x = g x
          where
            g :: Int -> <e> Int
            g y = apply k y
        |]
    , rejectAtLine
        "spec-poly-4-3"
        9
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        dot :: Vector m Int -> Vector m Int -> Int
        f :: Vector n Int -> Vector n Int -> Int
        f x z = g z
          where
            g :: Vector n Int -> Int
            g y = dot x y
        |]
    , reject
        "spec-poly-7-18"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        f :: k@Int -> Vector k Int -> Vector 3 Int
        f k v = v
        |]
    , rejectAtLine
        "spec-poly-7-19"
        8
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        len :: Vector m Int -> Int
        f :: Vector n Int -> Int
        f x = len x
          where
            g :: Vector (k - (k + 1)) Int -> Int
            g y = len y
        |]
    , accept
        "spec-poly-7-20"
        [r|
        module main (f, g)
        newtype Vector (n :: Nat) a = List a
        mk :: n@U64 -> Vector n Real
        len :: Vector k Real -> Int
        f :: k@U64 -> Int
        f k = len (mk k)
        mk8 :: n@U8 -> Vector n Real
        g :: k@U8 -> Int
        g k = len (mk8 k)
        |]
    , reject
        "spec-poly-7-7"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        roll :: w@Int -> Vector n Real -> Vector (n - w + 1) Real
        len :: Vector k Real -> Int
        f :: w@Int -> Vector n Real -> Int
        f w v = len (roll w v)
        |]
    ]
