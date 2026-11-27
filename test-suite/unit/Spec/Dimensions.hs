{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Dimensions
  ( dimTests
  , dim7Tests
  ) where

import Morloc.Frontend.Namespace
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Text.RawString.QQ
import qualified Data.Map.Strict as DM
import qualified Data.Set as DS
import Morloc.Typecheck.NatDecide (Condition (..), Decision (..), decide)
import Morloc.Typecheck.NatSolver (NatExpr (..))
import Spec.Common

dimTests :: TestTree
dimTests =
  testGroup
    "DIM"
    [ accept
        "spec-dim-6-1"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
        h :: Foo n m -> Int
        f :: Foo 1 3 -> Int
        f x = h x
        |]
    , rejectUser
        "spec-dim-6-2"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
        h :: Foo n n -> Int
        f :: Foo 1 2 -> Int
        f x = h x
        |]
    , rejectUser
        "spec-dim-6-3"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
        h :: Foo (n + 5) m -> Int
        f :: Foo 1 3 -> Int
        f x = h x
        |]
    , accept
        "spec-dim-6-4"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
        h :: Foo (n + 3) m -> Int
        f :: Foo 1 3 -> Int
        f x = h x
        |]
    , rejectUser
        "spec-dim-6-5"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        h :: Vector (m + 1) Int -> Int
        f :: Vector 0 Int -> Int
        f x = h x
        |]
    , accept
        "spec-dim-6-6"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        h :: Vector (m + 1) Int -> Int
        f :: Vector 1 Int -> Int
        f x = h x
        |]
    , accept
        "spec-dim-9-1"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        newtype M (m :: Nat) (n :: Nat) a = ((m@Int, n@I64), Vector (m * n) a)
        h :: M m k Int -> M k n Int -> Int
        f :: M 2 3 Int -> M 3 1 Int -> Int
        f x y = h x y
        |]
    , reject
        "spec-dim-9-2"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        f :: (n@Int, Int) -> Vector n Int
        |]
    , reject
        "spec-dim-9-3"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        newtype M (m :: Nat) a = ((k@Int, Int), Vector m a)
        f :: M 2 Int -> Int
        |]
    , reject
        "spec-dim-9-4"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        newtype M (m :: Nat) a = ((m@Real, Int), Vector m a)
        f :: M 2 Int -> Int
        |]
    , reject
        "spec-dim-9-5"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        newtype M (m :: Nat) a = ([m@Int], Vector m a)
        f :: M 2 Int -> Int
        |]
    , reject
        "spec-dim-9-7"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        newtype M (m :: Nat) a = ((m@Int, m@Int), Vector m a)
        f :: M 2 Int -> Int
        |]
    , reject
        "spec-dim-9-8"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        newtype M (m :: Nat) a = ({k = m@Int}, Vector m a)
        f :: M 2 Int -> Int
        |]
    , rejectUser
        "spec-dim-10-1"
        [r|
        module main (h)
        newtype Vector (n :: Nat) a = List a
        roll :: w@Int -> Vector n Int -> Vector (n - w + 1) Int
        total :: Vector k Int -> Int
        h w src = total (roll w src)
        |]
    , accept
        "spec-dim-10-2"
        [r|
        module main (h)
        newtype Vector (n :: Nat) a = List a
        roll :: w@Int -> Vector n Int -> Vector (n - w + 1) Int
        total :: Vector k Int -> Int
        h :: w@Int -> Vector (m + w - 1) Int -> Int
        h w src = total (roll w src)
        |]
    , rejectUser
        "spec-dim-10-3"
        [r|
        module main (h)
        newtype Vector (n :: Nat) a = List a
        roll :: w@Int -> Vector n Int -> Vector (n - w + 1) Int
        h w src = roll w src
        |]
    , reject
        "spec-dim-10-4"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        mk :: Int -> Vector n Real
        dot :: Vector n Real -> Vector n Real -> Real
        f :: Int -> Real
        f k = dot (mk k) (mk k)
        |]
    , accept
        "spec-dim-10-5"
        [r|
        module main (f, g)
        newtype Vector (n :: Nat) a = List a
        keep :: Vector n Real -> Vector m Real
        len :: Vector k Real -> Int
        f :: Vector n Real -> Vector m Real
        f v = keep (keep v)
        g :: Vector n Real -> Int
        g v = len (keep v)
        |]
    , reject
        "spec-dim-10-6"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        keep :: Vector n Real -> Vector m Real
        f :: Vector n Real -> Vector n Real
        f v = keep v
        |]
    , rejectSaying
        "spec-dim-10-7"
        "POLY-7"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        g :: V 3 -> Int
        run :: a -> V n -> Int
        run k x = g x
        |]
    , accept
        "spec-dim-10-8"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        g :: V n -> Int
        run :: a -> V n -> Int
        run k x = g x
        |]
    , reject
        "spec-dim-10-9"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        g3 :: V 3 -> Int
        v3 :: V 3
        len :: V k -> Int
        run :: Int -> (V n -> Int, Int)
        run k = (g3, k)
        |]
    , accept
        "spec-dim-10-10"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        g3 :: V 3 -> Int
        v3 :: V 3
        len :: V k -> Int
        run :: Int -> (V n -> Int, Int)
        app :: (V m -> Int, Int) -> V m -> Int
        use :: Int
        use = app (run 1) v3
        |]
    , accept
        "spec-dim-10-11"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        g3 :: V 3 -> Int
        v3 :: V 3
        len :: V k -> Int
        apply :: (V n -> Int) -> Int
        apply f = f v3
        |]
    , reject
        "spec-dim-10-12"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        g3 :: V 3 -> Int
        v3 :: V 3
        len :: V k -> Int
        apply :: (V n -> Int) -> Int
        use :: Int
        use = apply g3
        |]
    , accept
        "spec-dim-10-13"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        g3 :: V 3 -> Int
        v3 :: V 3
        len :: V k -> Int
        apply :: (V n -> Int) -> Int
        use :: Int
        use = apply len
        |]
    , reject
        "spec-dim-10-14"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        g3 :: V 3 -> Int
        v3 :: V 3
        len :: V k -> Int
        mk :: Int -> (V n, V n -> Int)
        mk k = (v3, g3)
        |]
    , reject
        "spec-dim-10-15"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        g3 :: V 3 -> Int
        v3 :: V 3
        len :: V k -> Int
        run :: Int -> [V n -> Int]
        run k = [g3]
        |]
    , reject
        "spec-dim-10-16"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        g3 :: V 3 -> Int
        g5 :: V 5 -> Int
        v3 :: V 3
        len :: V k -> Int
        dot :: V n -> V n -> Int
        mk :: Int -> V n
        map :: (a -> b) -> [a] -> [b]
        apply :: (V n -> Int) -> Int
        apply f = f v3
        useA :: ((V 5 -> Int) -> Int) -> Int
        useA k = k g5
        use :: Int
        use = useA apply
        |]
    , reject
        "spec-dim-10-17"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        g3 :: V 3 -> Int
        g5 :: V 5 -> Int
        v3 :: V 3
        len :: V k -> Int
        dot :: V n -> V n -> Int
        mk :: Int -> V n
        map :: (a -> b) -> [a] -> [b]
        apply :: (V n -> Int) -> Int
        apply f = f v3
        xs :: [V n]
        xs = map mk [1, 2]
        |]
    , reject
        "spec-dim-10-18"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        g3 :: V 3 -> Int
        g5 :: V 5 -> Int
        v3 :: V 3
        len :: V k -> Int
        dot :: V n -> V n -> Int
        mk :: Int -> V n
        map :: (a -> b) -> [a] -> [b]
        apply :: (V n -> Int) -> Int
        apply f = f v3
        ys :: [V n]
        ys = map (\k -> mk k) [1, 2]
        |]
    , reject
        "spec-dim-10-19"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        g3 :: V 3 -> Int
        g5 :: V 5 -> Int
        v3 :: V 3
        len :: V k -> Int
        dot :: V n -> V n -> Int
        mk :: Int -> V n
        map :: (a -> b) -> [a] -> [b]
        apply :: (V n -> Int) -> Int
        apply f = f v3
        useB :: ((V n -> Int) -> Int) -> Int
        useB k = k (\v -> k (\u -> dot u v))
        use :: Int
        use = useB apply
        |]
    , accept
        "spec-dim-10-20"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        g3 :: V 3 -> Int
        g5 :: V 5 -> Int
        v3 :: V 3
        len :: V k -> Int
        dot :: V n -> V n -> Int
        mk :: Int -> V n
        map :: (a -> b) -> [a] -> [b]
        apply :: (V n -> Int) -> Int
        apply f = f v3
        use :: Int -> V n
        use k = mk k
        ok :: [Int]
        ok = map (\k -> len (mk k)) [1, 2]
        use2 :: Int -> Int -> V n
        use2 a b = mk a
        |]
    , accept
        "spec-dim-10-21"
        [r|
        module main (build, two)
        newtype V (n :: Nat) = Int
        mk :: Int -> Int -> V n
        len :: V k -> Int
        map :: (a -> b) -> [a] -> [b]
        build = mk
        two :: Int -> V n
        two = mk 2
        |]
    , reject
        "spec-dim-10-22"
        [r|
        module main (xs)
        newtype V (n :: Nat) = Int
        mk :: Int -> Int -> V n
        len :: V k -> Int
        map :: (a -> b) -> [a] -> [b]
        build = mk 1
        xs :: [V n]
        xs = map build [1, 2]
        |]
    , reject
        "spec-dim-10-23"
        [r|
        module main (xs)
        newtype V (n :: Nat) = Int
        mk :: Int -> Int -> V n
        len :: V k -> Int
        map :: (a -> b) -> [a] -> [b]
        sel k = mk 1 k
        xs :: [V n]
        xs = map sel [1, 2]
        |]
    , accept
        "spec-dim-10-24"
        [r|
        module main (ys)
        newtype V (n :: Nat) = Int
        mk :: Int -> Int -> V n
        len :: V k -> Int
        map :: (a -> b) -> [a] -> [b]
        sel :: Int -> V n
        sel k = mk 1 k
        ys :: [Int]
        ys = map (\k -> len (sel k)) [1, 2]
        |]
    , accept
        "spec-dim-10-25"
        [r|
        module main (ys)
        newtype V (n :: Nat) = Int
        mk :: Int -> Int -> V n
        len :: V k -> Int
        sel k = mk 1 k
        ys :: Int
        ys = len (sel 2)
        |]
    , reject
        "spec-dim-10-26"
        [r|
        module main (bar)
        newtype V (n :: Nat) = Int
        mk :: Int -> V n
        mk2 :: Int -> Int -> V n
        len :: V k -> Int
        dot :: V n -> V n -> Int
        map :: (a -> b) -> [a] -> [b]
        bar v = dot (mk 1) v
        |]
    , reject
        "spec-dim-10-27"
        [r|
        module main (bar)
        newtype V (n :: Nat) = Int
        mk :: Int -> V n
        mk2 :: Int -> Int -> V n
        len :: V k -> Int
        dot :: V n -> V n -> Int
        map :: (a -> b) -> [a] -> [b]
        bar v ks = map (\k -> dot (mk k) v) ks
        |]
    , accept
        "spec-dim-10-28"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        mk :: Int -> V n
        mk2 :: Int -> Int -> V n
        len :: V k -> Int
        dot :: V n -> V n -> Int
        map :: (a -> b) -> [a] -> [b]
        open :: Int -> Int -> V n
        open b = let y = 1 in mk2 b
        g = mk
        use :: Int
        use = len (g 3)
        class Op a where
          op :: Int -> a
        instance Op Int where
          op k = k
        |]
    , reject
        "spec-dim-10-29"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        mk :: Int -> Int -> V n
        dot :: V n -> V n -> Int
        map :: (a -> b) -> [a] -> [b]
        len :: V k -> Int
        v3 :: V 3
        both :: [V m] -> Int
        const :: a -> b -> a
        f :: Int -> Int -> V n
        f a b = const (mk a b) (both (map (f a) [1, 2]))
        |]
    , reject
        "spec-dim-10-30"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        mk :: Int -> Int -> V n
        dot :: V n -> V n -> Int
        map :: (a -> b) -> [a] -> [b]
        len :: V k -> Int
        v3 :: V 3
        const :: a -> b -> a
        f :: Int -> Int -> V n
        f a b = const (mk a b) (dot (f a b) (mk a b))
        |]
    , accept
        "spec-dim-10-31"
        [r|
        module main (*)
        newtype V (n :: Nat) = Int
        mk :: Int -> Int -> V n
        dot :: V n -> V n -> Int
        map :: (a -> b) -> [a] -> [b]
        len :: V k -> Int
        v3 :: V 3
        apply :: (V n -> Int) -> Int
        apply g = g v3
        wrap :: (V m -> Int) -> Int
        wrap g = apply g
        h :: (V n -> Int) -> Int -> Int
        h g k = g (mk k k)
        use :: Int
        use = wrap len
        |]
    , reject
        "spec-dim-10-32"
        [r|
        module main (f)
        newtype V (n :: Nat) = Int
        g0 :: Int -> V 0
        f :: Int -> (n@Int -> V n)
        f k = g0
        |]
    , reject
        "spec-dim-10-33"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        mk :: k@Int -> V k
        isZero :: Int -> Bool
        g :: k@Int -> V k
        g k = ? isZero k = mk k : g 0
        run :: Int -> V 3
        run x = g 3
        |]
    , accept
        "spec-dim-10-34"
        [r|
        module main (common)
        type Tbl (n :: Nat)
        sim :: Int -> Tbl n
        flags :: Tbl n -> Vector n Bool
        filt :: Vector n Bool -> Tbl n -> Tbl m
        common :: Int -> Tbl m
        common k = filt (flags t) t
          where
            t = sim k
        |]
    , accept
        "spec-dim-10-35"
        [r|
        module main (common)
        type Tbl (n :: Nat)
        sim :: Int -> Tbl n
        flags :: Tbl n -> Vector n Bool
        filt :: Vector n Bool -> Tbl n -> Tbl m
        common k = filt (flags t) t
          where
            t = sim k
        |]
    , reject
        "spec-dim-10-36"
        [r|
        module main (common)
        type Tbl (n :: Nat)
        sim :: Int -> Tbl n
        flags :: Tbl n -> Vector n Bool
        filt :: Vector n Bool -> Tbl n -> Tbl m
        common :: Int -> Tbl m
        common k = filt (flags (sim k)) (sim k)
        |]
    , reject
        "spec-dim-10-37"
        [r|
        module main (common)
        type Tbl (n :: Nat)
        sim :: Int -> Tbl n
        flags :: Tbl n -> Vector n Bool
        filt :: Vector n Bool -> Tbl n -> Tbl m
        common :: Int -> Tbl m
        common k = filt (flags (t k)) (t k)
          where
            t x = sim x
        |]
    , accept
        "spec-dim-10-38"
        [r|
        module main (run)
        type Tbl (n :: Nat)
        sim :: Int -> Tbl n
        flags :: Tbl n -> Vector n Bool
        filt :: Vector n Bool -> Tbl n -> Tbl m
        common k = filt (flags t) t
          where
            t = sim k
        size :: Tbl n -> Int
        run :: Int -> Int
        isPos :: Int -> Bool
        run k = ? isPos k = size (common k) : 0
        |]
    , reject
        "spec-dim-10-40"
        [r|
        module main (run)
        type Tbl (n :: Nat)
        sim :: Int -> Tbl n
        flags :: Tbl n -> Vector n Bool
        filt :: Vector n Bool -> Tbl n -> Int
        fst :: (a, b) -> a
        inc :: Int -> Int
        common :: a -> Int -> Int
        common x k = filt (flags (fst t)) (fst t)
          where
            t = (sim k, x)
        run :: Int -> Int
        run k = common inc k
        |]
    , reject
        "spec-dim-10-39"
        [r|
        module main (common)
        effect IO
        type Tbl (n :: Nat)
        sim :: Int -> Tbl n
        flags :: Tbl n -> Vector n Bool
        filt :: Vector n Bool -> Tbl n -> Tbl m
        type Job n = <IO> Tbl n
        mkJob :: Int -> Job n
        common :: Int -> <IO> Tbl m
        common k = do
          a <- t
          b <- t
          filt (flags a) b
          where
            t = mkJob k
        |]
    ]

dim7Tests :: TestTree
dim7Tests =
  testGroup
    "DIM-7"
    [ accept
        "spec-dim-7-4"
        [r|
        module main (f, g)
        newtype Vector (n :: Nat) a = List a
        h :: Vector m Int -> Int
        sq :: Vector n Int -> Vector (n * n - 2 * n + 1) Int
        pairs :: Vector n Int -> Vector (n * (n - 1) / 2) Int
        f :: Vector n Int -> Int
        f x = h (sq x)
        g :: Vector n Int -> Int
        g x = h (pairs x)
        |]
    , rejectNaming
        "spec-dim-7-5"
        ["solution"]
        ["decide"]
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        sq :: Vector (m * m) Int -> Int
        f :: Vector n Int -> Int
        f x = sq x
        |]
    , rejectNaming
        "spec-dim-7-6"
        ["solution"]
        ["decide"]
        [r|
        module main (x)
        newtype Vector (n :: Nat) a = List a
        sq :: Vector (m * m) Int -> Vector (m - 3) Int
        v :: Vector 4 Int
        x = sq v
        |]
    , testCase "spec-dim-7-1" $
        decide (DS.fromList [n]) [NatGe (NatSub (NatVar n) (NatLit 1)) (NatLit 0)]
          @?= Refuted (DM.fromList [(n, 0)])
    , testCase "spec-dim-7-2" $
        case decide (DS.fromList [m, n]) [NatEq (NatMul (NatVar m) (NatVar n)) (NatMul (NatVar n) (NatVar m))] of
          Holds _ -> return ()
          d -> assertFailure (show d)
    , testCase "spec-dim-7-3" $
        decide (DS.fromList [n]) [NatEq (NatVar n) (NatAdd (NatVar m) (NatLit 1))]
          @?= Refuted (DM.fromList [(n, 0)])
    ]
  where
    n = TV "n"
    m = TV "m"
