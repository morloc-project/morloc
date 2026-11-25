{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Typeclasses
  ( cls3Tests
  , cls16Tests
  , cls5Tests
  , cls17Tests
  , cls2OwnInstanceTests
  , cls18Tests
  , cls20Tests
  , cls5ChainTests
  , cls23Tests
  ) where

import Morloc.Frontend.Namespace
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Text.RawString.QQ
import Spec.Common

cls3Tests :: TestTree
cls3Tests =
  testGroup
    "CLS-3"
    [ rejectSaying
        "spec-cls-3-1"
        "define it in an instance of"
        [r|
        module main (bar)
        class Foo a where
          foo :: a -> Int
        instance Foo Int where
          foo x = 5
        foo x = 1
        bar :: Int -> Int
        bar x = foo x
        |]
    , accept
        "spec-cls-3-2"
        [r|
        module main (bar)
        class Foo a where
          foo :: a -> Int
        instance Foo Int where
          foo x = 5
        bar :: Int -> Int
        bar x = foo x
          where
            foo y = 1
        |]
    , rejectSaying
        "spec-cls-3-3"
        "has the name of a term this module imports"
        [r|
        module lib (foo)
          foo :: Int -> Int
        module main (bar)
          import .lib
          class Foo a where
            foo :: a -> Int
          instance Foo Int where
            foo x = 5
          bar :: Int -> Int
          bar x = foo x
        |]
    , rejectSaying
        "spec-cls-3-4"
        "has the name of a term this module imports"
        [r|
        module liba (foo)
          foo :: Int -> Int
        module libb (foo)
          foo :: Int -> Str
        module main (bar)
          import .liba
          import .libb
          class Foo a where
            foo :: a -> Int
          bar :: Int -> Int
          bar x = foo x
        |]
    , accept
        "spec-cls-3-5"
        [r|
        module lib (foo, one)
          foo :: Int -> Int
          one :: Int
        module main (bar)
          import .lib as l
          import .lib (one)
          class Foo a where
            foo :: a -> Int
          instance Foo Int where
            foo x = 5
          bar :: Int -> Int
          bar x = foo (l.foo x)
        |]
    , rejectSaying
        "spec-cls-3-6"
        "which is not a method of"
        [r|
        module main (bar)
        class Foo a where
          foo :: a -> Int
        instance Foo Int where
          foo x = 5
          goo x = 6
        bar :: Int -> Int
        bar x = foo x
        |]
    ]

cls16Tests :: TestTree
cls16Tests =
  testGroup
    "CLS-16"
    [ testCase "spec-cls-16-1" $
        ( mostSpecific [general, vec (NatLitU 3) int]
        , mostSpecific [vec (NatLitU 3) int, general]
        , mostSpecific [general, ForallU m (vec (KVarU (m, KindNat)) int)] )
          @?= ([vec (NatLitU 3) int], [vec (NatLitU 3) int], [ForallU m (vec (KVarU (m, KindNat)) int)])
    , testCase "spec-cls-16-2" $
        ( length (mostSpecific [ForallU a (vec n (vec n (VarU a))), vec (NatLitU 3) (vec (NatLitU 4) int)])
        , mostSpecific [ForallU a (vec n (vec n (VarU a))), vec (NatLitU 3) (vec (NatLitU 3) int)] )
          @?= (2, [vec (NatLitU 3) (vec (NatLitU 3) int)])
    , accept
        "spec-cls-16-5"
        [r|
        module main (f)
        class C a where
          c1 :: a -> Str
          c2 :: a -> Str
        instance C (a, Str) where
          c1 x = "b1"
          c2 x = "b2"
        f :: (Int, Str) -> Str
        f x = c2 x
        |]
    , accept
        "spec-cls-16-6"
        [r|
        module main (g)
        newtype Vec (n :: Nat) a = List a
        class Sz a where
          sz :: a -> Str
        instance Sz (Vec n a) where
          sz x = "vec"
        g :: Vec 3 a -> Str
        g x = sz x
        |]
    , reject
        "spec-cls-16-7"
        [r|
        module main (g)
        newtype Vec (n :: Nat) a = List a
        class Sz a where
          sz :: a -> Str
        instance Sz (Vec 3 Str) where
          sz x = "three"
        g :: Vec n Str -> Str
        g x = sz x
        |]
    , accept
        "spec-cls-16-9"
        [r|
        module main (g)
        newtype Vec (n :: Nat) a = List a
        class Sz a where
          sz :: a -> Str
        class Sz a => Big a where
          big :: a -> Str
        instance Sz (Vec n a) where
          sz x = "vec"
        instance Big (Vec 3 a) where
          big x = "big"
        g :: Vec 3 a -> Str
        g x = big x
        |]
    , rejectSaying
        "spec-cls-22-1"
        "inside an instance of"
        [r|
        module main (f)
        class Sz a where
          sz :: a -> Int
        instance Sz Int where
          sz :: Int -> Int
          sz x = x
        f :: Int -> Int
        f x = sz x
        |]
    , accept
        "spec-cls-5-10"
        [r|
        module main (g)
        newtype Vec (n :: Nat) a = List a
        class Pk a b where
          pk :: a -> b
          unpk :: b -> a
        instance Pk [a] (Vec n a) where
          source Py from "lib.py" ("pk", "unpk")
        g :: Vec n Str -> Vec n Str
        g t = pk (unpk t :: [Str])
        |]
    , reject
        "spec-cls-5-11"
        [r|
        module main (g)
        newtype Vec (n :: Nat) a = List a
        class Pk a b where
          pk :: a -> b
          unpk :: b -> a
        instance Pk [a] (Vec 3 a) where
          source Py from "lib.py" ("pk", "unpk")
        g :: Vec n Str -> Vec n Str
        g t = pk (unpk t :: [Str])
        |]
    , acceptTypecheck
        "spec-cls-5-13"
        [r|
        module main (one)
        class Integral a
        class Integral a => MyInt a
        one :: MyInt a => a
        one = 1
        |]
    , acceptTypecheck
        "spec-cls-5-14"
        [r|
        module main (f)
        class Sz a where
          sz :: a -> Int
        class C a where
          m :: a -> Int
        hd :: [a] -> a
        instance Sz Int where
          source Py from "s.py" ("szi" as sz)
        instance Sz a => C [a] where
          m xs = sz (hd xs)
        instance Sz b => C [b] where
          source Py from "s.py" ("m2" as m)
        f :: [Int] -> Int
        f xs = m xs
        |]
    , acceptTypecheck
        "spec-cls-5-15"
        [r|
        module main (f)
        class Sz a where
          sz :: a -> Int
        class C a where
          m :: a -> Int
        hd :: [a] -> a
        instance Sz Int where
          source Py from "s.py" ("szi" as sz)
        instance Sz b => C [b] where
          source Py from "s.py" ("m2" as m)
        instance Sz a => C [a] where
          m xs = sz (hd xs)
        f :: [Int] -> Int
        f xs = m xs
        |]
    , reject
        "spec-cls-16-8"
        [r|
        module main (f)
        class C a where
          c1 :: a -> Str
          c2 :: a -> Str
        instance C (Bool, b) where
          c1 x = "a1"
        instance C (a, Str) where
          c1 x = "b1"
          c2 x = "b2"
        f :: (Bool, Str) -> Str
        f x = c2 x
        |]
    , reject
        "spec-cls-16-10"
        [r|
        module main (f)
        class C a where
          c1 :: a -> Str
          c2 :: a -> Str
        instance C (Bool, b) where
          c1 x = "a1"
        instance C (a, Str) where
          c1 x = "b1"
          c2 x = "b2"
        f :: (Bool, Str) -> Str
        f x = c1 x
        |]
    , testCase "spec-cls-16-3" $
        ( isSubtypeOf (ForallU a (ForallU b (pair (VarU a) (VarU b)))) (ForallU c (pair (VarU c) int))
        , isSubtypeOf (ForallU c (pair (VarU c) int)) (ForallU a (ForallU b (pair (VarU a) (VarU b))))
        , isSubtypeOf (vec n int) (vec (VarU a) int) )
          @?= (True, False, False)
    ]
  where
    a = TV "a"
    b = TV "b"
    c = TV "c"
    m = TV "m"
    n = NatVarU (TV "n")
    int = VarU (TV "Int")
    vec x y = AppU (VarU (TV "Vec")) [x, y]
    pair x y = AppU (VarU (TV "T")) [x, y]
    general = ForallU a (vec n (VarU a))

cls5Tests :: TestTree
cls5Tests =
  testGroup
    "CLS-5"
    [ reject
        "spec-cls-5-16"
        [r|
        module main (g)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        f :: Packable a b => b -> Int
        f x = 1
        g :: Bool -> Int
        g x = f x
        |]
    , accept
        "spec-cls-5-17"
        [r|
        module main (g)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Box = Int
        instance Packable Int Box where
          source Py from "lib.py" ("pack", "unpack")
        f :: Packable a b => b -> Int
        f x = 1
        g :: Box -> Int
        g x = f x
        |]
    , accept
        "spec-cls-5-2"
        [r|
        module main (g)
        class Sz a where
          sz :: a -> Str
        record W a where
          val :: a
        instance Sz Str where
          sz x = "str"
        instance Sz a => Sz (W a) where
          sz w = "w"
        g :: Sz a => W a -> Str
        g w = sz w
        |]
    , accept
        "spec-cls-5-8"
        [r|
        module main (useIt)
        class Sz a where
          sz :: a -> Str
        record W a where
          val :: a
        instance Sz Str where
          sz x = "str"
        instance Sz a => Sz (W a) where
          sz w = "w"
        mkW :: a -> W a
        useIt :: Str -> Str
        useIt s = sz (mkW s)
        |]
    , accept
        "spec-cls-5-9"
        [r|
        module main (f)
        class Plus a where
          plus :: a -> a -> a
        instance Plus Str where
          plus x y = x
        ident :: Plus a => a -> a
        ident x = x
        f :: Str -> Str
        f x = ident x
        |]
    , reject
        "spec-cls-5-1"
        [r|
        module main (g)
        class Sz a where
          sz :: a -> Str
        record W a where
          val :: a
        instance Sz Str where
          sz x = "str"
        instance Sz a => Sz (W a) where
          sz w = "w"
        g :: W a -> Str
        g w = sz w
        |]
    , rejectFixture "spec-cls-5-3" False "Base" "cls-5-3/main.loc"
    , reject
        "spec-cls-5-6"
        [r|
        module main (useIt)
        class Sz a where
          sz :: a -> Str
        record W a where
          val :: a
        instance Sz Bool where
          sz x = "bool"
        instance Sz a => Sz (W a) where
          sz w = "w"
        mkW :: a -> W a
        useIt :: Str -> Str
        useIt s = sz (mkW s)
        |]
    , reject
        "spec-cls-5-7"
        [r|
        module main (f)
        class Plus a where
          plus :: a -> a -> a
        instance Plus Str where
          plus x y = x
        ident :: Plus a => a -> a
        ident x = x
        f :: Bool -> Bool
        f x = ident x
        |]
    , accept
        "spec-cls-5-4"
        [r|
        module main (f)
        class Marker a
        class Marker a => Sz a where
          sz :: a -> Str
        instance Marker Str
        instance Sz Str where
          sz x = "str"
        f :: Str -> Str
        f x = sz x
        |]
    , accept
        "spec-cls-5-5"
        [r|
        module main (f)
        class Base a where
          base :: a -> Str
        class Base a => Sz a where
          sz :: a -> Str
        instance Sz Str where
          sz x = "sz"
        instance Base Str where
          base x = "base"
        f :: Str -> Str
        f x = sz x
        |]
    , instanceTableTest
    ]

cls17Tests :: TestTree
cls17Tests =
  testGroup
    "CLS-17"
    [ accept
        "spec-cls-17-2"
        [r|
        module main (f)
        class Def a where
          def :: a
        instance Def Str where
          def = "d"
        f :: Bool -> Str
        f k = def
        |]
    , reject
        "spec-cls-17-1"
        [r|
        module main (f)
        class Def a where
          def :: a
        instance Def Str where
          def = "d"
        f :: Bool -> ?Str
        f k = def
        |]
    , reject
        "spec-cls-17-3"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Foo = Int
        instance Packable Int Foo where
          source Py from "lib.py" ("pack", "unpack")
        f :: Int -> ?Foo
        f x = pack x
        |]
    ]

-- | A use of a method inside its own instance, in a definition expanded in
-- place because `tally []` leaves an element type open.
cls2OwnInstanceTests :: TestTree
cls2OwnInstanceTests =
  testGroup
    "CLS-2 own instance"
    [ rejectSaying
        "spec-cls-2-12"
        "is not determined, so it cannot be compiled"
        [r|
        module main (f)
        class Tally f where
          tally :: f a -> Str
        newtype Box a = [a]
        mkB :: [a] -> Box a
        instance Tally List where
          tally xs = tally (mkB xs)
        instance Tally Box where
          tally b = "box"
        f :: Int -> Str
        f n = tally []
        |]
    , rejectSaying
        "spec-cls-2-14"
        "is not determined, so it cannot be compiled"
        [r|
        module main (f)
        class Tally f where
          tally :: f a -> Str
        isEmpty :: [a] -> Bool
        instance Tally List where
          tally xs = ? isEmpty xs = "empty" : tally []
        f :: [Int] -> Str
        f xs = tally xs
        |]
    , rejectSaying
        "spec-cls-2-13"
        "Polymorphic recursion"
        [r|
        module main (f)
        class Tally f where
          tally :: f a -> Str
        instance Tally List where
          tally xs = "t"
        class Render a where
          render :: a -> Str
        wrap :: [a] -> [[a]]
        instance Render [a] where
          render xs = render (wrap xs)
        pair :: Str -> Str -> Str
        f :: [Int] -> Str
        f n = pair (tally []) (render n)
        |]
    ]

cls18Tests :: TestTree
cls18Tests =
  testGroup
    "CLS-18"
    [ rejectSaying
        "spec-cls-18-2"
        "does not mention"
        [r|
        module main (count)
        class Named a where
          name :: a -> Str
        count :: Named a => Str -> Str
        count n = n
        |]
    , rejectSaying
        "spec-cls-18-7"
        "does not mention"
        [r|
        module main (size)
        class Conv f a where
          conv :: f -> a
        size :: Conv f a => f -> Int
        size x = 1
        |]
    , acceptTypecheck
        "spec-cls-18-8"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        f :: Packable a b => b -> Int
        f x = 1
        |]
    , rejectSaying
        "spec-cls-18-9"
        "does not mention"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        f :: Packable a b => a -> Int
        f x = 1
        |]
    , acceptTypecheck
        "spec-cls-18-10"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        class Sizeable a where
          size :: a -> Int
        f :: Sizeable (Vector n Int) => Int -> Int
        f x = x
        |]
    , rejectSaying
        "spec-cls-18-11"
        "does not mention"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        class Sizeable a where
          size :: a -> Int
        f :: Sizeable (Vector n a) => Int -> Int
        f x = x
        |]
    , acceptTypecheck
        "spec-cls-18-12"
        [r|
        module main (f)
        class Handles e where
          h :: Int -> Int
        f :: Handles e => <e> Int -> <e> Int
        f x = x
        |]
    , rejectSaying
        "spec-cls-18-13"
        "does not mention"
        [r|
        module main (g)
        class Named a where
          name :: a -> Str
        g :: Int -> Int
        g x = y where
          y :: Named b => Int
          y = x
        |]
    , rejectSaying
        "spec-cls-18-14"
        "does not mention"
        [r|
        module main (f)
        class Named a where
          name :: a -> Str
        unused :: Named q => Int -> Int
        f :: Str -> Str
        f x = x
        |]
    , reject
        "spec-cls-18-1"
        [r|
        module main (useIt)
        class Gen a where
          gen :: Str -> [a]
        instance Gen Str where
          gen n = [n]
        showList :: [a] -> Str
        render :: Gen a => Str -> Str
        render n = showList (gen n)
        useIt :: Str -> Str
        useIt n = render n
        |]
    , reject
        "spec-cls-18-3"
        [r|
        module main (f)
        class Def a where
          def :: a
        instance Def Str where
          def = "d"
        instance Def Bool where
          def = True
        konst :: a -> b -> a
        f :: Str -> Str
        f k = konst k def
        |]
    , accept
        "spec-cls-18-4"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        class Sizeable a where
          size :: a -> Int
        instance Sizeable (Vector n a) where
          size x = 1
        fromList :: [a] -> Vector n a
        f :: [Int] -> Int
        f xs = size (fromList xs)
        |]
    , reject
        "spec-cls-18-5"
        [r|
        module main (f)
        class Sizeable a where
          size :: a -> Int
        instance Sizeable [a] where
          size x = 1
        f :: Int -> Int
        f x = size []
        |]
    , reject
        "spec-cls-18-6"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Box = Int
        instance Packable Int Box where
          source Py from "lib.py" ("pack", "unpack")
        class Def a where
          def :: a
        instance Def Str where
          def = "d"
        instance Def Bool where
          def = True
        konst :: a -> b -> a
        konst x y = x
        asBox :: Box -> Box
        asBox x = x
        f b = konst (unpack (asBox b)) def
        |]
    ]

cls20Tests :: TestTree
cls20Tests =
  testGroup
    "CLS-20"
    [ accept
        "spec-cls-20-1"
        [r|
        module main (f)
        class Size a where
          size :: a -> Int
        instance Size Int where
          size x = 1
        instance Size Str where
          size x = 2
        newtype Box = Int
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        f :: Box -> Int
        f b = size (unpack b)
        |]
    , reject
        "spec-cls-20-2"
        [r|
        module main (f)
        class Size a where
          size :: a -> Int
        instance Size Int where
          size x = 1
        instance Size Str where
          size x = 2
        newtype Box = Int
        class Pk a b where
          pk :: a -> b
          unpk :: b -> a
        instance Pk Int Box where
          source Py from "lib.py" ("pk", "unpk")
        f :: Box -> Int
        f b = size (unpk b)
        |]
    , reject
        "spec-cls-20-3"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        instance Packable Int Bool where
          source Py from "lib.py" ("pack", "unpack")
        instance Packable Str Bool where
          source Py from "lib2.py" ("pack", "unpack")
        f :: Bool -> Bool
        f b = b
        |]
    , reject
        "spec-cls-20-4"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Foo = Int
        instance Packable Int Foo where
          source Py from "lib.py" ("pack", "unpack")
        class Packable Str b => Wire b where
          wire :: b -> Str
        instance Wire Foo where
          wire x = "w"
        f :: Foo -> Str
        f x = wire x
        |]
    , reject
        "spec-cls-20-5"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Foo = Int
        instance Packable Int Foo where
          source Py from "lib.py" ("pack", "unpack")
        class Sh a where
          sh :: a -> Str
        instance Packable Str a => Sh [a] where
          sh x = "l"
        f :: [Foo] -> Str
        f x = sh x
        |]
    , reject
        "spec-cls-20-6"
        [r|
        module main (f, g)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Bag a = List Int
        instance Packable [Int] (Bag a) where
          source Py from "lib.py" ("pack", "unpack")
        g :: Bag a -> [Str]
        g b = unpack b
        f :: Int -> Int
        f x = x
        |]
    , accept
        "spec-cls-20-7"
        [r|
        module main (f)
        class Packable a where
          pk :: a -> Str
        instance Packable Int where
          pk x = "int"
        instance Packable Str where
          pk x = "str"
        f :: Int -> Str
        f x = pk x
        |]
    , accept
        "spec-cls-20-8"
        [r|
        module main (g)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Vector (n :: Nat) a = List a
        instance Packable [a] (Vector n a) where
          source Py from "lib.py" ("pack", "unpack")
        len :: [a] -> Int
        g :: Vector n Int -> Int
        g v = len (unpack v)
        |]
    , accept
        "spec-cls-20-9"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Box = Int
        instance Packable Int Box where
          source Py from "lib.py" ("pack", "unpack")
        class Def a where
          def :: a
        instance Def Int where
          def = 1
        instance Def Str where
          def = "d"
        same :: a -> a -> a
        size2 :: a -> Int
        sel :: Int -> Int -> Int
        class Pick a where
          pick :: a -> Box -> Int
        instance Pick [a] where
          pick xs b = size2 (same def (unpack b))
        f :: Box -> Int
        f b = pick [1] b
        |]
    , accept
        "spec-cls-20-10"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Box = Int
        instance Packable Int Box where
          source Py from "lib.py" ("pack", "unpack")
        class Def a where
          def :: a
        instance Def Int where
          def = 1
        instance Def Str where
          def = "d"
        same :: a -> a -> a
        size2 :: a -> Int
        sel :: Int -> Int -> Int
        class Pick a where
          pick :: a -> Box -> Int
        instance Pick [a] where
          pick xs b = size2 (same (unpack b) def)
        f :: Box -> Int
        f b = pick [1] b
        |]
    , reject
        "spec-cls-20-11"
        [r|
        module main (f)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Box = Int
        instance Packable Int Box where
          source Py from "lib.py" ("pack", "unpack")
        class Def a where
          def :: a
        instance Def Int where
          def = 1
        instance Def Str where
          def = "d"
        same :: a -> a -> a
        size2 :: a -> Int
        sel :: Int -> Int -> Int
        class Pick a where
          pick :: a -> Box -> Int
        instance Pick [a] where
          pick xs b = sel (size2 def) (size2 (unpack b))
        f :: Box -> Int
        f b = pick [1] b
        |]
    ]

cls5ChainTests :: TestTree
cls5ChainTests =
  testGroup
    "CLS-5 chains, CLS-16, CLS-20"
    [ rejectNaming
        "spec-cls-5-18"
        ["Functor"]
        []
        [r|
        module main (h)
        record R where
          x :: Int
        class Functor f where
          map :: (a -> b) -> f a -> f b
        class Sliceable f where
          __get_slice__ :: ?I64 -> ?I64 -> ?I64 -> f a -> f a
        h :: Sliceable f => f R -> f Int
        h xs = .[:].x xs
        |]
    , accept
        "spec-cls-5-19"
        [r|
        module main (h)
        record R where
          x :: Int
        class Functor f where
          map :: (a -> b) -> f a -> f b
        class Sliceable f where
          __get_slice__ :: ?I64 -> ?I64 -> ?I64 -> f a -> f a
        h :: (Sliceable f, Functor f) => f R -> f Int
        h xs = .[:].x xs
        |]
    , accept
        "spec-cls-16-19"
        [r|
        module main (f)
        record R where
          x :: Int
        class Functor f where
          map :: (a -> b) -> f a -> f b
        class Sliceable f where
          __get_slice__ :: ?I64 -> ?I64 -> ?I64 -> f a -> f a
        newtype Bag a = List a
        instance Sliceable Bag
        f :: Bag R -> Bag Int
        f xs = .[:].x xs
        |]
    , accept
        "spec-cls-20-15"
        [r|
        module main (k)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        newtype Bag a = List a
        instance Packable [a] (Bag a) where
          source Py from "lib.py" ("pack", "unpack")
        g :: Packable a (Bag b) => Bag b -> Int
        k :: Bag Int -> Bag Str -> Int
        k x y = add (g x) (g y)
        add :: Int -> Int -> Int
        |]
    ]

cls23Tests :: TestTree
cls23Tests =
  testGroup
    "CLS-23"
    [ rejectSaying
        "spec-cls-23-1"
        "may match once they are known"
        [r|
        module main (f)
        class Sh a where
          sh :: a -> Str
        instance Sh a where
          sh x = "any"
        newtype Box = Int
        instance Sh Box where
          sh x = "box"
        f :: a -> Str
        f x = sh x
        |]
    , accept
        "spec-cls-23-2"
        [r|
        module main (f)
        class Sh a where
          sh :: a -> Str
        instance Sh a where
          sh x = "any"
        newtype Box = Int
        instance Sh Box where
          sh x = "box"
        f :: Sh a => a -> Str
        f x = sh x
        |]
    , accept
        "spec-cls-23-3"
        [r|
        module main (f)
        class Sh a where
          sh :: a -> Str
        instance Sh a where
          sh x = "any"
        newtype Box = Int
        instance Sh Box where
          sh x = "box"
        f :: Int -> Str
        f x = sh x
        |]
    , rejectNaming
        "spec-cls-23-6"
        ["Ord", "Eq"]
        []
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
        ltBox :: Eq (Box a) => Box a -> Box a -> Bool
        ltBox x y = x < y
        |]
    , rejectNaming
        "spec-cls-23-7"
        ["Eq", "P2"]
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
        record Pair a b where
          p1 :: a
          p2 :: b
        type P2 a = Pair a Int
        instance Eq (Pair Int b) where
          (==) x y = True
          (!=) x y = False
        f :: P2 a -> P2 a -> Bool
        f x y = x == y
        |]
    ]
