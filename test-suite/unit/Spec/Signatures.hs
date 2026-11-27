{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Signatures
  ( sigTests
  , checkOnceTests
  ) where

import Morloc (typecheck)
import Morloc.Frontend.Namespace
import Morloc.Data.Doc (render)
import qualified Morloc.Monad as MM
import qualified Data.Text as MT
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Text.RawString.QQ
import qualified Data.Map.Strict as DM
import Spec.Common

sigTests :: TestTree
sigTests =
  testGroup
    "SIG"
    [ rejectSaying
        "spec-sig-1-1"
        "Type mismatch"
        [r|
        module lib (bad)
          bad :: a -> a
          bad x = 5

        module main (useIt)
          import .lib (bad)
          useIt :: Int -> Int
          useIt x = bad x
        |]
    , accept
        "spec-sig-1-2"
        [r|
        module lib (good)
          good :: a -> a
          good x = x

        module main (useIt)
          import .lib (good)
          useIt :: Int -> Int
          useIt x = good x
        |]
    , rejectSaying
        "spec-sig-1-3"
        "Type mismatch"
        [r|
        module sigs (bad)
          bad :: a -> a

        module impl (bad)
          import .sigs (bad)
          bad x = 5

        module main (useIt)
          import .impl (bad)
          useIt :: Int -> Int
          useIt x = bad x
        |]
    , rejectSaying
        "spec-sig-1-5"
        "Type mismatch"
        [r|
        module lib (good)
          good :: a -> a
          good x = x
          bad :: a -> a
          bad x = 5

        module main (useIt)
          import .lib (good)
          useIt :: Int -> Int
          useIt x = good x
        |]
    , rejectSaying
        "spec-sig-1-4"
        "Type mismatch"
        [r|
        module lib (Shift)
          class Shift a where
            shift :: a -> a
          instance Shift Int where
            shift x = x
          instance Shift Str where
            shift x = 5

        module main (useIt)
          import .lib (Shift)
          useIt :: Int -> Int
          useIt x = x
        |]
    , rejectUser
        "spec-sig-6-1"
        [r|
        module main (bar)
          foo :: Int -> Int
          foo :: Int -> Int
          bar :: Int -> Int
          bar x = foo x
        |]
    , accept
        "spec-sig-6-3"
        [r|
        module main (bar)
          foo :: Int -> Int
          bar :: Int -> Int
          bar x = foo x
        |]
    , reject
        "spec-sig-6-4"
        [r|
        module main (f)
          f :: Int -> Int
          f x = go x
            where
            go :: Int -> Int
            go :: Int -> Int
            go y = y
        |]
    , rejectUser
        "spec-sig-6-5"
        [r|
        module main (bar)
          class C a where
            foo :: a -> a
          foo :: Int -> Int
          bar :: Int -> Int
          bar x = foo x
        |]
    , accept
        "spec-sig-6-2"
        [r|
        module main (f, g)
          f :: Int -> Int
          f x = go x
            where
            go :: Int -> Int
            go y = y
          g :: Int -> Int
          g x = go x
            where
            go :: Int -> Int
            go y = y
        |]
    , accept
        "spec-sig-7-1"
        [r|
        module lib (foo)
          foo :: Int -> Int
        module main (bar)
          import .lib
          foo :: Int -> Str
          bar :: Int -> Str
          bar x = foo x
        |]
    , accept
        "spec-sig-7-3"
        [r|
        module liba (foo)
          foo :: Int -> Int
        module libb (foo)
          foo :: Int -> Str
        module main (bar)
          import .liba
          import .libb
          foo :: Int -> Bool
          bar :: Int -> Bool
          bar x = foo x
        |]
    , rejectUser
        "spec-sig-8-1"
        [r|
        module lib (foo)
          foo :: Int -> Int
        module main (bar)
          import .lib (foo)
          foo :: Int -> Str
          bar :: Int -> Str
          bar x = foo x
        |]
    , rejectUser
        "spec-sig-8-2"
        [r|
        module lib (foo)
          foo :: Int -> Int
        module main (bar)
          import .lib (foo)
          foo :: Int -> Int
          bar :: Int -> Int
          bar x = foo x
        |]
    , accept
        "spec-sig-8-3"
        [r|
        module lib (foo)
          foo :: Int -> Int
        module main (bar)
          import .lib (foo as libFoo)
          foo :: Int -> Str
          bar :: Int -> Int
          bar x = libFoo x
        |]
    , accept
        "spec-sig-8-4"
        [r|
        module lib (foo)
          foo :: Int -> Int
        module main (bar)
          import .lib as l (foo)
          foo :: Int -> Str
          bar :: Int -> Str
          bar x = foo x
        |]
    , rejectUser
        "spec-sig-8-5"
        [r|
        module lib (C)
          class C a where
            foo :: a -> a
        module main (bar)
          import .lib (C)
          foo :: Int -> Int
          bar :: Int -> Int
          bar x = foo x
        |]
    , rejectUser
        "spec-sig-8-6"
        [r|
        module lib (foo)
          foo :: Int -> Int
        module main (bar)
          import .lib (foo)
          class Foo a where
            foo :: a -> Int
          bar :: Int -> Int
          bar x = foo x
        |]
    , rejectUser
        "spec-sig-9-2"
        [r|
        module lib (foo)
          foo :: Int -> Int
        module main (bar)
          import .lib (foo)
          foo x = x
          bar :: Int -> Str
          bar x = foo x
        |]
    , accept
        "spec-sig-9-1"
        [r|
        module lib (foo)
          foo :: Int -> Int
        module main (bar)
          import .lib (foo)
          foo x = x
          bar :: Int -> Int
          bar x = foo x
        |]
    , testCase "spec-sig-5-1" $ do
        -- a generic export is checked, but no instance is chosen for it
        (x, st) <- runFrontWithState sig5Generic
        case x of
          Left e -> assertFailure ("expected acceptance, got: " <> show e)
          Right es -> do
            let chosen = [v | e <- es, AnnoS _ _ (VarS v (Many _ (_ : _))) <- annoNodes e, v `elem` [EV "lt", EV "le"]]
                Specs specs = stateSpecs st
            assertEqual "method uses given implementations" [] chosen
            assertEqual "shared specializations" 0 (length specs)
    , testCase "spec-sig-5-3" $ do
        -- the generic export is warned about and left out; the concrete one
        -- is realized
        config <- emptyConfig
        ((x, msgs), st) <-
          MM.runMorlocMonad Nothing 0 config defaultValue . typecheck Nothing . Code $
            MT.replace "module main (h)" "module main (h, g)" sig5Generic
              <> "\n    g :: Int -> Int\n    g x = h x 1\n"
        case x of
          Left e -> assertFailure ("expected acceptance, got: " <> show e)
          Right (gasts, rasts) -> do
            assertBool ("no warning names h: " <> show msgs) (any (MT.isInfixOf "skipping generic export 'h'") msgs)
            assertBool "g warned about" (not (any (MT.isInfixOf "export 'g'") msgs))
            assertBool "h not recorded" (DM.member (EV "h") (stateSkippedGeneric st))
            let types = [t | AnnoS (Idx _ t) _ _ <- gasts] <> [t | AnnoS (Idx _ t) _ _ <- rasts]
            assertBool "nothing realized" (not (null types))
            assertBool "a generic tree realized" (not (any containsUnk types))
    , testCase "spec-sig-5-4" $ do
        -- a literal at a signature's constrained variable leaves it generic
        config <- emptyConfig
        ((x, msgs), st) <- MM.runMorlocMonad Nothing 0 config defaultValue . typecheck Nothing . Code $ sig5Literal
        case x of
          Left e -> assertFailure ("expected acceptance, got: " <> show e)
          Right _ -> do
            assertBool ("no warning names f: " <> show msgs) (any (MT.isInfixOf "skipping generic export 'f'") msgs)
            assertBool ("no warning names h: " <> show msgs) (any (MT.isInfixOf "skipping generic export 'h'") msgs)
            assertBool "g warned about" (not (any (MT.isInfixOf "export 'g'") msgs))
            assertBool "f not recorded" (DM.member (EV "f") (stateSkippedGeneric st))
    , accept
        "spec-sig-5-6"
        [r|
        module main (f)
        class Integral a where
          ofInt :: a -> Int
        both :: b -> b -> b
        wrap :: a -> ?a
        f :: Integral a => a -> ?a
        f x = both 1 (wrap x)
        |]
    , accept
        "spec-sig-5-2"
        [r|
        module main (x)
          class Monoid a where
            mempty :: a
          instance Monoid Str where
            mempty = ""
          instance Monoid (List a) where
            mempty = []
          x = mempty
        |]
    ]

checkOnceTests :: TestTree
checkOnceTests =
  testGroup
    "SIG-10, POLY-9"
    [ reject
        "spec-sig-10-1"
        [r|
        module main (ok)
          class Integral a where
            add :: a -> a -> a
          instance Integral Int where
            add x y = x
          ok :: Int -> Int
          ok x = x
          bad x = add x "x"
        |]
    , reject
        "spec-sig-10-2"
        [r|
        module main (ok)
          ok :: Int -> Int
          ok x = x
          bad = 1 "x"
        |]
    , reject
        "spec-sig-10-3"
        [r|
        module main (ok)
          ok :: Int -> Int
          ok x = x
          a1 = b1
          b1 = a1
        |]
    , accept
        "spec-sig-10-4"
        [r|
        module main (ok)
          class Integral a where
            add :: a -> a -> a
          instance Integral Int where
            add x y = x
          ok :: Int -> Int
          ok x = x
          good x = add x 1
        |]
    , reject
        "spec-poly-9-1"
        [r|
        module main (ok)
          class Integral a where
            add :: a -> a -> a
          instance Integral Int where
            add x y = x
          ok :: Int -> Int
          ok x = x
          bad t = add (.0 t) "x"
        |]
    , reject
        "spec-sig-10-5"
        [r|
        module main (ok)
          ok :: Int -> Int
          ok x = x
          bad = .[0:2] "abc"
        |]
    , accept
        "spec-pat-5-14"
        [r|
        module main (h)
          class Indexable f where
            __access_index__ :: ?I64 -> f a -> a
          instance Indexable List
          get0 :: Indexable f => f a -> a
          get0 xs = .[0] xs
          h :: [Int] -> Int
          h xs = get0 xs
        |]
    , accept
        "spec-poly-8-8"
        [r|
        module main (h)
          class B a => A a where
            fa :: a -> a
          class A a => B a where
            fb :: a -> a
          instance A Int where
            fa x = x
          instance B Int where
            fb x = x
          both x = fa (fb x)
          h :: Int -> Int
          h x = both x
        |]
    , accept
        "spec-num-10-6"
        [r|
        module main (f, k)
          f :: ?Real
          f = 5
          g :: ?Real -> Real
          g x = 1.0
          k :: Real
          k = g 5
        |]
    , accept
        "spec-poly-9-2"
        [r|
        module main (f)
          getFst = .0
          both p q = (getFst p, getFst q)
          f :: (Int, Str) -> (Str, Bool, Int) -> (Int, Str)
          f p q = both p q
        |]
    , testCase "spec-poly-9-3" $ do
        config <- emptyConfig
        ((x, _), _) <-
          MM.runMorlocMonad Nothing 0 config defaultValue . typecheck Nothing . Code $
            "module main (getFst)\n  getFst = .0\n"
        case x of
          Left (Rejected _ d) -> assertBool ("diagnostic lacks pattern: " <> MT.unpack (render d)) ("is a pattern" `MT.isInfixOf` render d)
          Left e -> assertFailure ("expected a user error, got: " <> show e)
          Right _ -> assertFailure "expected rejection"
    , testCase "spec-poly-9-5" $ do
        config <- emptyConfig
        ((x, msgs), _) <-
          MM.runMorlocMonad Nothing 0 config defaultValue . typecheck Nothing . Code $
            "module lib (getFst)\n  getFst = .0\n\nmodule main (getFst, f)\n  import .lib (getFst)\n  f :: (Int, Str) -> Int\n  f p = getFst p\n"
        case x of
          Left e -> assertFailure ("expected acceptance, got: " <> show e)
          Right _ -> assertBool ("no warning names getFst: " <> show msgs) (any (MT.isInfixOf "skipping generic export 'getFst'") msgs)
    , accept
        "spec-poly-9-4"
        [r|
        module main (setX)
          setX = .(.x = 99) { x = 0, y = "hi" }
        |]
    ]
