{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Patterns
  ( patTests
  , patternConformanceTests
  ) where

import Morloc (typecheckFrontend)
import Morloc.Frontend.Namespace
import Morloc.Data.Doc (render)
import Morloc.Frontend.Typecheck (resolveTypes)
import Morloc.CodeGenerator.Serial (generalTypeToSerialAST, serialAstToMsgpackSchema)
import qualified Morloc.Monad as MM
import qualified Data.Text as MT
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Text.RawString.QQ
import Spec.Common

patTests :: TestTree
patTests =
  testGroup
    "PAT"
    [ reject
        "spec-pat-1-1"
        [r|
        module main (part)
        add :: Int -> Int -> Int
        part :: (Int, Int, Int) -> Int
        part = \(a, b) -> add a b
        |]
    , reject
        "spec-pat-1-2"
        [r|
        module main (f)
        f :: (Int, Int, Int) -> Int
        f t = let (a, b) = t in a
        |]
    , reject
        "spec-pat-1-4"
        [r|
        module main (part)
        part :: (Int, Int, Int) -> Int -> Int
        part (a, b) c = a
        |]
    , reject
        "spec-pat-1-5"
        [r|
        module main (part)
        part :: Int -> (Int, Int, Int) -> Int
        part = \c (a, b) -> a
        |]
    , reject
        "spec-pat-1-6"
        [r|
        module main (f, g)
        first2 t = match t | (a, b) = a
        f :: (Int, Int) -> Int
        f t = first2 t
        g :: (Int, Int, Int) -> Int
        g t = first2 t
        |]
    , reject
        "spec-pat-1-7"
        [r|
        module main (g, f)
        first2 t = match t | (a, b) = a
        f :: (Int, Int) -> Int
        f t = first2 t
        g :: (Int, Int, Int) -> Int
        g t = first2 t
        |]
    , reject
        "spec-pat-1-8"
        [r|
        module main (f)
        effect IO
        data R = Ok Int | Err Str
        get :: Int -> <IO> (R, Int, Int)
        f :: Int -> <IO> Int
        f x = do
          (Ok y, z) <- get x
          z
        |]
    , reject
        "spec-pat-1-9"
        [r|
        module main (f)
        f :: (Int, Int, Int) -> Int
        f t = let (_, _) = t in 1
        |]
    , rejectNaming
        "spec-pat-1-10"
        ["z"]
        []
        [r|
        module main (f)
        record R where
          p :: Int
        f :: R -> Int
        f = \{z = x} -> 1
        |]
    , accept
        "spec-pat-1-11"
        [r|
        module main (f)
        f :: Int -> Int
        f x = let h = \(a, b) -> a in x
        |]
    , rejectNaming
        "spec-pat-1-12"
        ["tuple", "2", "elements"]
        []
        [r|
        module main (f)
        f t = let (a, b) = t in let (c, d, e) = t in a
        |]
    , rejectNaming
        "spec-pat-1-13"
        ["tuple", "2", "elements"]
        []
        [r|
        module main (g)
        f t = let (a, b) = t in .2 t
        g :: (Int, Int, Int) -> Int
        g = f
        |]
    , rejectNaming
        "spec-pat-1-14"
        ["tuple", "Int"]
        []
        [r|
        module main (f)
        f :: Int -> Int
        f x = let (_, _) = x in 1
        |]
    , rejectNaming
        "spec-pat-1-15"
        ["tuple", "2", "elements"]
        []
        [r|
        module main (g)
        g :: (Int, Int, Int) -> Int
        g = \(_, _) -> 1
        |]
    , rejectNaming
        "spec-pat-1-16"
        ["field", "z"]
        []
        [r|
        module main (f)
        record R where
          p :: Int
        f :: R -> Int
        f {z = _} = 1
        |]
    , rejectNaming
        "spec-pat-1-19"
        ["record", "tuple"]
        []
        [r|
        module main (f)
        f t = let (_, _) = t in let ({x = _}) = t in 1
        |]
    , rejectNaming
        "spec-pat-1-20"
        ["field", "y"]
        []
        [r|
        module main (f)
        f :: {x = Int} -> Int
        f {y = _} = 1
        |]
    , accept
        "spec-pat-1-21"
        [r|
        module main (f)
        f :: {x = Int, y = Str} -> Int
        f {x = a} = a
        |]
    , rejectAtLine
        "spec-pat-1-22"
        5
        [r|
        module main (f)
        f :: (Int, (Int, Int, Int)) -> Int
        f = \( c
             , (a, b)) -> c
        |]
    , rejectNaming
        "spec-pat-1-23"
        ["field", "y"]
        []
        [r|
        module main (f)
        f :: {x = Int} -> Int
        f r = match r | {y = _} = 1
        |]
    , rejectNaming
        "spec-pat-1-24"
        ["tuple", "2", "elements"]
        []
        [r|
        module main (f)
        f :: (Int, Int, Int) -> Int
        f (_, _) = x
          where
          x = 1
        |]
    , rejectUser
        "spec-pat-1-25"
        [r|
        module main (f)
        f t = let (a, b) = t in .2 t
        |]
    , rejectNaming
        "spec-pat-1-26"
        ["tuple", "record"]
        []
        [r|
        module main (f)
        f t = (.0 t, .x t)
        |]
    , rejectNaming
        "spec-pat-1-27"
        ["tuple", "record"]
        []
        [r|
        module main (f)
        f t = (.x t, .0 t)
        |]
    , rejectUser
        "spec-pat-1-28"
        [r|
        module main (g)
        record R = R {x :: Int}
        f t = (.x t, .0 t)
        g :: R -> (Int, Int)
        g r = f r
        |]
    , accept
        "spec-pat-1-29"
        [r|
        module main (f)
        f t = let (a, b) = t in .1 t
        |]
    , accept
        "spec-pat-1-17"
        [r|
        module main (f, g)
        fst2 (a, b) = a
        f :: (Int, Str) -> Int
        f = fst2
        g :: (Str, Int) -> Str
        g = fst2
        |]
    , accept
        "spec-pat-1-18"
        [r|
        module main (f)
        record R where
          p :: Int
          q :: Str
        f :: R -> Int
        f {p = x} = x
        |]
    , accept
        "spec-pat-1-3"
        [r|
        module main (f)
        add :: Int -> Int -> Int
        f :: (Int, (Int, Int)) -> Int
        f (a, (b, c)) = add a (add b c)
        |]
    , accept
        "spec-pat-3-1"
        [r|
        module main (f)
        data L = Nil | Cons Str L
        f :: L -> Int
        f xs = match xs
          | (Cons x (Cons y rest)) = 2
          | (Cons x Nil) = 1
          | Nil = 0
        |]
    , rejectNaming
        "spec-pat-3-2"
        ["Cons", "Nil"]
        []
        [r|
        module main (f)
        data L = Nil | Cons Str L
        f :: L -> Int
        f xs = match xs
          | (Cons x (Cons y rest)) = 2
          | Nil = 0
        |]
    , reject
        "spec-pat-3-3"
        [r|
        module main (f)
        f :: Int -> Int
        f | 0 = 1
          | 1 = 2
        |]
    , accept
        "spec-pat-3-4"
        [r|
        module main (f)
        data B = Y | N
        f :: (B, B) -> Int
        f | (Y, _) = 1
          | (_, Y) = 2
          | (N, N) = 3
        |]
    , rejectNaming
        "spec-pat-3-5"
        ["Y"]
        []
        [r|
        module main (f)
        data B = Y | N
        f :: (B, B) -> Int
        f | (N, _) = 1
          | (Y, N) = 2
        |]
    , rejectNaming
        "spec-pat-3-6"
        ["Blue"]
        ["Red", "Green"]
        [r|
        module main (f)
        data Color = Red | Green | Blue
        f :: Color -> Int
        f | Red = 0
          | Green = 1
        |]
    , rejectNaming
        "spec-pat-3-7"
        ["Dot"]
        []
        [r|
        module main (f)
        data Shape = Circle Real | Rect Real Real | Dot
        f :: Shape -> Real
        f | (Circle r) = r
          | (Rect w h) = w
        |]
    , rejectNaming
        "spec-pat-3-8"
        ["Circle"]
        []
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
        instance Eq Real
        data Shape = Circle Real | Dot
        f :: Shape -> Str
        f | (Circle 0.0) = "degenerate"
          | Dot = "dot"
        |]
    , reject
        "spec-pat-3-9"
        [r|
        module main (f)
        f :: Int -> Int
        f | 0 ? True = 1
              : 2
        |]
    , acceptWarning
        "spec-pat-8-1"
        "unreachable"
        [r|
        module main (f)
        data A = Foo | Bar
        f :: A -> Int
        f | Foo = 1
          | _ = 2
          | Bar = 3
        |]
    , acceptWarning
        "spec-pat-8-2"
        "unreachable"
        [r|
        module main (f)
        data A = Foo | Bar
        f :: A -> Int
        f x = match x
          | Foo = 1
          | Bar = 2
          | Foo = 3
        |]
    , acceptWarning
        "spec-pat-8-3"
        "unreachable"
        [r|
        module main (f)
        data Color = Red | Green | Blue
        f :: Color -> Int
        f | x = 0
          | Green = 1
        |]
    , accept
        "spec-pat-7-1"
        [r|
        module main (f, g, h)
        f :: [Int] -> Int
        f | [] = 0
          | [x] = x
          | [x, y] = y
          | xs = 3
        g :: (Int, [[Str]]) -> [Str]
        g | (n, [[s], _]) = [s]
          | (n, [ys]) = ys
          | _ = []
        h :: [Bool] -> Bool
        h xs = match xs | all@[a, _] = a | _ = True
        |]
    , reject
        "spec-pat-7-2"
        [r|
        module main (f)
        f :: [Int] -> Int
        f | [] = 0
          | [x] = x
        |]
    , reject
        "spec-pat-7-3"
        [r|
        module main (f)
        newtype Deque a = List a
        f :: Deque Int -> Int
        f | [x] = x
          | _ = 0
        |]
    , reject
        "spec-pat-7-4"
        [r|
        module main (f)
        f :: [Int] -> Str
        f | [x] = x
          | _ = "none"
        |]
    , reject
        "spec-pat-7-6"
        [r|
        module main (f)
        f :: Vector 2 Int -> Int
        f | [x, y] = y
          | _ = 0
        |]
    , accept
        "spec-pat-5-1"
        [r|
        module main (f, g, h)
        class Indexable f where
          __access_index__ :: ?I64 -> f a -> a
        class Sliceable f where
          __get_slice__ :: ?I64 -> ?I64 -> ?I64 -> f a -> f a
        instance Indexable List
        instance Sliceable List
        f :: [Int] -> Int
        f x = .[0] x
        g :: [Int] -> [Int]
        g x = .[0:1] x
        map :: (a -> b) -> [a] -> [b]
        h :: [[Int]] -> [Int]
        h xss = map .[0] xss
        |]
    , reject
        "spec-pat-5-2"
        [r|
        module main (f)
        class Indexable f where
          __access_index__ :: ?I64 -> f a -> a
        instance Indexable List
        f :: (Int, Int) -> Int
        f x = .[0] x
        |]
    , reject
        "spec-pat-5-3"
        [r|
        module main (g)
        class Sliceable f where
          __get_slice__ :: ?I64 -> ?I64 -> ?I64 -> f a -> f a
        instance Sliceable List
        g :: Int -> Int
        g x = .[0:1] x
        |]
    , reject
        "spec-pat-5-4"
        [r|
        module main (f)
        class Indexable f where
          __access_index__ :: ?I64 -> f a -> a
        instance Indexable List
        newtype Deque a = List a
        f :: Deque Int -> Int
        f x = .[0] x
        |]
    , reject
        "spec-pat-5-5"
        [r|
        module main (k)
        class Indexable f where
          __access_index__ :: ?I64 -> f a -> a
        instance Indexable List
        f :: g a -> a
        f x = .[0] x
        k :: (Int, Int) -> Int
        k p = f p
        |]
    , reject
        "spec-pat-5-6"
        [r|
        module main (h)
        class Indexable f where
          __access_index__ :: ?I64 -> f a -> a
        instance Indexable List
        map :: (a -> b) -> [a] -> [b]
        h :: [(Int, Int)] -> [Int]
        h xss = map .[0] xss
        |]
    , reject
        "spec-pat-5-7"
        [r|
        module main (g)
        class Sliceable f where
          __get_slice__ :: ?I64 -> ?I64 -> ?I64 -> f a -> f a
        instance Sliceable List
        g :: ?[Int] -> ?[Int]
        g x = .[0:1] x
        |]
    , rejectUser
        "spec-pat-5-8"
        [r|
        module main (g)
        class Sliceable f where
          __get_slice__ :: ?I64 -> ?I64 -> ?I64 -> f a -> f a
        instance Sliceable List
        g :: (Int -> Int) -> (Int -> Int)
        g f = .[0:1] f
        |]
    , reject
        "spec-pat-5-9"
        [r|
        module main (g)
        g :: IFile Int -> Int
        g h = .[0:2] h
        |]
    , accept
        "spec-pat-5-10"
        [r|
        module main (g)
        g :: IFile [Int] -> [Int]
        g h = .[0:2] h
        |]
    ]

-- | The runtime pattern check (morloc-nexus view) agrees with PAT-5: each
-- row of the table the Rust tests also read names a pattern, a type and
-- that type's schema, and whether the pattern applies. The schema is the
-- one morloc writes for the type.
patternConformanceTests :: TestTree
patternConformanceTests = testCase "spec-pat-9-1" $ do
  table <- MT.pack <$> readFile "data/rust/morloc-runtime-types/tests/pattern-conformance.tsv"
  let rows = [MT.splitOn "\t" l | l <- MT.lines table, not (MT.null l), not ("#" `MT.isPrefixOf` l)]
      prelude ty = "module main (h)\n  record R = R { x :: Int, y :: Str }\n  const :: a -> b -> a\n  h :: " <> ty <> " -> " <> ty <> "\n"
  problems <- forM rows $ \case
    [pat, ty, schema, expected] -> do
      config <- emptyConfig
      ((applied, _), _) <-
        MM.runMorlocMonad Nothing 0 config defaultValue . typecheckFrontend Nothing . Code $
          prelude ty <> "  h v = const v (" <> pat <> " v)\n"
      ((written, _), _) <-
        MM.runMorlocMonad Nothing 0 config defaultValue $ do
          es <- typecheckFrontend Nothing (Code (prelude ty <> "  h v = v\n"))
          case map resolveTypes es of
            [AnnoS (Idx i (FunT [t] _)) _ _] -> render . serialAstToMsgpackSchema <$> generalTypeToSerialAST i t
            _ -> return "?"
      let accepted = either (const "no") (const "yes") applied
          schemaOk = either (const False) (== schema) written
      return $
        [MT.unpack (pat <> " on " <> ty <> ": the compiler says " <> accepted) | accepted /= expected]
          <> [MT.unpack (ty <> ": its schema is " <> either (MT.pack . show) id written) | not schemaOk]
    row -> return ["malformed row: " <> show row]
  let found = concat problems
  unless (null found) (assertFailure (unlines found))
