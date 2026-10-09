{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : SpecTests
-- Description : Unit tests cited by the language spec
--
-- Each test is named by its spec ID, spec-<prefix>-<rule>-<n>, and is cited
-- by the `Tests:` line of that rule.
module SpecTests (specTests) where

import Morloc (typecheckFrontend)
import Morloc.Frontend.Namespace
import Morloc.Frontend.Typecheck (evaluateAnnoSTypes)
import qualified Morloc.Monad as MM
import qualified Data.Text as MT
import qualified System.Directory as SD
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit
import Text.RawString.QQ

specTests :: TestTree
specTests =
  testGroup "spec"
    [ localOption (mkTimeout 2000000) modTests
    , localOption (mkTimeout 5000000) aliasTests
    , localOption (mkTimeout 2000000) kindTests
    , localOption (mkTimeout 2000000) teqTests
    , localOption (mkTimeout 2000000) newtTests
    , modelCheck
    ]

-- | The items in model/ agree with each other and with these tests.
modelCheck :: TestTree
modelCheck = testCase "model/check.sh passes" $ do
  (code, out, err) <- readProcessWithExitCode "bash" ["model/check.sh"] ""
  case code of
    ExitSuccess -> return ()
    ExitFailure _ -> assertFailure (out <> err)

modTests :: TestTree
modTests =
  testGroup
    "MOD"
    [ accept
        "spec-mod-14-1"
        [r|
        module lib (mkP)
          type P = Int
          mkP :: Int -> P
        module main (g)
          import lib (mkP)
          type P = Str
          g :: Int -> Int
          g x = mkP x
        |]
    , reject
        "spec-mod-14-2"
        [r|
        module lib (mkP)
          type P = Int
          mkP :: Int -> P
        module main (g)
          import lib (mkP)
          type P = Str
          g :: Int -> Str
          g x = mkP x
        |]
    , reject
        "spec-mod-15-1"
        [r|
        module main (f)
          f :: Foo -> Foo
        |]
    , accept
        "spec-mod-15-2"
        [r|
        module main (f)
          type Foo = Int
          f :: Foo -> Foo
        |]
    , reject
        "spec-mod-16-1"
        [r|
        module lib (P, mkP)
          type P = Int
          mkP :: Int -> P
        module main (f)
          import lib (P, mkP)
          type P = Str
          f :: Int -> P
          f x = mkP x
        |]
    , accept
        "spec-mod-16-2"
        [r|
        module lib (P, mkP)
          type P = Int
          mkP :: Int -> P
        module main (f)
          import lib (mkP)
          type P = Str
          f :: Int -> Int
          f x = mkP x
        |]
    ]

aliasTests :: TestTree
aliasTests =
  testGroup
    "ALIAS"
    [ accept
        "spec-alias-1-1"
        [r|
        module main (f)
        type Pt = (Int, Int)
        f :: Pt -> (Int, Int)
        f x = x
        |]
    , reject
        "spec-alias-1-2"
        [r|
        module main (f)
        type Pt = (Int, Int)
        f :: Pt -> (Int, Str)
        f x = x
        |]
    , accept
        "spec-alias-1-3"
        [r|
        module main (f)
        type A = Int
        type B = Int
        f :: A -> B
        f x = x
        |]
    , accept
        "spec-alias-1-4"
        [r|
        module main (leaves)
        data Tree a = Leaf a | Node (Tree a) (Tree a)
        type IntTree = Tree Int
        leaves :: IntTree -> Int
        leaves | (Leaf _) = 1
               | (Node a b) = leaves a
        |]
    , accept
        "spec-alias-1-5"
        [r|
        module main (size)
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        instance Foldable List
        count :: Int -> a -> Int
        length :: Foldable f => f a -> Int
        length = fold count 0
        type Bag a = [a]
        size :: Bag Int -> Int
        size xs = length xs
        |]
    , accept
        "spec-alias-1-6"
        [r|
        module main (sizeOf)
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        instance Foldable List
        count :: Int -> a -> Int
        length :: Foldable f => f a -> Int
        length = fold count 0
        type Model k v = [(k, v)]
        sizeOf :: Model k Int -> Int
        sizeOf xs = length xs
        |]
    , reject
        "spec-alias-2-1"
        [r|
        module main (f)
        type Ph a = Int
        f :: Ph Str -> Int
        |]
    , accept
        "spec-alias-2-2"
        [r|
        module main (f)
        type Ph a = [a]
        f :: Ph Str -> Int
        |]
    , reject
        "spec-alias-2-3"
        [r|
        module main (f)
        type FP (n :: Nat) a = (a, a)
        f :: FP 2 Int -> Int
        |]
    , accept
        "spec-alias-2-4"
        [r|
        module main (f)
        newtype V (n :: Nat) a = List a
        type FP (n :: Nat) a = V n a
        f :: FP 2 Int -> Int
        |]
    , reject
        "spec-alias-3-1"
        [r|
        module main (f)
        type P a b = (a, b)
        f :: P Int -> Int
        |]
    , accept
        "spec-alias-3-2"
        [r|
        module main (f)
        type P a b = (a, b)
        f :: P Int Int -> Int
        |]
    , reject
        "spec-alias-3-3"
        [r|
        module main (f)
        type P a b = (a, b)
        f :: P Int Int Int -> Int
        |]
    , reject
        "spec-alias-3-4"
        [r|
        module main (g)
        class Sz f where
          sz :: f a -> Int
        type P a b = (a, b)
        g :: Sz (P Int) => P Int Int -> Int
        |]
    , accept
        "spec-alias-3-5"
        [r|
        module main (g)
        class Sz f where
          sz :: f a -> Int
        newtype P a b = (a, b)
        g :: Sz (P Int) => P Int Int -> Int
        |]
    , reject
        "spec-alias-3-6"
        [r|
        module main (x)
        type P a b = (a, b)
        x = (1, 2) :: P Int
        |]
    , accept
        "spec-alias-3-7"
        [r|
        module main (x)
        type P a b = (a, b)
        x = (1, 2) :: P Int Int
        |]
    , reject
        "spec-alias-3-8"
        [r|
        module main (f)
        type P a b = (a, b)
        type Q = P Int
        f :: Q -> Int
        |]
    , accept
        "spec-alias-3-9"
        [r|
        module main (f)
        type P a b = (a, b)
        type Q = P Int Int
        f :: Q -> Int
        |]
    , reject
        "spec-alias-3-10"
        [r|
        module main (f)
        newtype G (r :: Nat) a = List a
        type M a n b = (G n a, b)
        f :: M Int -> Int
        |]
    , accept
        "spec-alias-3-11"
        [r|
        module main (f)
        newtype G (r :: Nat) a = List a
        type M a n b = (G n a, b)
        f :: M Int Str -> Int
        |]
    , reject
        "spec-alias-4-1"
        [r|
        module main (f)
        type N a = (a, [N [a]])
        f :: N Int -> Int
        |]
    , accept
        "spec-alias-4-2"
        [r|
        module main (f)
        type N a = (a, [N a])
        f :: N Int -> Int
        |]
    , reject
        "spec-alias-6-1"
        [r|
        module main (f)
        class Sh a where
          sh :: a -> Str
        type A = Int
        instance Sh A
        f :: A -> Str
        f x = sh x
        |]
    , accept
        "spec-alias-6-2"
        [r|
        module main (f)
        class Sh a where
          sh :: a -> Str
        newtype A = Int
        instance Sh A
        f :: A -> Str
        f x = sh x
        |]
    , reject
        "spec-alias-6-3"
        [r|
        module main (f)
        type A = Int
        type Py => A = "int"
        f :: A -> Int
        |]
    , accept
        "spec-alias-6-4"
        [r|
        module main (f)
        newtype A = Int
        type Py => A = "int"
        f :: A -> Int
        |]
    , accept
        "spec-alias-6-5"
        [r|
        module main (f)
        class Sh a where
          sh :: a -> Str
        instance Sh Int
        type A = Int
        type B = Int
        f :: A -> B -> Str
        f x y = sh y
        |]
    , accept
        "spec-alias-7-1"
        [r|
        module main (f)
        type P = [P]
        type Q = [Q]
        g :: Q -> Int
        f :: P -> Int
        f x = g x
        |]
    , accept
        "spec-alias-7-2"
        [r|
        module main (f)
        type T a = (a, ?(T a))
        f :: T Int -> Int
        |]
    , accept
        "spec-alias-8-1"
        [r|
        module main (f)
        newtype G (r :: Nat) a = List a
        type V n = G n Int
        f :: V 3 -> Int
        |]
    , reject
        "spec-alias-8-2"
        [r|
        module main (f)
        newtype G (r :: Nat) a = List a
        type V n = G n Int
        f :: V Str -> Int
        |]
    , reject
        "spec-alias-8-3"
        [r|
        module main (f)
        newtype G (r :: Nat) a = List a
        type Bad h = (G h Int, [h])
        f :: Bad 2 -> Int
        |]
    , accept
        "spec-alias-8-4"
        [r|
        module main (f)
        newtype G (r :: Nat) a = List a
        type Ok h = (G h Int, [Int])
        f :: Ok 2 -> Int
        |]
    , reject
        "spec-alias-8-5"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type Bed3 r = {chrom = Str, start = Int} + r
        f :: Tbl 3 (Bed3 Int) -> Int
        |]
    , accept
        "spec-alias-8-6"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type Bed3 r = {chrom = Str, start = Int} + r
        f :: Tbl 3 (Bed3 {name = Str}) -> Int
        |]
    , accept
        "spec-alias-8-7"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type B r s = {c = Str} + r + s
        f :: Tbl 3 (B {a = Int} {b = Int}) -> Int
        |]
    , accept
        "spec-alias-8-8"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type WithCol r k a = r + Singleton k a
        f :: Tbl 3 (WithCol {x = Int} "y" Int) -> Int
        |]
    , accept
        "spec-alias-8-9"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type Cols = {x = Int}
        type Ext r = Cols + r
        f :: Tbl 3 (Ext {y = Int}) -> Int
        |]
    , accept
        "spec-alias-8-10"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type Bed3 r = {c = Str} + r
        f :: Tbl 3 (Bed3 r) -> Tbl 3 ({x = Int} + r)
        |]
    , accept
        "spec-alias-8-11"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type Bed3 r = {c = Str} + r
        f :: Tbl 3 ({x = Int} + r) -> Tbl 3 (Bed3 r)
        |]
    , accept
        "spec-alias-8-12"
        [r|
        module main (f)
        newtype G (r :: Nat) a = List a
        type V n = G n Int
        f :: V (n + 1) -> Int
        |]
    , reject
        "spec-alias-8-13"
        [r|
        module main (f)
        type B a = [a]
        f :: B 3 -> Int
        |]
    , accept
        "spec-alias-8-14"
        [r|
        module main (f)
        type B a = [a]
        f :: B Int -> Int
        |]
    , reject
        "spec-alias-8-15"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type M n r = Tbl n r
        f :: M Int -> Int
        |]
    , accept
        "spec-alias-8-16"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type M n r = Tbl n r
        f :: M 3 -> Int
        |]
    , reject
        "spec-alias-9-1"
        [r|
        module main (f)
        type A = Int
        type A = Str
        f :: A -> Int
        |]
    , accept
        "spec-alias-9-2"
        [r|
        module main (f)
        type A = Int
        type B = Str
        f :: A -> Int
        |]
    , reject
        "spec-alias-9-3"
        [r|
        module main (f)
        type A = Int
        type A a = [a]
        f :: A -> Int
        |]
    , accept
        "spec-alias-9-4"
        [r|
        module main (f)
        type A = Int
        type B a = [a]
        f :: A -> Int
        |]
    , reject
        "spec-alias-9-5"
        [r|
        module main (f)
        type A = Int
        newtype A = Str
        f :: A -> Int
        |]
    , accept
        "spec-alias-9-6"
        [r|
        module main (f)
        type A = Int
        newtype B = Str
        f :: A -> Int
        |]
    , accept
        "spec-alias-9-7"
        [r|
        module lib (g)
          type A = Int
          g :: A -> Int
        module main (f)
          import lib (g)
          type A = Str
          f :: Int -> Int
          f x = g x
        |]
    , accept
        "spec-alias-11-1"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type Cols = {x = Int, y = Bool}
        f :: Tbl n Cols -> Int
        |]
    , accept
        "spec-alias-11-2"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type Cols = {x = Int, y = Bool}
        type MyTable n = Tbl n Cols
        f :: MyTable n -> Int
        |]
    , reject
        "spec-alias-11-3"
        [r|
        module main (f)
        type Twice = (a, a)
        f :: Twice -> Int
        |]
    , accept
        "spec-alias-11-4"
        [r|
        module main (f)
        type Twice a = (a, a)
        f :: Twice Int -> Int
        |]
    , reject
        "spec-alias-11-5"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type Cols = {x = Int, y = Bool}
        type MyTable = Tbl n Cols
        f :: MyTable -> Int
        |]
    , accept
        "spec-alias-12-1"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
        g :: Foo 2 2 -> Int
        f :: Foo 1 3 -> Int
        f x = g x
        |]
    ]

kindTests :: TestTree
kindTests =
  testGroup
    "KIND"
    [ accept
        "spec-kind-6-2"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type MyTable n = Tbl n {x = Int}
        f :: MyTable -> Int
        |]
    , accept
        "spec-kind-6-3"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type MyTable n = Tbl n {x = Int}
        f :: MyTable 3 -> Int
        |]
    , accept
        "spec-kind-6-1"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        type V (n :: Nat) a = Vector n a
        f :: V Int -> Int
        |]
    ]

teqTests :: TestTree
teqTests =
  testGroup
    "TEQ"
    [ accept
        "spec-teq-1-1"
        [r|
        module main (f)
        type Pair a = (a, a)
        type Two = Pair Int
        f :: Two -> (Int, Int)
        f x = x
        |]
    , reject
        "spec-teq-1-2"
        [r|
        module main (f)
        type Pair a = (a, a)
        type Two = Pair Int
        f :: Two -> (Int, Str)
        f x = x
        |]
    ]

newtTests :: TestTree
newtTests =
  testGroup
    "NEWT"
    [ accept
        "spec-newt-3-1"
        [r|
        module main (f)
        newtype Buffer (n :: Nat) a = List a
        f :: Buffer 3 Int -> Int
        |]
    ]

accept :: String -> MT.Text -> TestTree
accept name code =
  testCase name $
    runFront code >>= \case
      Right _ -> return ()
      Left e -> assertFailure $ "expected acceptance, got: " <> show e

-- Passes on any error. Its accept twin, which differs only in the rule's
-- subject, shows that the rule is what rejects it.
reject :: String -> MT.Text -> TestTree
reject name code =
  testCase name $
    runFront code >>= \case
      Right _ -> assertFailure "expected rejection"
      Left _ -> return ()

runFront :: MT.Text -> IO (Either MorlocError [AnnoS (Indexed TypeU) Many Int])
runFront code = do
  config <- emptyConfig
  ((x, _), _) <-
    MM.runMorlocMonad
      Nothing
      0
      config
      defaultValue
      (typecheckFrontend Nothing (Code code) >>= mapM evaluateAnnoSTypes)
  return x

emptyConfig :: IO Config
emptyConfig = do
  home <- SD.getHomeDirectory
  return $
    Config
      { configHome = home <> "/.local/share/morloc"
      , configState = home <> "/.local/share/morloc"
      , configLibrary = home <> "/.local/share/src/morloc"
      , configPlane = "default"
      , configPlaneCore = "morloclib"
      , configTmpDir = home <> "/.morloc/tmp"
      , configBuildConfig = home <> "/.morloc/.build-config.yaml"
      , configLangOverrides = mempty
      , configRegistry = Nothing
      }
