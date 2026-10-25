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

import Morloc (typecheck, typecheckFrontend)
import Morloc.Frontend.Namespace
import Morloc.Data.Doc (render)
import qualified Data.Char as DC
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
    , localOption (mkTimeout 2000000) derivTests
    , localOption (mkTimeout 2000000) instanceAnnotationTests
    , localOption (mkTimeout 5000000) dimTests
    , localOption (mkTimeout 2000000) patTests
    , localOption (mkTimeout 2000000) lexTests
    , localOption (mkTimeout 2000000) sigTests
    , localOption (mkTimeout 2000000) baseTests
    , localOption (mkTimeout 2000000) ifaceTests
    , localOption (mkTimeout 20000000) annTests
    , modelCheck
    ]

-- | Typecheck a program written to a directory beside its YAML config, as a
-- labeled program must be: labels are declared in the config.
runLabeled :: String -> MT.Text -> MT.Text -> IO (Either MorlocError ())
runLabeled name code yaml = do
  tmp <- SD.getTemporaryDirectory
  let dir = tmp <> "/morloc-label-test-" <> map (\c -> if c == ' ' then '-' else c) name
  SD.createDirectoryIfMissing True dir
  let path = dir <> "/main.loc"
  writeFile path (MT.unpack code)
  writeFile (dir <> "/main.yaml") (MT.unpack yaml)
  writeFile (dir <> "/lib.py") "def f(x): return x\ndef app(g, x): return g(x)\ndef io(x): return x\n"
  config <- emptyConfig
  ((x, _), _) <- MM.runMorlocMonad Nothing 0 config defaultValue (typecheck (Just path) (Code code))
  SD.removeDirectoryRecursive dir
  return (() <$ x)

labelProgram :: MT.Text -> MT.Text
labelProgram body =
  "module main (g)\neffect IO\nsource Py from \"lib.py\" (\"f\", \"app\", \"io\")\nf :: Int -> Int\napp :: (Int -> Int) -> Int -> Int\nio :: Int -> <IO> Int\nk :: Int\nk = 5\n" <> body

labelAccepts :: String -> MT.Text -> MT.Text -> TestTree
labelAccepts msg yaml body = testCase msg $ do
  result <- runLabeled msg (labelProgram body) yaml
  case result of
    Right _ -> return ()
    Left e -> assertFailure ("expected acceptance, got: " <> show e)

labelRejects :: String -> MT.Text -> MT.Text -> TestTree
labelRejects msg yaml body = testCase msg $ do
  result <- runLabeled msg (labelProgram body) yaml
  case result of
    Left _ -> return ()
    Right _ -> assertFailure "expected an error"

annTests :: TestTree
annTests =
  testGroup
    "ANN"
    [ labelAccepts "spec-ann-8-1" logYaml "g :: Int -> Int\ng x = l@f x\n"
    , labelRejects "spec-ann-8-2" logYaml "g :: Int -> Int\ng x = l@k\n"
    , labelRejects "spec-ann-8-3" logYaml "g :: Int -> Int\ng x = l@(f x)\n"
    , labelAccepts "spec-ann-11-1" logYaml "h :: (Int -> Int) -> Int -> Int\nh q x = l@q x\ng :: Int -> Int\ng x = h f x\n"
    , labelRejects "spec-ann-11-2" cacheYaml "h :: (Int -> Int) -> Int -> Int\nh q x = l@q x\ng :: Int -> Int\ng x = h f x\n"
    , labelAccepts "spec-ann-10-2" logYaml "s :: <IO> Int\ns = io 1\ng :: Int -> <IO> Int\ng x = l@s\n"
    , labelAccepts "spec-ann-10-3" cacheYaml "s :: <IO> Int\ns = io 1\ng :: Int -> <IO> Int\ng x = l@s\n"
    , labelAccepts "spec-ann-11-3" cacheYaml "g :: Int -> <IO> Int\ng x = l@s where\n  s = io x\n"
    ]
  where
    logYaml = "labeled-groups:\n  l: { log: true }\n"
    cacheYaml = "labeled-groups:\n  l: { cache: true }\n"

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
    , reject
        "spec-rec-4-1"
        [r|
        module main (g)
          record Q where
            num :: Int
            den :: Int
          class Same a where
            same :: a -> a -> Bool
          instance Same Q where
            same a b = True
          g :: Int -> Bool
          g n = same {num = n, den = 2} {num = n, den = 4}
        |]
    , accept
        "spec-rec-4-2"
        [r|
        module main (g)
          record Q where
            num :: Int
            den :: Int
          class Same a where
            same :: a -> a -> Bool
          instance Same Q where
            same a b = True
          g :: Int -> Bool
          g n = same ({num = n, den = 2} :: Q) {num = n, den = 4}
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
    , accept
        "spec-alias-4-3"
        [r|
        module main (f)
        type Id a = a
        type T a = (a, ?(T (Id a)))
        f :: T Int -> Int
        |]
    , reject
        "spec-alias-4-4"
        [r|
        module main (f)
        type Id a = a
        type T a = (a, ?(T (Id [a])))
        f :: T Int -> Int
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
    , reject
        "spec-alias-12-2"
        [r|
        module main (f)
        effect IO
        type Scorers a = [a -> Int]
        g :: Scorers (<IO> Int) -> <IO> Int
        f :: Scorers (<> Int) -> <IO> Int
        f x = g x
        |]
    , accept
        "spec-alias-12-3"
        [r|
        module main (f)
        effect IO
        type Scorers a = [a -> Int]
        g :: Scorers (<> Int) -> <IO> Int
        f :: Scorers (<IO> Int) -> <IO> Int
        f x = g x
        |]
    , acceptTypecheck
        "spec-alias-12-4"
        ( "module main (f)\ntype D a = (a, a)\ng :: " <> ladder "a" <> " -> Int\nf :: "
            <> ladder "Int" <> " -> Int\nf x = g x\n" )
    , rejectNaming
        "spec-alias-5-1"
        ["WindSpeed"]
        ["Real"]
        [r|
        module main (f)
        type WindSpeed = Real
        g :: Str -> Int
        f :: WindSpeed -> Int
        f x = g x
        |]
    , accept
        "spec-alias-5-2"
        [r|
        module main (f)
        type WindSpeed = Real
        g :: WindSpeed -> Int
        f :: WindSpeed -> Int
        f x = g x
        |]
    , rejectNaming
        "spec-alias-5-3"
        ["WindSpeed"]
        ["Real"]
        [r|
        module main (f)
        type WindSpeed = Real
        g :: [(Str, Int)] -> Int
        ident :: a -> a
        f :: [(WindSpeed, Int)] -> Int
        f x = g (ident x)
        |]
    , rejectNaming
        "spec-alias-5-5"
        ["WindSpeed", "Label"]
        ["Real", "Str"]
        [r|
        module main (f)
        type WindSpeed = Real
        type Label = Str
        g :: Label -> Int
        f :: WindSpeed -> Int
        f x = g x
        |]
    , accept
        "spec-alias-5-6"
        [r|
        module main (f)
        type WindSpeed = Real
        type Label = Real
        g :: Label -> Int
        f :: WindSpeed -> Int
        f x = g x
        |]
    , accept
        "spec-alias-5-4"
        [r|
        module main (f)
        type WindSpeed = Real
        g :: [(WindSpeed, Int)] -> Int
        ident :: a -> a
        f :: [(WindSpeed, Int)] -> Int
        f x = g (ident x)
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

-- An annotation inside an instance method's body is an ordinary annotation.
instanceAnnotationTests :: TestTree
instanceAnnotationTests =
  testGroup
    "CLS annotations"
    [ acceptTypecheck
        "spec-cls-2-5"
        [r|
        module main (f)
        class Size a where
          size :: a -> Int
        newtype Box = Int
        instance Size Int where
          size x = 1
        instance Size Box where
          size b = (size :: Int -> Int) (unpack b)
        class Packable a b where
          pack :: a -> b
          unpack :: b -> a
        f :: Box -> Int
        f b = size b
        |]
    ]

derivTests :: TestTree
derivTests =
  testGroup
    "DERIV"
    [ accept
        "spec-deriv-1-1"
        [r|
        module main (f)
        class Eq a where
          (==) :: a -> a -> Bool
        class Ord a where
          (<=) :: a -> a -> Bool
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
        class Ord a where
          (<=) :: a -> a -> Bool
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
        class Ord a where
          (<=) :: a -> a -> Bool
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
        class Ord a where
          (<=) :: a -> a -> Bool
        class Show a where
          show :: a -> Str
        class Hashable a where
          hash :: a -> Str
        newtype Name = Str deriving (Show)
        instance Eq Name
        f :: Name -> Int
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

ifaceTests :: TestTree
ifaceTests =
  testGroup
    "IFACE"
    [ reject
        "spec-iface-6-1"
        [r|
        module main (foo)
        fmt :: Int -> Str
        --' @render --raw=fmt
        foo :: Int -> Int
        foo x = x
        |]
    , accept
        "spec-iface-6-2"
        [r|
        module main (foo)
        fmt :: Int -> Str
        --' @render --rawest=fmt
        foo :: Int -> Int
        foo x = x
        |]
    , reject
        "spec-iface-6-3"
        [r|
        module main (foo)
        fmt :: Int -> Str
        --' @with -r/--raw=fmt
        foo :: Int -> Int
        foo x = x
        |]
    , accept
        "spec-iface-6-4"
        [r|
        module main (foo)
        fmt :: Int -> Str
        --' @with -r/--rawest=fmt
        foo :: Int -> Int
        foo x = x
        |]
    ]

patTests :: TestTree
patTests =
  testGroup
    "PAT"
    [ accept
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

baseTests :: TestTree
baseTests =
  testGroup
    "BASE"
    [ rejectUser
        "spec-base-3-2"
        [r|
        module main (f)
          (&&) :: Bool -> Bool -> Bool
          (&&) x y = x
          f :: Bool -> Bool
          f x = x && x
        |]
    , accept
        "spec-base-3-3"
        [r|
        module main (f)
          (&.) :: Bool -> Bool -> Bool
          (&.) x y = x
          f :: Bool -> Bool
          f x = x &. x
        |]
    ]

sigTests :: TestTree
sigTests =
  testGroup
    "SIG"
    [ rejectUser
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
          import lib
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
          import liba
          import libb
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
          import lib (foo)
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
          import lib (foo)
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
          import lib (foo as libFoo)
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
          import lib as l (foo)
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
          import lib (C)
          foo :: Int -> Int
          bar :: Int -> Int
          bar x = foo x
        |]
    , rejectUser
        "spec-sig-9-2"
        [r|
        module lib (foo)
          foo :: Int -> Int
        module main (bar)
          import lib (foo)
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
          import lib (foo)
          foo x = x
          bar :: Int -> Int
          bar x = foo x
        |]
    ]

lexTests :: TestTree
lexTests =
  testGroup
    "LEX"
    [ accept
        "spec-lex-4-1"
        [r|
        module main (a, b, c, d, e)
        effect IO
        record R where
          on :: Bool
          n :: Int
        a :: Int -> <IO> R
        a x = do
          {on = True, n = x}
        b :: Int -> <IO> R
        b x = do {on = False, n = x}
        c :: Bool -> <IO> R
        c t = do
          ? t = do
              {on = False, n = 0}
          : do
              {on = True, n = 1}
        d :: Int -> <IO> Int
        d x = do { let y = x; y }
        e :: Int -> <IO> R
        e x = do { let y = x; {on = True, n = y} }
        |]
    , reject
        "spec-lex-4-2"
        [r|
        module main (d)
        effect IO
        d :: Int -> <IO> Int
        d x = do { y = x; y }
        |]
    , accept
        "spec-cost-6-2"
        [r|
        module main (f)
        source Py from "f.py" where
          --' @selftime 2us + 10ns * 3
          g
        --' Add one
        --' @selftime 10ns
        g :: Int -> Int
        f :: Int -> Int
        f x = g x
        |]
    , reject
        "spec-cost-6-3"
        [r|
        module main (f)
        source Py from "f.py" ("g")
        --' Add one
        --' @selftime 10MB
        g :: Int -> Int
        f :: Int -> Int
        f x = g x
        |]
    , reject
        "spec-cost-6-4"
        [r|
        module main (f)
        source Py from "f.py" ("g")
        --' Add one
        --' @selftime 10ns * n
        g :: Int -> Int
        f :: Int -> Int
        f x = g x
        |]
    , reject
        "spec-cost-6-5"
        [r|
        module main (f)
        source Py from "f.py" where
          --' @selftime 10 parsecs
          g
        g :: Int -> Int
        f :: Int -> Int
        f x = g x
        |]
    , reject
        "spec-cost-6-6"
        [r|
        module main (f)
        source Py from "f.py" where
          --' @selftime 1us - 2us
          g
        g :: Int -> Int
        f :: Int -> Int
        f x = g x
        |]
    , reject
        "spec-cost-6-7"
        [r|
        module main (f)
        --' A number
        --' @selftime 1ns
        type Q = Int
        g :: Q -> Int
        f :: Int -> Int
        f x = g x
        |]
    , reject
        "spec-cost-6-8"
        [r|
        module main (f)
        g :: Int -> Int
        --' Defined here
        --' @selftime 1ns
        f :: Int -> Int
        f x = g x
        h :: Int -> Int
        --' @selftime 1ns
        h x = x
        |]
    , accept
        "spec-src-2-1"
        [r|
        module main (f)
        source Py from "f.py" where
          --' @name plus
          add
        add :: Int -> Int
        f :: Int -> Int
        f x = add x
        |]
    , reject
        "spec-src-2-2"
        [r|
        module main (f)
        source Py from "f.py"
          ( "minus"
          --' @name plus
          , "minus" as add
          )
        add :: Int -> Int
        minus :: Int -> Int
        f :: Int -> Int
        f x = add (minus x)
        |]
    , reject
        "spec-src-2-3"
        [r|
        module main (f)
        source Py from "f.py" where
          --' @name plus
          `and`
        f :: Int -> Int
        f x = x
        |]
    , accept
        "spec-doc-1-2"
        [r|
        module main (f)
        source Py from "f.py"
          ( "h"
          --' @selftime 1ns
          , "g"
          )
        g :: Int -> Int
        h :: Int -> Int
        f :: Int -> Int
        f x = g (h x)
        |]
    , reject
        "spec-doc-1-3"
        [r|
        module main (f)
        source Py from "f.py"
          ( "h"
          --' @selftime 1ns
          ,
          --' @selftime 2ns
          "g"
          )
        g :: Int -> Int
        h :: Int -> Int
        f :: Int -> Int
        f x = g (h x)
        |]
    , accept
        "spec-cost-6-16"
        [r|
        module main (f)
        source Py from "f.py" ("fold")
        --' Fold a list
        fold ::
          --' The function
          --' @calls 2 * 3
          (Int -> Int -> Int) ->
          Int ->
          --' The list
          [Int] ->
          --' The result
          Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = fold add 0 xs
        |]
    , reject
        "spec-cost-6-17"
        [r|
        module main (f)
        source Py from "f.py" ("fold")
        --' Fold a list
        fold ::
          --' The function
          --' @calls 1us
          (Int -> Int -> Int) ->
          Int ->
          --' The list
          [Int] ->
          --' The result
          Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = fold add 0 xs
        |]
    , reject
        "spec-cost-6-18"
        [r|
        module main (f)
        source Py from "f.py" ("fold")
        --' Fold a list
        fold ::
          --' The function
          --' @calls n
          (Int -> Int -> Int) ->
          Int ->
          --' The list
          [Int] ->
          --' The result
          Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = fold add 0 xs
        |]
    , reject
        "spec-cost-6-19"
        [r|
        module main (f)
        source Py from "f.py" ("fold")
        --' Fold a list
        fold ::
          --' The function
          --' @calls 1 - 2
          (Int -> Int -> Int) ->
          Int ->
          --' The list
          [Int] ->
          --' The result
          Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = fold add 0 xs
        |]
    , reject
        "spec-cost-6-20"
        [r|
        module main (f)
        source Py from "f.py" ("fold")
        --' Fold a list
        --' @calls 2
        fold ::
          --' The function
          (Int -> Int -> Int) ->
          Int ->
          --' The list
          [Int] ->
          --' The result
          Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = fold add 0 xs
        |]
    , reject
        "spec-cost-6-21"
        [r|
        module main (f)
        source Py from "f.py" ("fold")
        --' Fold a list
        fold ::
          --' The function
          (Int -> Int -> Int) ->
          Int ->
          --' The list
          [Int] ->
          --' The result
          --' @calls 2
          Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = fold add 0 xs
        |]
    , reject
        "spec-cost-6-22"
        [r|
        module main (f)
        source Py from "f.py" ("fold")
        --' Fold a list
        fold ::
          --' The function
          (Int -> Int -> Int) ->
          Int ->
          --' The list
          --' @calls 2
          [Int] ->
          --' The result
          Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = fold add 0 xs
        |]
    , accept
        "spec-cost-6-23"
        [r|
        module main (f)
        source Py from "f.py" ("fold")
        type Seed = Int
        type Step = Int -> Int -> Int
        --' Fold a list
        fold ::
          (Int -> Int -> Int) ->
          --' The seed or step
          --' @calls 2
          Step ->
          [Int] ->
          Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = 0
        |]
    , rejectUser
        "spec-cost-6-24"
        [r|
        module main (f)
        source Py from "f.py" ("fold")
        type Seed = Int
        type Step = Int -> Int -> Int
        --' Fold a list
        fold ::
          (Int -> Int -> Int) ->
          --' The seed or step
          --' @calls 2
          Seed ->
          [Int] ->
          Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = 0
        |]
    , rejectUser
        "spec-cost-6-25"
        [r|
        module main (f)
        source Py from "f.py" ("fold")
        type Seed = Int
        type Step = Int -> Int -> Int
        --' Fold a list
        fold ::
          (Int -> Int -> Int) ->
          --' The seed or step
          --' @calls 2
          Int ->
          [Int] ->
          Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = 0
        |]
    , rejectUser
        "spec-cost-6-26"
        [r|
        module main (f)
        source Py from "f.py" ("fold")
        type Seed = Int
        type Step = Int -> Int -> Int
        --' Fold a list
        fold ::
          (Int -> Int -> Int) ->
          --' The seed or step
          --' @calls 2
          a ->
          [Int] ->
          Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = 0
        |]
    , reject
        "spec-cost-6-27"
        [r|
        module main (f)
        source Py from "f.py"
          ( "plus" as add
          --' @calls 2
          , "fold"
          )
        fold :: (Int -> Int -> Int) -> Int -> [Int] -> Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = fold add 0 xs
        |]
    , accept
        "spec-cost-5-2"
        [r|
        module main (f)
        source Py from "f.py" ("step")
        --' Step through a table
        --' @selftime 10ns * n * m
        step ::
          --' The rows
          --' @dimension n m
          [[Int]] ->
          --' The result
          --' @dimension n
          [Int]
        f :: [[Int]] -> [Int]
        f xs = step xs
        |]
    , rejectUser
        "spec-cost-5-3"
        [r|
        module main (f)
        source Py from "f.py" ("step")
        --' Step through a table
        --' @selftime 10ns * k
        step ::
          --' The rows
          --' @dimension n
          [[Int]] ->
          --' The result
          [Int]
        f :: [[Int]] -> [Int]
        f xs = step xs
        |]
    , rejectUser
        "spec-cost-5-4"
        [r|
        module main (f)
        source Py from "f.py" ("step")
        --' Step through a table
        --' @selftime 10ns * n
        step ::
          --' The rows
          --' @dimension n m q
          [[Int]] ->
          --' The result
          [Int]
        f :: [[Int]] -> [Int]
        f xs = step xs
        |]
    , reject
        "spec-cost-5-5"
        [r|
        module main (f)
        source Py from "f.py" ("step")
        --' Step through a table
        --' @selftime 10ns * n
        step ::
          --' The rows
          --' @dimension 1.5
          [[Int]] ->
          --' The result
          [Int]
        f :: [[Int]] -> [Int]
        f xs = step xs
        |]
    , reject
        "spec-cost-5-6"
        [r|
        module main (f)
        source Py from "f.py" ("step")
        --' Step through a table
        --' @selftime 10ns * n
        step ::
          --' The rows
          --' @dimension n*m
          [[Int]] ->
          --' The result
          [Int]
        f :: [[Int]] -> [Int]
        f xs = step xs
        |]
    , reject
        "spec-cost-5-7"
        [r|
        module main (f)
        source Py from "f.py" ("step")
        --' Step through a table
        --' @dimension n
        step ::
          --' The rows
          [[Int]] ->
          --' The result
          [Int]
        f :: [[Int]] -> [Int]
        f xs = step xs
        |]
    , reject
        "spec-cost-5-8"
        [r|
        module main (f)
        source Py from "f.py" ("step")
        --' A table
        --' @dimension n
        type Table = [[Int]]
        --' Step through a table
        step ::
          --' The rows
          Table ->
          --' The result
          [Int]
        f :: [[Int]] -> [Int]
        f xs = step xs
        |]
    , accept
        "spec-cost-5-9"
        [r|
        module main (f)
        source Py from "f.py" ("step")
        type Table = [[Int]]
        --' Step through a table
        --' @selftime 10ns * m
        step ::
          --' The rows
          --' @dimension n m
          Table ->
          --' The result
          [Int]
        f :: [[Int]] -> [Int]
        f xs = step xs
        |]
    , accept
        "spec-cost-7-1"
        [r|
        module main (f)
        effect IO
        source Py from "f.py" ("readNums", "keep")
        --' Read numbers
        --' @selftime 1ns * n
        readNums ::
          Str ->
          --' @dimension n
          <IO> [Int]
        --' Keep some numbers
        --' @selftime 2ns * m
        keep ::
          --' @dimension m
          ?[Int] ->
          [Int]
        f :: Str -> <IO> [Int]
        f p = readNums p
        |]
    , rejectUser
        "spec-cost-7-2"
        [r|
        module main (f)
        source Py from "f.py" ("fold")
        fold ::
          --' @calls n
          (Int -> Int -> Int) ->
          Int ->
          [Int] ->
          Int
        add :: Int -> Int -> Int
        f :: [Int] -> Int
        f xs = fold add 0 xs
        |]
    , accept
        "spec-cost-5-10"
        [r|
        module main (f)
        class Mappable f where
          --' Apply to each element
          --' @selftime 2ns * n
          mapEach ::
            --' @calls n
            (a -> b) ->
            --' @dimension n
            f a ->
            --' @dimension n
            f b
        f :: Int -> Int
        f x = x
        |]
    , rejectUser
        "spec-cost-5-11"
        [r|
        module main (f)
        class Mappable f where
          mapEach ::
            (a -> b) ->
            --' @dimension n m
            f a ->
            f b
        f :: Int -> Int
        f x = x
        |]
    , accept
        "spec-cost-5-12"
        [r|
        module main (f)
        class Mappable f where
          mapEach ::
            (a -> b) ->
            --' @dimension n m
            f [a] ->
            f b
        --' @selftime 1ns * n
        each ::
          (a -> b) ->
          --' @dimension n
          g a ->
          g b
        f :: Int -> Int
        f x = x
        |]
    , rejectUser
        "spec-cost-5-13"
        [r|
        module main (f)
        both ::
          (a -> b) ->
          --' @dimension n
          p a c ->
          p b c
        f :: Int -> Int
        f x = x
        |]
    , accept
        "spec-cost-6-10"
        [r|
        module main (f)
        source Py from "f.py"
          ( "h"
          --' @selftime 20ns
          , "g"
          --' @selftime 3ns
          , (+.)
          )
        source Py from "f.py" where
          k
          --' @selftime 1ns
          %inline m
          --' @selftime 2ns
          (-.)
        g :: Int -> Int
        h :: Int -> Int
        k :: Int -> Int
        m :: Int -> Int
        (+.) :: Int -> Int -> Int
        (-.) :: Int -> Int -> Int
        f :: Int -> Int
        f x = m (k (g (h x))) +. 1 -. 2
        |]
    , reject
        "spec-cost-6-11"
        [r|
        module main (f)
        source Py from "f.py"
          ( "h"
          --' @selftime 10MB
          , "g"
          --' @selftime 3ns
          , (+.)
          )
        source Py from "f.py" where
          k
          --' @selftime 1ns
          %inline m
          --' @selftime 2ns
          (-.)
        g :: Int -> Int
        h :: Int -> Int
        k :: Int -> Int
        m :: Int -> Int
        (+.) :: Int -> Int -> Int
        (-.) :: Int -> Int -> Int
        f :: Int -> Int
        f x = m (k (g (h x))) +. 1 -. 2
        |]
    , reject
        "spec-cost-6-12"
        [r|
        module main (f)
        source Py from "f.py"
          ( "h"
          --' @selftime 20ns
          , "g"
          --' @selftime 10MB
          , (+.)
          )
        source Py from "f.py" where
          k
          --' @selftime 1ns
          %inline m
          --' @selftime 2ns
          (-.)
        g :: Int -> Int
        h :: Int -> Int
        k :: Int -> Int
        m :: Int -> Int
        (+.) :: Int -> Int -> Int
        (-.) :: Int -> Int -> Int
        f :: Int -> Int
        f x = m (k (g (h x))) +. 1 -. 2
        |]
    , reject
        "spec-cost-6-13"
        [r|
        module main (f)
        source Py from "f.py"
          ( "h"
          --' @selftime 20ns
          , "g"
          --' @selftime 3ns
          , (+.)
          )
        source Py from "f.py" where
          k
          --' @selftime 10MB
          %inline m
          --' @selftime 2ns
          (-.)
        g :: Int -> Int
        h :: Int -> Int
        k :: Int -> Int
        m :: Int -> Int
        (+.) :: Int -> Int -> Int
        (-.) :: Int -> Int -> Int
        f :: Int -> Int
        f x = m (k (g (h x))) +. 1 -. 2
        |]
    , reject
        "spec-cost-6-14"
        [r|
        module main (f)
        source Py from "f.py"
          ( "h"
          --' @selftime 20ns
          , "g"
          --' @selftime 3ns
          , (+.)
          )
        source Py from "f.py" where
          k
          --' @selftime 1ns
          %inline m
          --' @selftime 10MB
          (-.)
        g :: Int -> Int
        h :: Int -> Int
        k :: Int -> Int
        m :: Int -> Int
        (+.) :: Int -> Int -> Int
        (-.) :: Int -> Int -> Int
        f :: Int -> Int
        f x = m (k (g (h x))) +. 1 -. 2
        |]
    ]

-- | An alias applied to itself 28 times, whose expansion has 2^28 leaves.
ladder :: MT.Text -> MT.Text
ladder leaf = iterate (\t -> "(D " <> t <> ")") leaf !! 28

-- | Acceptance by the typechecker alone, without expanding the resulting
-- types, for a program whose types are exponential once expanded.
acceptTypecheck :: String -> MT.Text -> TestTree
acceptTypecheck name code =
  testCase name $ do
    config <- emptyConfig
    ((x, _), _) <- MM.runMorlocMonad Nothing 0 config defaultValue (typecheckFrontend Nothing (Code code))
    case x of
      Right _ -> return ()
      Left e -> assertFailure $ "expected acceptance, got: " <> show e

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

-- | A rejection reported as the user's error, not as a compiler fault.
rejectUser :: String -> MT.Text -> TestTree
rejectUser name code =
  testCase name $
    runFront code >>= \case
      Right _ -> assertFailure "expected rejection"
      Left (SystemError e) -> assertFailure $ "expected a user error, got a compiler error: " <> show e
      Left _ -> return ()

-- | A rejection whose diagnostic names each type in @named@ and none in
-- @unnamed@, as whole words. Nothing else about its text is checked.
rejectNaming :: String -> [MT.Text] -> [MT.Text] -> MT.Text -> TestTree
rejectNaming name named unnamed code =
  testCase name $
    runFront code >>= \case
      Right _ -> assertFailure "expected rejection"
      Left (SystemError e) -> assertFailure $ "expected a user error, got a compiler error: " <> show e
      Left err -> do
        let ws = MT.split (\c -> not (DC.isAlphaNum c || c == '_')) (diagnosticText err)
        mapM_ (\n -> assertBool ("diagnostic does not name " <> MT.unpack n) (n `elem` ws)) named
        mapM_ (\n -> assertBool ("diagnostic names " <> MT.unpack n) (n `notElem` ws)) unnamed
  where
    diagnosticText (SourcedError _ d) = render d
    diagnosticText (UnificationError _ _ _ d) = render d
    diagnosticText (SystemError d) = render d

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
