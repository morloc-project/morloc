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
import qualified Data.Map.Strict as DM
import qualified Data.Set as DS
import Morloc.Typecheck.NatDecide (Condition (..), Decision (..), decide)
import Morloc.Typecheck.NatSolver (NatExpr (..))

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
    , localOption (mkTimeout 2000000) dim7Tests
    , localOption (mkTimeout 2000000) cls16Tests
    , localOption (mkTimeout 5000000) poly7Tests
    , localOption (mkTimeout 2000000) bindTests
    , localOption (mkTimeout 2000000) hostTests
    , localOption (mkTimeout 2000000) effTests
    , localOption (mkTimeout 2000000) appTests
    , localOption (mkTimeout 2000000) patTests
    , localOption (mkTimeout 2000000) recTests
    , localOption (mkTimeout 2000000) collTests
    , localOption (mkTimeout 2000000) sumTests
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

-- | The frontend on a program under test-suite/fixtures/modules, with that
-- directory's library standing for the installed modules.
runFixture :: Bool -> FilePath -> IO (Either MorlocError [AnnoS (Indexed TypeU) Many Int])
runFixture autoInstall rel = do
  root <- SD.makeAbsolute "test-suite/fixtures/modules"
  config <- emptyConfig
  let path = root <> "/" <> rel
  code <- MT.pack <$> readFile path
  ((x, _), _) <-
    MM.runMorlocMonad Nothing 0 config {configLibrary = root <> "/library"} defaultValue
      (MM.modify (\st -> st {stateAutoInstall = autoInstall}) >> typecheckFrontend (Just path) (Code code))
  return x

acceptFixture :: String -> FilePath -> TestTree
acceptFixture name rel =
  testCase name $
    runFixture False rel >>= \case
      Right _ -> return ()
      Left e -> assertFailure $ "expected acceptance, got: " <> show e

-- | A user rejection whose diagnostic contains @needle@, with or without
-- the installation of missing modules.
rejectFixture :: String -> Bool -> MT.Text -> FilePath -> TestTree
rejectFixture name autoInstall needle rel =
  testCase name $
    runFixture autoInstall rel >>= \case
      Right _ -> assertFailure "expected rejection"
      Left (Rejected _ d) ->
        assertBool ("diagnostic lacks " <> MT.unpack needle <> ": " <> MT.unpack (render d)) (needle `MT.isInfixOf` render d)
      Left e -> assertFailure $ "expected a user error, got: " <> show e

modTests :: TestTree
modTests =
  testGroup
    "MOD"
    [ acceptFixture "spec-mod-6-1" "mod-6/main.loc"
    , acceptFixture "spec-mod-7-1" "mod-7/system.loc"
    , acceptFixture "spec-mod-7-2" "mod-7/local.loc"
    , rejectFixture "spec-mod-7-3" False "import .mloc" "mod-7/missing.loc"
    , rejectFixture "spec-mod-7-4" True "import .mloc" "mod-7/missing.loc"
    , accept
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
    , reject
        "spec-kind-2-1"
        [r|
        module main (f)
        type R = Singleton "x" Int
        f :: R -> Int
        f x = 1
        |]
    , accept
        "spec-kind-2-2"
        [r|
        module main (f)
        type R = {x = Int}
        f :: R -> Int
        f x = 1
        |]
    , reject
        "spec-kind-2-3"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        f :: Vector -> Int
        f x = 1
        |]
    , accept
        "spec-kind-2-4"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        f :: Vector Int -> Int
        f x = 1
        |]
    , accept
        "spec-kind-2-5"
        [r|
        module main (f)
        type P a = {x = a}
        f :: P Int -> Int
        f x = 1
        |]
    , reject
        "spec-kind-2-6"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        f :: [Vector] -> Int
        f x = 1
        |]
    , reject
        "spec-kind-2-7"
        [r|
        module main (f)
        f :: {x = Singleton "y" Int} -> Int
        f x = 1
        |]
    , reject
        "spec-kind-2-8"
        [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        type V = Vector
        f :: V -> Int
        f x = 1
        |]
    , reject
        "spec-kind-2-9"
        [r|
        module main (f)
        class Sh a where
          sh :: a -> Singleton "x" Int
        f :: Int -> Int
        f x = 1
        |]
    , reject
        "spec-kind-5-4"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type X = Tbl Int
        f :: X -> Int
        f x = 1
        |]
    , reject
        "spec-kind-5-1"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        f :: Tbl 3 4 -> Int
        f x = 1
        |]
    , reject
        "spec-kind-5-2"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        f :: Tbl Int -> Int
        f x = 1
        |]
    , accept
        "spec-kind-5-3"
        [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        f :: Tbl 3 {x = Int} -> Int
        f x = 1
        |]
    , reject
        "spec-kind-4-1"
        [r|
        module main (g)
        newtype Buffer (n :: Nat) a = List a
        f :: Buffer (3 - 10) Int -> Int
        g :: Int -> Int
        g x = x
        |]
    , reject
        "spec-tab-3-1"
        [r|
        module main (oops)
        newtype Table (n :: Nat) (r :: Rec) = Int
        getCol :: f@Str -> Table n r -> [ProjectField r f]
        mk :: Table 2 {x = Int, y = Str}
        oops = getCol "z" mk
        |]
    , accept
        "spec-tab-3-2"
        [r|
        module main (ok)
        newtype Table (n :: Nat) (r :: Rec) = Int
        getCol :: f@Str -> Table n r -> [ProjectField r f]
        mk :: Table 2 {x = Int, y = Str}
        ok = getCol "x" mk
        |]
    , accept
        "spec-kind-4-2"
        [r|
        module main (g)
        newtype Buffer (n :: Nat) a = List a
        f :: Buffer (10 - 3) Int -> Int
        g :: Int -> Int
        g x = x
        |]
    , accept
        "spec-kind-4-3"
        [r|
        module main (ok, empty)
        newtype Vector (n :: Nat) a = List a
        roll :: w@Int -> Vector n Real -> Vector (n - w + 1) Real
        src :: Vector 10 Real
        ok = roll 3 src
        empty = roll 11 src
        |]
    , reject
        "spec-kind-4-4"
        [r|
        module main (bad)
        newtype Vector (n :: Nat) a = List a
        roll :: w@Int -> Vector n Real -> Vector (n - w + 1) Real
        src :: Vector 10 Real
        bad = roll 12 src
        |]
    , accept
        "spec-kind-4-5"
        [r|
        module main (bad)
        newtype Vector (n :: Nat) a = List a
        roll :: w@Int -> Vector n Real -> Vector (n - w + 1) Real
        src :: Vector 10 Real
        bad = roll (-2) src
        |]
    , reject
        "spec-kind-4-6"
        [r|
        module main (g)
        newtype Vector (n :: Nat) a = List a
        f :: Vector (n - (n + 1)) Int -> Int
        g :: Int -> Int
        g x = x
        |]
    , accept
        "spec-kind-4-7"
        [r|
        module main (g)
        newtype Buffer (n :: Nat) a = List a
        f :: Buffer ((3 - 10) + 20) Int -> Int
        g :: Int -> Int
        g x = x
        |]
    , reject
        "spec-kind-4-8"
        [r|
        module main (g)
        newtype Buffer (n :: Nat) a = List a
        f :: Buffer (7 / 0) Int -> Int
        g :: Int -> Int
        g x = x
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

-- | A rejection whose message contains @needle@.
rejectSaying :: String -> MT.Text -> MT.Text -> TestTree
rejectSaying name needle code =
  testCase name $
    runFrontWithState code >>= \case
      (Right _, _) -> assertFailure "expected rejection"
      (Left e, st) | MM.isDefect st e -> assertFailure $ "expected a user error, got a compiler error: " <> show e
      (Left e, _) -> assertBool ("diagnostic lacks " <> MT.unpack needle <> ": " <> show e) (needle `MT.isInfixOf` MT.pack (show e))

appTests :: TestTree
appTests =
  testGroup
    "APP"
    [ acceptTypecheck
        "spec-app-1-1"
        [r|
        module main (go)
        source Py from "lib.py" ("make_adder" as mkAdder)
        mkAdder :: Int -> Int -> Int
        class Mk a where
          mk :: Int -> a
        instance Mk (Int -> Int) where
          mk n = mkAdder n
        go :: Int -> Int
        go x = (mk 1) x
        |]
    , acceptTypecheck
        "spec-app-4-1"
        [r|
        module main (go)
        source Py from "lib.py" ("make_adder" as mkAdder)
        mkAdder :: Int -> Int -> Int
        class Mk a where
          mk :: Int -> a
        instance Mk (Int -> Int) where
          mk n = mkAdder n
        go :: Int -> Int
        go x = mk 1 x
        |]
    , rejectSaying
        "spec-app-4-2"
        "Invalid function application"
        [r|
        module main (go)
        apply1 :: (Int -> a) -> a
        apply1 f = f 1 2
        go :: Int -> Int
        go x = x
        |]
    , rejectSaying
        "spec-app-4-3"
        "Invalid function application"
        [r|
        module main (go)
        effect IO
        source Py from "lib.py" ("later")
        later :: Int -> <IO> a
        go :: Int -> <IO> Int
        go x = later 1 2
        |]
    ]

effTests :: TestTree
effTests =
  testGroup
    "EFF"
    [ rejectSaying
        "spec-eff-13-1"
        "Inescapable effect"
        [r|
        module main (f)
        effect IO
        source Py from "lib.py" ("tick")
        tick :: Int -> <IO> Int
        class Run a where
          run :: a -> Int
        instance Run (Int -> <IO> Int) where
          run g = 1
        f :: Int -> Int
        f x = run tick
        |]
    , accept
        "spec-eff-13-2"
        [r|
        module main (f)
        effect IO
        source Py from "lib.py" ("tick")
        tick :: Int -> <IO> Int
        class Run a where
          run :: a -> <IO> Int
        instance Run (Int -> <IO> Int) where
          run g = g 1
        f :: Int -> <IO> Int
        f x = run tick
        |]
    , rejectSaying
        "spec-eff-13-3"
        "Inescapable effect"
        [r|
        module main (f)
        effect IO
        source Py from "lib.py" ("tick", "run_cb" as runCb)
        tick :: Int -> <IO> Int
        runCb :: (Int -> <IO> Int) -> Int
        f :: Int -> Int
        f x = runCb tick
        |]
    , rejectSaying
        "spec-eff-13-4"
        "Inescapable effect"
        [r|
        module main (f)
        effect IO
        source Py from "lib.py" ("tick")
        tick :: Int -> <IO> Int
        class Run a where
          run :: a -> Int
        instance Run (<IO> Int) where
          run g = 1
        f :: Int -> Int
        f x = run (tick x)
        |]
    , accept
        "spec-eff-13-5"
        [r|
        module main (f)
        effect IO
        source Py from "lib.py" ("tick")
        tick :: Int -> <IO> Int
        class Run a where
          run :: a -> <IO> Int
        instance Run (<IO> Int) where
          run g = g
        f :: Int -> <IO> Int
        f x = run (tick x)
        |]
    , rejectSaying
        "spec-eff-13-6"
        "Inescapable effect"
        [r|
        module main (f)
        effect IO
        source Py from "lib.py" ("tick")
        tick :: Int -> <IO> Int
        class Run a where
          run :: a -> Int
        instance Run (Int -> <IO> Int)
        f :: Int -> Int
        f x = run tick
        |]
    , rejectSaying
        "an undeclared effect in an instance head is rejected"
        "Undeclared effect"
        [r|
        module main (f)
        class Run a where
          run :: a -> Int
        instance Run (Int -> <Bogus> Int) where
          run g = 1
        f :: Int -> Int
        f x = 1
        |]
    ]

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

newtTests :: TestTree
newtTests =
  testGroup
    "NEWT"
    [ rejectUser
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

hostTests :: TestTree
hostTests =
  testGroup
    "HOST"
    [ rejectAtLine
        "spec-host-3-1"
        3
        [r|
        module main (g)
        source Py from "c.py" where
          --' @rsize 2
          shift
        shift :: Real -> [Real] -> [Real]
        g :: Int -> Int
        |]
    , accept
        "spec-host-3-2"
        [r|
        module main (g)
        type Shifter = [Real] -> [Real]
        source Py from "c.py" where
          --' @rsize 1
          shift
        shift :: Real -> Shifter
        g :: Int -> Int
        |]
    , reject
        "spec-host-3-3"
        [r|
        module main (g)
        effect IO
        source Py from "c.py" where
          --' @rsize 1
          mk
        mk :: Int -> <IO> (Int -> Int)
        g :: Int -> Int
        |]
    , reject
        "spec-host-3-4"
        [r|
        module main (g)
        class Shift a where
          shift :: a -> [a] -> [a]
        instance Shift Real where
          source Py from "c.py" where
            --' @rsize 2
            shift
        g :: Int -> Int
        |]
    ]

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

recTests :: TestTree
recTests =
  testGroup
    "REC"
    [ rejectNaming
        "spec-rec-5-2"
        ["y"]
        []
        [r|
        module main (g)
        record R = R {x :: Int, z :: Str}
        f t = (.x t, .y t)
        g :: R -> (Int, Str)
        g r = f r
        |]
    , rejectNaming
        "spec-rec-5-3"
        ["y"]
        []
        [r|
        module main (g)
        record I = I {x :: Int}
        record O = O {a :: I}
        f t = (.a.x t, .a.y t)
        g :: O -> (Int, Int)
        g o = f o
        |]
    , accept
        "spec-rec-5-4"
        [r|
        module main (g)
        record R = R {x :: Int, z :: Str}
        f t = (.x t, .z t)
        g :: R -> (Int, Str)
        g r = f r
        |]
    , accept
        "spec-rec-5-5"
        [r|
        module main (g)
        record P a = P {v :: a, w :: Int}
        f t = .v t
        g :: P Str -> Str
        g p = f p
        |]
    , rejectNaming
        "spec-rec-5-6"
        ["Str", "Int"]
        []
        [r|
        module main (g)
        f t = (.x t, .y t)
        record R = R {x :: Int, y :: Str}
        g :: R -> (Str, Int)
        g r = f r
        |]
    , rejectNaming
        "spec-rec-5-8"
        ["y"]
        []
        [r|
        module main (g)
        record R = R {x :: Int}
        f t = .y t
        g :: Int -> Int
        g n = f {x = n}
        |]
    , accept
        "spec-rec-5-7"
        [r|
        module main (g)
        record R = R {k0 :: Int, k1 :: Int, k2 :: Int, k3 :: Int, k4 :: Int, k5 :: Int, k6 :: Int, k7 :: Int, k8 :: Int, k9 :: Int, k10 :: Int, k11 :: Int, k12 :: Int, k13 :: Int, k14 :: Int, k15 :: Int}
        f t = [.k0 t, .k1 t, .k2 t, .k3 t, .k4 t, .k5 t, .k6 t, .k7 t, .k8 t, .k9 t, .k10 t, .k11 t, .k12 t, .k13 t, .k14 t, .k15 t]
        g :: R -> [Int]
        g r = f r
        |]
    ]

collTests :: TestTree
collTests =
  testGroup
    "COLL"
    [ rejectNaming
        "spec-coll-7-1"
        ["arity"]
        []
        [r|
        module main (g)
        f t = .2 t
        g :: (Int, Str) -> Int
        g p = f p
        |]
    , rejectUser
        "spec-coll-7-2"
        [r|
        module main (g)
        f t = .0 t
        g :: (Int, Str) -> Str
        g p = f p
        |]
    , accept
        "spec-coll-7-3"
        [r|
        module main (g)
        f t = .0 t
        g :: (Int, Str) -> Int
        g p = f p
        |]
    , rejectNaming
        "spec-coll-7-4"
        ["Infinite"]
        []
        [r|
        module main (f)
        f t = [.0 t, t]
        |]
    , rejectUser
        "spec-coll-7-5"
        [r|
        module main (f)
        f t = (.0 t, t 1)
        |]
    , rejectUser
        "spec-coll-7-6"
        [r|
        module main (g)
        h t = (.0 = "a") t
        g :: (Int, Int) -> (Int, Int)
        g p = h p
        |]
    , accept
        "spec-coll-7-7"
        [r|
        module main (g)
        h t = .1 ((.0 = 5) t)
        g :: (Int, Str) -> Str
        g p = h p
        |]
    , accept
        "spec-coll-7-8"
        [r|
        module main (f)
        type Pair a = (a, a)
        f :: Pair Int -> Int
        f p = .0 p
        |]
    , rejectUser
        "spec-coll-7-9"
        [r|
        module main (g)
        f t = (.0 t, .1 t)
        g :: (Int, Str) -> (Str, Int)
        g p = f p
        |]
    , accept
        "spec-coll-7-10"
        [r|
        module main (g)
        f t = [.0 t, .1 t, .2 t, .3 t, .4 t, .5 t, .6 t, .7 t, .8 t, .9 t, .10 t, .11 t, .12 t, .13 t, .14 t, .15 t]
        g :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> [Int]
        g p = f p
        |]
    , rejectNaming
        "spec-coll-7-11"
        ["tuple"]
        []
        [r|
        module main (g)
        f t = .0 t
        g :: [Int] -> Int
        g p = f p
        |]
    , rejectNaming
        "spec-coll-7-12"
        ["arity"]
        []
        [r|
        module main (g)
        f t = .3 t
        g :: Int
        g = f (1, 2, 3)
        |]
    , rejectUser
        "spec-coll-7-13"
        [r|
        module main (g)
        h t = (.0 = "a") t
        g :: (Int, Int) -> (Str, Int)
        g p = h p
        |]
    ]

sumTests :: TestTree
sumTests =
  testGroup
    "SUM"
    [ rejectNaming
        "spec-sum-2-1"
        ["Constructor", "X", "unique"]
        []
        [r|
        data A = X
        data B = X | Y
        f :: B -> Int
        f b = 1
        |]
    , accept
        "spec-sum-2-2"
        [r|
        data A = X
        data B = Z | Y
        f :: B -> Int
        f b = 1
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
    , testCase "spec-sig-5-1" $ do
        -- a generic export is checked, but no instance is chosen for it
        (x, st) <- runFrontWithState sig5Generic
        case x of
          Left e -> assertFailure ("expected acceptance, got: " <> show e)
          Right es -> do
            let chosen = [v | e <- es, AnnoS _ _ (VarS v (Many (_ : _))) <- annoNodes e, v `elem` [EV "lt", EV "le"]]
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

-- | A generic export using a class whose container instances use the class
-- again.
sig5Generic :: MT.Text
sig5Generic =
  [r|
  module main (h)
    class Cmp a where
      lt :: a -> a -> Bool
      le :: a -> a -> Bool
    instance Cmp Int, Cmp Real, Cmp Str, Cmp Bool where
      lt x y = False
      le x y = True
    pick :: Bool -> Int -> Int -> Int
    pick b x y
      ? b = x
      : y
    cmpC :: Cmp a => a -> a -> Int
    cmpC x y = pick (lt x y) 0 (pick (le x y) 1 2)
    instance Cmp a => Cmp [a] where
      lt xs ys = lt (cmpC xs ys) 0
      le xs ys = le (cmpC xs ys) 0
    instance (Cmp a, Cmp b) => Cmp (a, b) where
      lt x y = lt (pick (lt (.0 x) (.0 y)) (cmpC (.1 x) (.1 y)) 0) 0
      le x y = le (pick (le (.0 x) (.0 y)) (cmpC (.1 x) (.1 y)) 0) 0
    h :: Cmp a => a -> a -> Int
    h x y = pick (lt x y) (pick (le x y) 1 2) (pick (lt y x) (pick (le y x) 3 4) 5)
  |]

lexTests :: TestTree
lexTests =
  testGroup
    "LEX"
    [ accept
        "spec-lit-5-2"
        [r|
        module main (a, b, c)
        a :: Str
        a = "it\'s \"quoted\""
        b :: Str
        b = '''it\'s'''
        c :: Str
        c = """say \"hi\" and \'bye\'"""
        |]
    , reject
        "spec-lit-5-1"
        [r|
        module main (g)
        g :: Str
        g = "x""
        |]
    , accept
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
        "spec-lex-4-3"
        [r|
        module main (a, b, c, d, e, f, g, h)
        effect IO
        record R where
          on :: Bool
          n :: Int
        source Py from "f.py" ("bump")
        bump :: Int -> <IO> Int
        a :: Int -> (<IO> Int, Int)
        a x = (do bump x, 1)
        b :: Bool -> <IO> Int
        b t = ? t = do bump 0 : do bump 1
        c :: Int -> <IO> Int
        c x = do { k <- bump x; r <- do bump k; r }
        d :: Int -> <IO> Int
        d x = do bump y where y = x
        e :: Int -> (<IO> R, Int)
        e x = (do {on = True, n = x}, 1)
        f :: Int -> <IO> Int
        f y = let a = do bump y in a
        g :: Bool -> Bool -> <IO> Int
        g s t = ? s = do bump 0 ? t = bump 1 : bump 2
        data T = A | B
        h :: T -> <IO> Int
        h | A = do bump 0 | B = bump 1
        |]
    , accept
        "spec-lex-4-4"
        [r|
        module main (a, b)
        effect IO
        record R where
          on :: Bool
          n :: Int
        source Py from "f.py" ("bump")
        bump :: Int -> <IO> Int
        effect Err
        bumpE :: Int -> <IO, Err> Int
        a :: Int -> <IO, Err> Int
        a x = (do bumpE x :: <IO, Err> Int)
        b :: Bool -> Int -> <IO> Int
        b t x = do
          ? t = do bump x
          : bump 0
        |]
    , accept
        "spec-lex-4-6"
        [r|
        effect IO
        source Py from "f.py" ("bump")
        bump :: Int -> <IO> Int
        (do bump 1, 2)
        |]
    , rejectNaming
        "spec-lex-4-5"
        ["unexpected", "further"]
        []
        [r|
        module main (a)
        effect IO
        record R where
          on :: Bool
          n :: Int
        source Py from "f.py" ("bump")
        bump :: Int -> <IO> Int
        a :: Int -> (<IO> Int, Int)
        a x = (do bump x, , 1)
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
          --' Add two
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
          --' Add two
          ,
          --' Add three
          "g"
          )
        g :: Int -> Int
        h :: Int -> Int
        f :: Int -> Int
        f x = g (h x)
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

-- Passes on any rejection of the program. Its accept twin, which differs only
-- in the rule's subject, shows that the rule is what rejects it.
reject :: String -> MT.Text -> TestTree
reject name code =
  testCase name $
    runFrontWithState code >>= \case
      (Right _, _) -> assertFailure "expected rejection"
      (Left e, st) | MM.isDefect st e -> assertFailure $ "expected a user error, got a compiler error: " <> show e
      (Left _, _) -> return ()

-- | A rejection reported as the user's error, not as a compiler fault.
rejectUser :: String -> MT.Text -> TestTree
rejectUser name code =
  testCase name $
    runFrontWithState code >>= \case
      (Right _, _) -> assertFailure "expected rejection"
      (Left e, st) | MM.isDefect st e -> assertFailure $ "expected a user error, got a compiler error: " <> show e
      (Left e@(EnvFailed _), _) -> assertFailure $ "expected a user error, got an environment failure: " <> show e
      (Left _, _) -> return ()

-- | A rejection whose diagnostic names each type in @named@ and none in
-- @unnamed@, as whole words. Nothing else about its text is checked.
rejectNaming :: String -> [MT.Text] -> [MT.Text] -> MT.Text -> TestTree
rejectNaming name named unnamed code =
  testCase name $
    runFrontWithState code >>= \case
      (Right _, _) -> assertFailure "expected rejection"
      (Left e, st) | MM.isDefect st e -> assertFailure $ "expected a user error, got a compiler error: " <> show e
      (Left err, _) -> do
        let ws = MT.split (\c -> not (DC.isAlphaNum c || c == '_')) (diagnosticText err)
        mapM_ (\n -> assertBool ("diagnostic does not name " <> MT.unpack n) (n `elem` ws)) named
        mapM_ (\n -> assertBool ("diagnostic names " <> MT.unpack n) (n `notElem` ws)) unnamed
  where
    diagnosticText (Rejected _ d) = render d
    diagnosticText (EnvFailed d) = render d
    diagnosticText (Fault _ d) = render d

-- | A rejection reported at the given line of the source (counting the
-- empty line that opens a quasiquote as line 1).
rejectAtLine :: String -> Int -> MT.Text -> TestTree
rejectAtLine name line code =
  testCase name $ do
    (x, st) <- runFrontWithState code
    case x of
      Right _ -> assertFailure "expected rejection"
      Left (Rejected (AtNode i) _) -> fmap srcLocLine (DM.lookup i (stateSourceMap st)) @?= Just line
      Left e -> assertFailure ("expected a located error, got " <> show e)

-- | Acceptance with a warning that contains @needle@.
acceptWarning :: String -> MT.Text -> MT.Text -> TestTree
acceptWarning name needle code =
  testCase name $ do
    config <- emptyConfig
    ((x, msgs), _) <-
      MM.runMorlocMonad Nothing 0 config defaultValue (typecheckFrontend Nothing (Code code))
    case x of
      Left e -> assertFailure $ "expected acceptance, got: " <> show e
      Right _ ->
        assertBool ("no warning mentions " <> MT.unpack needle <> ": " <> show msgs)
          (any (MT.isInfixOf needle) msgs)

runFront :: MT.Text -> IO (Either MorlocError [AnnoS (Indexed TypeU) Many Int])
runFront code = fst <$> runFrontWithState code

runFrontWithState :: MT.Text -> IO (Either MorlocError [AnnoS (Indexed TypeU) Many Int], MorlocState)
runFrontWithState code = do
  config <- emptyConfig
  ((x, _), st) <-
    MM.runMorlocMonad
      Nothing
      0
      config
      defaultValue
      (typecheckFrontend Nothing (Code code) >>= mapM evaluateAnnoSTypes)
  return (x, st)

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
