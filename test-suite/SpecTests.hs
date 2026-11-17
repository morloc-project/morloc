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
import Morloc.Frontend.Typecheck (evaluateAnnoSTypes, resolveTypes)
import Morloc.CodeGenerator.Serial (generalTypeToSerialAST, serialAstToMsgpackSchema)
import Control.Monad (forM, unless)
import Morloc.Frontend.Instances (ResolveError (..), defaultPolicy, instanceDecls, resolve)
import qualified Morloc.TypeEval as TE
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
    , localOption (mkTimeout 2000000) cls5Tests
    , localOption (mkTimeout 2000000) cls16Tests
    , gen14Tests
    , localOption (mkTimeout 2000000) cls17Tests
    , localOption (mkTimeout 2000000) cls18Tests
    , localOption (mkTimeout 5000000) cls2OwnInstanceTests
    , localOption (mkTimeout 2000000) cls20Tests
    , localOption (mkTimeout 5000000) cls23Tests
    , localOption (mkTimeout 5000000) cls5ChainTests
    , localOption (mkTimeout 5000000) poly1Tests
    , localOption (mkTimeout 5000000) poly7Tests
    , localOption (mkTimeout 5000000) poly8Tests
    , localOption (mkTimeout 5000000) checkOnceTests
    , localOption (mkTimeout 20000000) patternConformanceTests
    , localOption (mkTimeout 2000000) bindTests
    , localOption (mkTimeout 2000000) hostTests
    , localOption (mkTimeout 2000000) effTests
    , localOption (mkTimeout 2000000) appTests
    , localOption (mkTimeout 2000000) cls3Tests
    , localOption (mkTimeout 2000000) diagTests
    , localOption (mkTimeout 2000000) intrTests
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
    , accept
        "spec-cls-2-7"
        [r|
        module main (combine)
        class Cat a where
          cat :: a -> a -> a
        record Foo = Bar { n :: Str }
        instance Cat Foo where
          cat a b = b
        record Scan where
          q :: Foo
        combine :: Foo -> Foo -> Scan
        combine x y = {q = cat x y}
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
          import lib
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
          import liba
          import libb
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
          import lib as l
          import lib (one)
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

intrTests :: TestTree
intrTests =
  testGroup
    "INTR"
    [ rejectSaying
        "spec-intr-9-1"
        "non-functional"
        [r|
        module main (f)
        f :: Int -> Try Str Int
        f x = @try x 1
        |]
    , accept
        "spec-intr-9-5"
        [r|
        module main (f)
        f :: Int -> Try Str Int
        f x = @try x
        |]
    , rejectSaying
        "spec-intr-9-2"
        "non-functional"
        [r|
        module main (f)
        effect IO
        f :: OStream Int -> [Int] -> <IO> (Try Str ())
        f o xs = @write 3 o xs 7
        |]
    , accept
        "spec-intr-9-6"
        [r|
        module main (f)
        effect IO
        f :: OStream Int -> [Int] -> <IO> (Try Str ())
        f o xs = @write 3 o xs
        |]
    , acceptTypecheck
        "spec-intr-9-3"
        [r|
        module main (f)
        f :: Int -> Int
        f x = @throw "boom" x
        |]
    , rejectSaying
        "spec-intr-9-4"
        "non-functional"
        [r|
        module main (f)
        f :: Int -> Str
        f x = @version x
        |]
    ]

diagTests :: TestTree
diagTests =
  testGroup
    "DIAG"
    [ rejectSaying
        "spec-diag-1-1"
        "inside an instance of"
        [r|
        module main (bar)
        class Foo a where
          foo :: a -> a
        instance Foo Int where
          foo :: Int -> Int
          foo x = x
        bar :: Int -> Int
        bar x = foo x
        |]
    ]

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
    , rejectSaying
        "spec-app-1-4"
        "POLY-7"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        g :: V 3 -> Int
        run :: Int -> (V n -> Int)
        run k x = g x
        |]
    , accept
        "spec-app-1-5"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: Int -> (V n -> Int)
        run :: Int -> V 3 -> Int
        run k xs = f k xs
        |]
    , accept
        "spec-app-1-6"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        type F (n :: Nat) = V n -> Int
        f :: Int -> F n
        run :: Int -> V 3 -> Int
        run k xs = f k xs
        |]
    , rejectSaying
        "spec-app-1-7"
        "POLY-7"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        type F (n :: Nat) = V n -> Int
        g :: V 3 -> Int
        run :: Int -> F n
        run k x = g x
        |]
    , accept
        "spec-app-1-8"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        g :: V n -> Int
        run :: Int -> (V n -> Int)
        run k x = g x
        |]
    , accept
        "spec-app-1-9"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: Int -> (n@Int -> V n)
        run :: Int -> V 3
        run k = f k 3
        |]
    , rejectSaying
        "spec-app-1-10"
        "binds nothing"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: (n@Int -> V n) -> Int
        run :: Int -> Int
        run k = 1
        |]
    , accept
        "spec-app-1-11"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: Int -> (n@Int -> (m@Int -> V m))
        run :: Int -> V 4
        run k = f k 3 4
        |]
    , rejectSaying
        "spec-app-1-12"
        "binds nothing"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: ?n@Int -> V n
        run :: Int -> Int
        run k = 1
        |]
    , rejectSaying
        "spec-app-1-13"
        "binds nothing"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: <IO> n@Int -> V n
        run :: Int -> Int
        run k = 1
        |]
    , rejectSaying
        "spec-app-1-14"
        "given twice"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: n@Int -> n@Int -> V n
        run :: Int -> Int
        run k = 1
        |]
    , rejectSaying
        "spec-app-1-15"
        "binds nothing"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        f :: Eq (n@Int) => n@Int -> V n
        run :: Int -> Int
        run k = 1
        |]
    , rejectSaying
        "spec-app-1-16"
        "binds nothing"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        type W = n@Int
        run :: Int -> Int
        run k = 1
        |]
    , rejectSaying
        "spec-app-1-17"
        "srcLocLine = 5, srcLocCol = 24"
        [r|
        module main (run)
        newtype V (n :: Nat) = Int
        g :: m@Int -> V m
        h :: Int -> V (m@Int)
        run :: Int -> Int
        run k = 1
        |]
    , rejectSaying
        "spec-app-1-18"
        "binds nothing"
        [r|
        module main (x)
        newtype V (n :: Nat) = Int
        x :: n@Int
        x = 3
        |]
    , rejectSaying
        "spec-app-1-19"
        "binds nothing"
        [r|
        module main (f)
        newtype V (n :: Nat) = Int
        f :: Int -> (Int -> n@Int)
        |]
    , rejectSaying
        "spec-app-1-20"
        "binds nothing"
        [r|
        module main (f)
        import root-py
        type Py => W = n@Int
        f :: Int -> Int
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
    [ rejectSaying
        "spec-sig-1-1"
        "Type mismatch"
        [r|
        module lib (bad)
          bad :: a -> a
          bad x = 5

        module main (useIt)
          import lib (bad)
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
          import lib (good)
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
          import sigs (bad)
          bad x = 5

        module main (useIt)
          import impl (bad)
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
          import lib (good)
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
          import lib (Shift)
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
        "spec-sig-8-6"
        [r|
        module lib (foo)
          foo :: Int -> Int
        module main (bar)
          import lib (foo)
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

-- | Generic exports whose bodies, directly and through a signed term, apply
-- the class to a literal, and a concrete use of one.
sig5Literal :: MT.Text
sig5Literal =
  [r|
  module main (f, h, g)
    class Integral a where
      add :: a -> a -> a
    instance Integral Int, Integral Real where
      add x y = x
    f :: Integral a => a -> a
    f x = add x 1
    h :: Integral a => a -> a
    h x = f x
    g :: Real -> Real
    g y = f y
  |]

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

gen14Tests :: TestTree
gen14Tests =
  testGroup
    "GEN-14"
    [ reject "spec-gen-14-5" [r|
        module main (P)
        record P where
          x :: Int
        record Py => P = "struct"
        |]
    , accept "spec-gen-14-6" [r|
        module main (P)
        record P where
          x :: Int
        record Py => P = "OrderedDict"
        |]
    , reject "spec-gen-14-7" [r|
        module main (O)
        object O where
          x :: Int
        object Py => O = "struct"
        |]
    , accept "spec-gen-14-8" [r|
        module main (O)
        object O where
          x :: Int
        object Py => O = "Obj"
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

-- | The table Link builds, read by the resolver: one entry per instance,
-- methodless ones included, with its context.
instanceTableTest :: TestTree
instanceTableTest =
  testCase "instance table built by Link" $ do
    (_, st) <-
      runFrontWithState
        [r|
        module main (f)
        class Marker a
        class Sz a where
          sz :: a -> Str
        record W a where
          val :: a
        record Foo = Bar { n :: Str }
        class Cat a where
          cat :: a -> a -> a
        instance Marker Str
        instance Sz Str where
          sz x = "str"
        instance Sz a => Sz (W a) where
          sz w = "w"
        instance Cat Foo where
          cat a b = b
        f :: Str -> Str
        f x = x
        |]
    let env = stateInstances st
        scope = stateGeneralTypedefs st
        run c ts = resolve defaultPolicy scope env [] (Constraint (ClassName c) ts)
        keyName (Right (EvInstance k _ _)) = Just (snd (ikHead k))
        keyName _ = Nothing
        w t = AppU (VarU (TV "W")) [t]
        str = VarU (TV "Str")
        foo = either (const (VarU (TV "Foo"))) id (TE.evaluateType scope (VarU (TV "Foo")))
    length (instanceDecls env (ClassName "Marker")) @?= 1
    map idContext [d | d <- instanceDecls env (ClassName "Sz"), not (null (idContext d))]
      @?= [[Constraint (ClassName "Sz") [VarU (TV "#0")]]]
    keyName (run "Sz" [w str]) @?= Just [w (VarU (TV "#0"))]
    (case run "Sz" [w (VarU (TV "Bool"))] of Left (ContextUnmet {}) -> True; _ -> False) @?= True
    (case foo of NamU {} -> True; _ -> False) @?= True
    keyName (run "Cat" [foo]) @?= Just [VarU (TV "Foo")]

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
            "module lib (getFst)\n  getFst = .0\n\nmodule main (getFst, f)\n  import lib (getFst)\n  f :: (Int, Str) -> Int\n  f p = getFst p\n"
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
