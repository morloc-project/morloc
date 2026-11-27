{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Common where

import Morloc (typecheck, typecheckFrontend)
import Morloc.Frontend.Namespace
import Morloc.Data.Doc (render)
import qualified Data.Char as DC
import Morloc.Frontend.Typecheck (evaluateAnnoSTypes)
import Morloc.Frontend.Instances (ResolveError (..), defaultPolicy, instanceDecls, resolve)
import qualified Morloc.TypeEval as TE
import qualified Morloc.Monad as MM
import qualified Data.Text as MT
import qualified System.Directory as SD
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Text.RawString.QQ
import qualified Data.Map.Strict as DM

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
    MM.runMorlocMonad Nothing 0 config {configLibrary = root <> "/library", configState = root} defaultValue
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

-- | A rejection whose message contains @needle@.
rejectSaying :: String -> MT.Text -> MT.Text -> TestTree
rejectSaying name needle code =
  testCase name $
    runFrontWithState code >>= \case
      (Right _, _) -> assertFailure "expected rejection"
      (Left e, st) | MM.isDefect st e -> assertFailure $ "expected a user error, got a compiler error: " <> show e
      (Left e, _) -> assertBool ("diagnostic lacks " <> MT.unpack needle <> ": " <> show e) (needle `MT.isInfixOf` MT.pack (show e))

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
      , configTmpDir = home <> "/.morloc/tmp"
      , configBuildConfig = home <> "/.morloc/.build-config.yaml"
      , configLangOverrides = mempty
      }

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
