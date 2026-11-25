{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : PackageTests
-- Description : Spec tests of package identity, origins and the plane
--               layout (model/language/packages.md, planes.md). Installs
--               run against a temporary state; remote repositories are
--               local bare repositories that git reaches in place of
--               github.com through its url.insteadOf setting.
module PackageTests
  ( packageTests
  ) where

import Control.Exception (SomeException, finally, try)
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Morloc.Data.Text as MT
import qualified Morloc
import qualified Morloc.Module as Mod
import Morloc.Frontend.TypeNames (homePath)
import Morloc.Namespace.Prim (MVar (..))
import qualified Morloc.Monad as MM
import Morloc.Namespace.Prim (Defaultable (..))
import Morloc.Namespace.State (Config (..), MorlocError (..), MorlocMonad, MorlocState (..))
import qualified System.Directory as SD
import System.Environment (setEnv)
import System.FilePath ((</>))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (callProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | The fixture repositories, each reached by git in place of its github.com
-- URL. Only these URLs are redirected, so no other fetch is affected; every
-- test sets the same values, so tests may run in parallel.
fixtureRepos :: [(String, String)]
fixtureRepos = [("1org", "x"), ("Casey", "high"), ("casey", "low"), ("debris", "m"), ("forker", "piece"), ("loner", "lone"), ("mallory", "convert"), ("morloc-bio", "oldbio"), ("morloclib", "oldstd"), ("nosrc", "lib1"), ("owner", "thing"), ("partial", "dep"), ("partial", "top"), ("realone", "piece"), ("renamer", "repo-name"), ("sameowner", "dep"), ("sameowner", "top"), ("snapped", "x"), ("user", "app"), ("user2", "app2"), ("user3", "app3"), ("weena", "bits"), ("weena", "bobs")]

fixtureRoot :: IO FilePath
fixtureRoot = do
  tmp <- SD.getTemporaryDirectory
  let root = tmp </> "morloc-package-tests-remotes"
      entries =
        ("protocol.file.allow", "always")
          : [ ("url.file://" <> root </> o </> r <> ".insteadOf", "https://github.com/" <> o <> "/" <> r)
            | (o, r) <- fixtureRepos
            ]
  SD.createDirectoryIfMissing True root
  setEnv "GIT_CONFIG_COUNT" (show (length entries))
  forM_ (zip [0 :: Int ..] entries) $ \(i, (k, v)) -> do
    setEnv ("GIT_CONFIG_KEY_" <> show i) k
    setEnv ("GIT_CONFIG_VALUE_" <> show i) v
  return root

-- | A fresh state, with its plane.
withState :: (Config -> IO a) -> IO a
withState k = do
  tmp <- SD.getTemporaryDirectory
  dir <- createTempDirectory tmp "morloc-package-state"
  let cfg =
        Config
          { configHome = dir
          , configState = dir
          , configLibrary = dir </> "src/morloc/plane"
          , configTmpDir = dir </> "tmp"
          , configBuildConfig = dir </> "build-config.yaml"
          , configLangOverrides = mempty
          }
  r <- k cfg
  SD.removeDirectoryRecursive dir
  return r

run :: Config -> MorlocMonad a -> IO (Either MorlocError a)
run cfg action = do
  ((r, _), _) <- MM.runMorlocMonad Nothing 0 cfg defaultValue action
  return r

-- | Install, as `morloc install` would, swallowing IO exceptions as failures.
install :: Config -> T.Text -> IO (Either String ())
install cfg modstr = do
  r <- try (run cfg (Mod.installModule Mod.DoNotOverwrite Mod.HttpsProtocol Nothing Map.empty modstr))
  return $ case r of
    Left (e :: SomeException) -> Left (show e)
    Right (Left e) -> Left (show e)
    Right (Right ()) -> Right ()

-- | Install with the typecheck `morloc install` runs. Only a rejection is a
-- failure; an IO exception propagates and fails the test.
installChecked :: Config -> T.Text -> IO (Either String ())
installChecked cfg modstr = do
  r <- run cfg (Mod.installModule Mod.DoNotOverwrite Mod.HttpsProtocol (Just Morloc.typecheckModule) Map.empty modstr)
  case r of
    Left e@(Rejected _ _) -> return (Left (show e))
    Left e -> assertFailure ("expected a rejection, got: " <> show e)
    Right () -> return (Right ())

-- | A package directory: package.yaml lines and main.loc text.
writePackage :: FilePath -> [String] -> String -> IO ()
writePackage dir yaml loc = do
  SD.createDirectoryIfMissing True dir
  writeFile (dir </> "package.yaml") (unlines yaml)
  writeFile (dir </> "main.loc") loc

-- | A bare repository at @<root>/<owner>/<repo>@ holding one commit of a
-- package.
remoteRepo :: String -> String -> [String] -> String -> IO ()
remoteRepo owner repo yaml loc = do
  root <- fixtureRoot
  tmp <- SD.getTemporaryDirectory
  work <- createTempDirectory tmp "morloc-package-work"
  writePackage work yaml loc
  let bare = root </> owner </> repo
  SD.removePathForcibly bare
  SD.createDirectoryIfMissing True (root </> owner)
  let git args = callProcess "git" (["-C", work, "-c", "user.name=t", "-c", "user.email=t@t", "-c", "init.defaultBranch=main"] <> args)
  git ["init", "-q"]
  git ["add", "."]
  git ["commit", "-q", "-m", "fixture"]
  callProcess "git" ["clone", "-q", "--bare", work, bare]
  SD.removeDirectoryRecursive work

modLoc :: String -> String
modLoc name = "module " <> name <> " (x)\n\nx :: Int\nx = 1\n"

accepted :: Either String () -> Assertion
accepted = either (\e -> assertFailure ("expected acceptance, got: " <> e)) return

rejected :: String -> Either String () -> Assertion
rejected needle r = case r of
  Right () -> assertFailure "expected rejection"
  Left e -> assertBool ("diagnostic lacks " <> needle <> ": " <> e) (needle `isInfix` e)
  where
    isInfix a b = T.pack a `T.isInfixOf` T.pack b

installedAt :: Config -> FilePath -> Assertion
installedAt cfg rel = do
  ok <- SD.doesFileExist (configLibrary cfg </> rel </> "main.loc")
  assertBool ("not installed at " <> rel) ok

counter :: IORef Int
counter = unsafePerformIO (newIORef 0)
{-# NOINLINE counter #-}

-- | A new, empty directory under @parent@.
createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory parent prefix = do
  n <- atomicModifyIORef' counter (\i -> (i + 1, i))
  t <- getPOSIXTime
  let dir = parent </> (prefix <> "-" <> show (floor (t * 1000000) :: Integer) <> "-" <> show n)
  SD.createDirectory dir
  return dir

packageTests :: TestTree
packageTests =
  testGroup
    "PKG"
    [ testCase "spec-pkg-1-1" . withState $ \cfg -> do
        tmp <- SD.getTemporaryDirectory
        dir <- createTempDirectory tmp "morloc-pkg"
        writePackage dir ["name: nolib"] (modLoc "nolib")
        install cfg (MT.pack dir) >>= rejected "gives no library"
    , testCase "spec-pkg-1-2" . withState $ \cfg -> do
        tmp <- SD.getTemporaryDirectory
        dir <- createTempDirectory tmp "morloc-pkg"
        writePackage dir ["name: withlib", "library: weena"] (modLoc "withlib")
        install cfg (MT.pack dir) >>= accepted
        installedAt cfg "weena/withlib"
    , testCase "spec-pkg-1-3" . withState $ \cfg -> do
        remoteRepo "morloclib" "oldstd" ["name: oldstd"] (modLoc "oldstd")
        install cfg "github:morloclib/oldstd" >>= accepted
        installedAt cfg "std/oldstd"
    , testCase "spec-pkg-2-1" . withState $ \cfg -> do
        tmp <- SD.getTemporaryDirectory
        dir <- createTempDirectory tmp "morloc-pkg"
        writePackage dir ["name: badlib", "library: Wee_na"] (modLoc "badlib")
        install cfg (MT.pack dir) >>= rejected "[a-z][a-z0-9-]*"
    , testCase "spec-pkg-2-2" $
        Mod.parseInstallString "std/root" `seq`
          assertBool "std/x is refused" (either (const True) (const False) (Mod.parseInstallString "std/root"))
    , testCase "spec-pkg-3-1" . withState $ \cfg -> do
        tmp <- SD.getTemporaryDirectory
        dir <- createTempDirectory tmp "morloc-pkg-differently-named"
        writePackage dir ["name: named", "library: weena"] (modLoc "named")
        install cfg (MT.pack dir) >>= accepted
        installedAt cfg "weena/named"
    , testCase "spec-pkg-3-2" . withState $ \cfg -> do
        remoteRepo "renamer" "repo-name" ["name: other-name", "library: renamer"] (modLoc "other-name")
        install cfg "github:renamer/repo-name" >>= rejected "is 'renamer/other-name'"
    , testCase "spec-pkg-3-3" . withState $ \cfg -> do
        tmp <- SD.getTemporaryDirectory
        dir <- createTempDirectory tmp "morloc-pkg"
        SD.createDirectoryIfMissing True dir
        writeFile (dir </> "package.yaml") "name: named\nlibrary: weena\n"
        writeFile (dir </> "named.loc") (modLoc "named")
        install cfg (MT.pack dir) >>= accepted
        found <- run cfg (Mod.findBareModuleMaybe (MV "weena/named"))
        either (assertFailure . show) (assertBool "a <name>.loc entry is imported" . (/= Nothing)) found
    , testCase "spec-pkg-5-1" . withState $ \cfg -> do
        remoteRepo "weena" "bits" ["name: bits", "library: weena"] (modLoc "bits")
        remoteRepo "weena" "bobs" ["name: bobs", "library: weena"] (modLoc "bobs")
        install cfg "weena/bobs" >>= rejected "has no recorded origin"
        install cfg "github:weena/bits" >>= accepted
        installedAt cfg "weena/bits"
        install cfg "weena/bobs" >>= accepted
        installedAt cfg "weena/bobs"
    , testCase "spec-pkg-5-2" . withState $ \cfg -> do
        remoteRepo "owner" "thing" ["name: thing", "library: other"] (modLoc "thing")
        install cfg "github:owner/thing" >>= rejected "morloc library add other"
        _ <- run cfg (Mod.addOrigin "other" (Mod.Origin Mod.RemoteGithub "owner"))
        install cfg "github:owner/thing" >>= accepted
        installedAt cfg "other/thing"
    , testCase "spec-pkg-5-3" . withState $ \cfg -> do
        remoteRepo "mallory" "convert" ["name: convert", "library: bio"] (modLoc "convert")
        install cfg "github:mallory/convert" >>= rejected "morloc library add bio"
        r <- run cfg (Mod.addOrigin "bio" (Mod.Origin Mod.RemoteGithub "mallory"))
        assertBool "a shipped origin cannot be re-recorded" (either (const True) (const False) r)
    , testCase "spec-pkg-5-4" . withState $ \cfg -> do
        writeFile (configState cfg </> "origins.yaml") "std: \"github:mallory\"\n"
        r <- run cfg Mod.loadOrigins
        assertBool "an origins file naming std is refused" (either (const True) (const False) r)
    , testCase "spec-pkg-5-5" . withState $ \cfg -> do
        remoteRepo "casey" "low" ["name: low", "library: casey"] (modLoc "low")
        install cfg "github:casey/low" >>= accepted
        remoteRepo "Casey" "high" ["name: high", "library: casey"] (modLoc "high")
        install cfg "github:Casey/high" >>= accepted
        installedAt cfg "casey/high"
    , testCase "spec-pkg-5-6" . withState $ \cfg -> do
        remoteRepo "sameowner" "dep" ["name: dep", "library: sameowner"] (modLoc "dep")
        remoteRepo "sameowner" "top" ["name: top", "library: sameowner"] "module top (y)\n\nimport sameowner/dep (x)\n\ny :: Int\ny = x\n"
        install cfg "github:sameowner/top" >>= accepted
        installedAt cfg "sameowner/dep"
    , testCase "spec-pkg-2-3" . withState $ \cfg -> do
        remoteRepo "1org" "x" ["name: x", "library: 1org"] (modLoc "x")
        install cfg "github:1org/x" >>= rejected "is not of the form"
        present <- SD.doesDirectoryExist (configLibrary cfg </> "1org")
        assertBool "nothing is installed" (not present)
    , testCase "spec-mod-1-5" . withState $ \cfg -> do
        tmp <- SD.getTemporaryDirectory
        dir <- createTempDirectory tmp "morloc-pkg"
        writePackage dir ["name: sibs", "library: weena"]
          "module helper (h)\n\nh :: Int\nh = 1\n\nmodule sibs (h)\n\nimport .helper (h)\n"
        install cfg (MT.pack dir) >>= accepted
        installedAt cfg "weena/sibs"
    , testCase "spec-pkg-1-4" . withState $ \cfg -> do
        remoteRepo "morloc-bio" "oldbio" ["name: oldbio"] (modLoc "oldbio")
        install cfg "github:morloc-bio/oldbio" >>= accepted
        installedAt cfg "bio/oldbio"
    , testCase "spec-pkg-7-3" . withState $ \cfg -> do
        remoteRepo "snapped" "x" ["name: x", "library: snapped"] (modLoc "x")
        hash <- headHash "snapped" "x"
        SD.createDirectoryIfMissing True (configState cfg </> "snapshots")
        writeFile (configState cfg </> "snapshots" </> "pins.txt") ("snapped/x " <> hash <> " github:snapped/x\n")
        r <- try (run cfg (do
          snap <- Mod.loadSnapshot
          MM.modify (\st -> st {stateSnapshot = snap})
          Mod.installModule Mod.DoNotOverwrite Mod.HttpsProtocol Nothing Map.empty "snapped/x"))
        case r of
          Left (e :: SomeException) -> assertFailure (show e)
          Right (Left e) -> assertFailure (show e)
          Right (Right ()) -> installedAt cfg "snapped/x"
    , testCase "spec-pkg-6-1" . withState $ \cfg -> do
        remoteRepo "loner" "lone" ["name: lone", "library: loner"] (modLoc "lone")
        hash <- headHash "loner" "lone"
        remoteRepo "user" "app" (["name: app", "library: user", "morloc-deps:", "  - name: lone", "    library: loner", "    source: github:loner/lone", "    git-hash: \"" <> hash <> "\""]) "module app (y)\n\nimport loner/lone (x)\n\ny :: Int\ny = x\n"
        install cfg "github:user/app" >>= accepted
        installedAt cfg "loner/lone"
        origins <- run cfg Mod.loadOrigins
        assertBool "a dependency's source records no origin" (either (const False) (Map.notMember "loner") origins)
    , testCase "spec-pkg-6-2" . withState $ \cfg -> do
        remoteRepo "realone" "piece" ["name: piece", "library: realone"] (modLoc "piece")
        remoteRepo "forker" "piece" ["name: piece", "library: realone"] (modLoc "piece")
        hash <- headHash "forker" "piece"
        install cfg "github:realone/piece" >>= accepted
        remoteRepo "user2" "app2" (["name: app2", "library: user2", "morloc-deps:", "  - name: piece", "    library: realone", "    source: github:forker/piece", "    git-hash: \"" <> hash <> "\""]) "module app2 (y)\n\nimport realone/piece (x)\n\ny :: Int\ny = x\n"
        _ <- run cfg (Mod.addOrigin "user2" (Mod.Origin Mod.RemoteGithub "user2"))
        _ <- uninstallDir cfg "realone/piece"
        install cfg "user2/app2" >>= rejected "comes from github:realone"
    , testCase "spec-pkg-6-3" . withState $ \cfg -> do
        remoteRepo "nosrc" "lib1" ["name: lib1", "library: nosrc"] (modLoc "lib1")
        remoteRepo "user3" "app3" ["name: app3", "library: user3"] "module app3 (y)\n\nimport nosrc/lib1 (x)\n\ny :: Int\ny = x\n"
        install cfg "github:user3/app3" >>= rejected "morloc library add nosrc"
    , testCase "spec-pkg-7-1" $
        Mod.parseSnapshotLine "weena/bits 3ce5 github:weena/bits"
          @?= Just (Right ("weena/bits", "3ce5", Just "github:weena/bits"))
    , testCase "spec-pkg-7-2" $
        assertBool "a line of an unshipped library needs a source" (isLeftJust (Mod.parseSnapshotLine "weena/bits 3ce5"))
    , testCase "spec-pkg-8-1" $ do
        tmp <- SD.getTemporaryDirectory
        dir <- createTempDirectory tmp "morloc-pkg-deps"
        SD.createDirectoryIfMissing True (dir </> "lib")
        writeFile (dir </> "main.loc") "module p (x)\nimport .lib.inner (x)\nimport root\n"
        writeFile (dir </> "lib/inner.loc") "module inner (x)\nimport weena/bits.sub (y)\nimport .lib.deeper\n"
        writeFile (dir </> "lib/deeper.loc") "module deeper ()\nimport bio/convert\n"
        deps <- Mod.extractMorlocDeps (dir </> "main.loc")
        SD.removeDirectoryRecursive dir
        deps @?= ["root", "weena/bits", "bio/convert"]
    , testCase "spec-gen-4-1" $ do
        homePath (Just "root-py") (MV "root-py") @?= ["root_py"]
        homePath (Just "root-py") (MV "root-py.lib.x") @?= ["root_py", "lib", "x"]
        homePath (Just "root-py") (MV ".lib.x") @?= ["root_py", "lib", "x"]
    , testCase "spec-gen-4-2" $ do
        homePath (Just "bio/convert") (MV "bio/convert") @?= ["_lib", "bio", "convert"]
        homePath (Just "bio/convert") (MV ".lib.x") @?= ["_lib", "bio", "convert", "lib", "x"]
        homePath (Just "bio/convert") (MV "bio/convert.lib.x") @?= ["_lib", "bio", "convert", "lib", "x"]
        homePath (Just "weena/convert") (MV "weena/convert") @?= ["_lib", "weena", "convert"]
    , testCase "spec-gen-4-4" $
        homePath (Just "weena/foo") (MV "foo") @?= ["_lib", "weena", "foo"]
    , testCase "spec-gen-4-3" $
        homePath Nothing (MV ".lib.x") @?= ["_local", "lib", "x"]
    , testCase "spec-plane-1-1" . withState $ \cfg -> do
        SD.createDirectoryIfMissing True (configLibrary cfg </> "root")
        refused cfg "older morloc"
    , testCase "spec-plane-1-2" . withState $ \cfg -> do
        Mod.initPlaneLayout cfg
        SD.createDirectoryIfMissing True (configLibrary cfg </> "std" </> "root")
        Mod.checkPlaneLayout cfg
    , testCase "spec-plane-1-3" . withState $ \cfg -> do
        writeFile (configState cfg </> "LAYOUT") "layout 1\n"
        refused cfg "older morloc"
        writeFile (configState cfg </> "LAYOUT") "layout 99\n"
        refused cfg "newer morloc"
        r <- try (Mod.initPlaneLayout cfg)
        assertBool "a newer layout file is never rewritten" (either (\(_ :: SomeException) -> True) (const False) r)
        txt <- readFile (configState cfg </> "LAYOUT")
        length txt `seq` txt @?= "layout 99\n"
    , testCase "spec-pkg-11-1" . withState $ \cfg -> do
        remoteRepo "partial" "dep" ["name: dep", "library: partial"] (modLoc "dep")
        remoteRepo "partial" "top" ["name: top", "library: partial", "setup: missing.sh"] "module top (y)\n\nimport partial/dep (x)\n\ny :: Int\ny = x\n"
        install cfg "github:partial/top" >>= rejected "setup script not found"
        topDir <- SD.doesDirectoryExist (configLibrary cfg </> "partial" </> "top")
        assertBool "a failed install leaves no module" (not topDir)
        installedAt cfg "partial/dep"
        origins <- run cfg Mod.loadOrigins
        assertBool "a failed install records no origin" (either (const False) (Map.notMember "partial") origins)
    , testCase "install of a generic export does not warn" . withState $ \cfg -> do
        tmp <- SD.getTemporaryDirectory
        dir <- createTempDirectory tmp "morloc-pkg"
        writePackage dir ["name: gen", "library: weena"] "module gen (*)\n\nident :: a -> a\nident x = x\n"
        flip finally (SD.removeDirectoryRecursive dir) $ do
          ((r, warns), _) <-
            MM.runMorlocMonad Nothing 0 cfg defaultValue
              (Mod.installModule Mod.DoNotOverwrite Mod.HttpsProtocol (Just Morloc.typecheckModule) Map.empty (MT.pack dir))
          either (\e -> assertFailure ("install failed: " <> show e)) return r
          assertEqual "warnings" [] warns
    , testCase "spec-pkg-11-3" . withState $ \cfg -> do
        tmp <- SD.getTemporaryDirectory
        forM_
          [ "module undecl (f)\n\nf :: NoSuchType -> Int\n"
          , "module undecl (f)\n\nf :: Int -> Int\nf x = x 1\n"
          ] $ \loc -> do
            dir <- createTempDirectory tmp "morloc-pkg"
            writePackage dir ["name: undecl", "library: weena"] loc
            flip finally (SD.removeDirectoryRecursive dir) $ do
             r <- installChecked cfg (MT.pack dir)
             rejected "weena/undecl" r
             rejected "main.loc" r
             either (\e -> assertBool ("reported as a defect: " <> e) (not ("Compiler bug" `T.isInfixOf` T.pack e))) return r
             present <- SD.doesDirectoryExist (configLibrary cfg </> "weena" </> "undecl")
             assertBool "a module failing typecheck is not installed" (not present)
    , testCase "spec-diag-1-2" . withState $ \cfg -> do
        tmp <- SD.getTemporaryDirectory
        forM_ [["dependencies: []"], ["cpp-deps: 3"], ["include: ["]] $ \extra -> do
          dir <- createTempDirectory tmp "morloc-pkg"
          flip finally (SD.removeDirectoryRecursive dir) $ do
           writePackage dir (["name: yamlbad", "library: weena"] <> extra) (modLoc "yamlbad")
           r <- installChecked cfg (MT.pack dir)
           rejected (dir </> "package.yaml") r
           either (\e -> assertBool ("a Haskell error escaped: " <> e) (not (any (`T.isInfixOf` T.pack e) ["CallStack", "Exception"]))) return r
           meta <- run cfg (Mod.loadModuleMetadata True (dir </> "main.loc"))
           either (\e -> assertBool ("make names the file: " <> show e) (T.pack (dir </> "package.yaml") `T.isInfixOf` T.pack (show e))) (const (assertFailure "make accepted a bad package.yaml")) meta
    , testCase "spec-pkg-11-2" . withState $ \cfg -> do
        remoteRepo "debris" "m" ["name: m", "library: debris"] (modLoc "m")
        Mod.initPlaneLayout cfg
        SD.createDirectoryIfMissing True (configLibrary cfg </> "debris" </> "m")
        install cfg "github:debris/m" >>= accepted
        recorded <- SD.doesFileExist (configState cfg </> "fdb" </> "debris.m.module")
        assertBool "a directory without a record is reinstalled" recorded
    ]
  where
    isLeftJust (Just (Left _)) = True
    isLeftJust _ = False
    headHash owner repo = do
      root <- fixtureRoot
      takeWhile (/= '\n') <$> readGit ["-C", root </> owner </> repo, "rev-parse", "HEAD"]
    refused cfg needle = do
      r <- try (Mod.checkPlaneLayout cfg)
      case r of
        Left (e :: SomeException) -> assertBool ("diagnostic lacks " <> needle <> ": " <> show e) (T.pack needle `T.isInfixOf` T.pack (show e))
        Right () -> assertFailure "expected the state to be refused"
    readGit args = do
      tmp <- SD.getTemporaryDirectory
      out <- createTempDirectory tmp "morloc-git-out"
      callProcess "sh" ["-c", "git " <> unwords (map quote args) <> " > " <> quote (out </> "o")]
      s <- readFile (out </> "o")
      length s `seq` SD.removeDirectoryRecursive out
      return s
    quote s = "'" <> s <> "'"
    uninstallDir cfg rel = do
      SD.removePathForcibly (configLibrary cfg </> rel)
      forM_ ["module", "lock"] $ \ext ->
        SD.removePathForcibly (configState cfg </> "fdb" </> map (\c -> if c == '/' then '.' else c) rel <> "." <> ext)
