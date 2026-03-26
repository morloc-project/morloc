{- |
Module      : Main (test suite)
Description : Test entry point for morloc-manager
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Collects all test groups and runs them via the tasty framework.
-}

module Main (main) where

import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import MorlocManager.Types
import MorlocManager.Config (readConfig, writeConfig, readFlagsFile)
import MorlocManager.Container
  ( RunConfig(..), defaultRunConfig, BuildConfig(..)
  , engineExecutable, engineSpecificRunFlags, buildRunArgs, buildBuildArgs
  )
import MorlocManager.SELinux (isSafeToRelabel)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "morloc-manager"
  [ testGroup "Types" typeTests
  , testGroup "Error messages" errorTests
  , testGroup "Config defaults" configDefaultTests
  , testGroup "Config JSON" configJsonTests
  , testGroup "Config flags" configFlagsTests
  , testGroup "Container CLI" containerTests
  , testGroup "SELinux" selinuxTests
  ]

-- ======================================================================
-- Type tests
-- ======================================================================

typeTests :: [TestTree]
typeTests =
  [ testCase "showVersion formats correctly" $
      showVersion (Version 0 67 0) @?= "0.67.0"

  , testCase "parseVersion round-trips" $
      parseVersion "0.67.0" @?= Just (Version 0 67 0)

  , testCase "parseVersion rejects invalid" $
      parseVersion "abc" @?= Nothing

  , testCase "parseVersion rejects incomplete" $
      parseVersion "0.67" @?= Nothing

  , testProperty "parseVersion . showVersion == Just" $
      \(NonNegative ma) (NonNegative mi) (NonNegative pa) ->
        let v = Version ma mi pa
        in parseVersion (show ma <> "." <> show mi <> "." <> show pa) == Just v

  , testCase "Version ordering is semantic" $
      Version 1 0 0 > Version 0 99 99 @? "major takes precedence"

  , testCase "Version ordering: minor component" $
      Version 0 2 0 > Version 0 1 99 @? "minor takes precedence over patch"

  , testCase "Version equality" $
      Version 0 67 0 == Version 0 67 0 @? "same version is equal"
  ]

-- ======================================================================
-- Error message tests
-- ======================================================================

errorTests :: [TestTree]
errorTests =
  [ testCase "InvalidVersion renders correctly" $
      assertBool "contains 'Invalid version'" $
        "Invalid version" `Text.isInfixOf` renderError (InvalidVersion "abc")

  , testCase "NoCommand renders correctly" $
      assertBool "contains 'No command'" $
        "No command" `Text.isInfixOf` renderError NoCommand

  , testCase "NoActiveVersion suggests select not install" $
      assertBool "contains 'select'" $
        "select" `Text.isInfixOf` renderError NoActiveVersion

  , testCase "VersionNotInstalled suggests info" $
      assertBool "contains 'info'" $
        "info" `Text.isInfixOf` renderError (VersionNotInstalled (Version 0 99 0))

  , testCase "ConfigPermissionDenied mentions permissions" $
      assertBool "contains 'Permission'" $
        "Permission" `Text.isInfixOf` renderError (ConfigPermissionDenied "/etc/morloc/config.json")

  , testCase "InstallError renders correctly" $
      assertBool "contains 'Install failed'" $
        "Install failed" `Text.isInfixOf` renderError (InstallError "test error")

  , testCase "FreezeError only used for freeze operations" $
      assertBool "contains 'Freeze failed'" $
        "Freeze failed" `Text.isInfixOf` renderError (FreezeError "tar error")
  ]

-- ======================================================================
-- Config default tests
-- ======================================================================

configDefaultTests :: [TestTree]
configDefaultTests =
  [ testCase "defaultConfig has no active version" $
      configActiveVersion defaultConfig @?= Nothing

  , testCase "defaultConfig uses Podman" $
      configEngine defaultConfig @?= Podman

  , testCase "defaultConfig uses local scope" $
      configActiveScope defaultConfig @?= Local

  , testCase "defaultConfig uses base environment" $
      configActiveEnv defaultConfig @?= "base"
  ]

-- ======================================================================
-- Config JSON round-trip tests
-- ======================================================================

configJsonTests :: [TestTree]
configJsonTests =
  [ testCase "Config JSON round-trip" $ withSystemTempDirectory "mm-test" $ \dir -> do
      let path = dir </> "config.json"
          cfg = Config
            { configActiveVersion = Just (Version 0 67 0)
            , configActiveScope = Local
            , configActiveEnv = "ml"
            , configEngine = Docker
            }
      result <- writeConfig path cfg
      result @?= Right ()
      readBack <- readConfig path :: IO (Either ManagerError Config)
      case readBack of
        Left err -> assertFailure ("Read failed: " <> show err)
        Right cfg' -> do
          configActiveVersion cfg' @?= Just (Version 0 67 0)
          configActiveEnv cfg' @?= "ml"
          configEngine cfg' @?= Docker

  , testCase "Config read missing file returns ConfigNotFound" $
      withSystemTempDirectory "mm-test" $ \dir -> do
        result <- readConfig (dir </> "nonexistent.json") :: IO (Either ManagerError Config)
        case result of
          Left (ConfigNotFound _) -> pure ()
          other -> assertFailure ("Expected ConfigNotFound, got: " <> show other)

  , testCase "Config read invalid JSON returns ConfigParseError" $
      withSystemTempDirectory "mm-test" $ \dir -> do
        let path = dir </> "bad.json"
        writeFile path "not json at all"
        result <- readConfig path :: IO (Either ManagerError Config)
        case result of
          Left (ConfigParseError _ _) -> pure ()
          other -> assertFailure ("Expected ConfigParseError, got: " <> show other)

  , testCase "VersionConfig JSON round-trip" $ withSystemTempDirectory "mm-test" $ \dir -> do
      let path = dir </> "vc.json"
          vc = VersionConfig
            { vcImage = "ghcr.io/morloc-project/morloc/morloc-full:0.67.0"
            , vcHostDir = "/home/user/.local/share/morloc/versions/0.67.0"
            , vcShmSize = "1g"
            , vcEngine = Podman
            }
      _ <- writeConfig path vc
      readBack <- readConfig path :: IO (Either ManagerError VersionConfig)
      case readBack of
        Left err -> assertFailure ("Read failed: " <> show err)
        Right vc' -> do
          vcImage vc' @?= "ghcr.io/morloc-project/morloc/morloc-full:0.67.0"
          vcShmSize vc' @?= "1g"

  , testCase "FreezeManifest JSON round-trip" $ withSystemTempDirectory "mm-test" $ \dir -> do
      let path = dir </> "fm.json"
          fm = FreezeManifest
            { fmMorlocVersion = Version 0 67 0
            , fmFrozenAt = read "2026-03-24 15:00:00 UTC"
            , fmModules = [ModuleEntry "math" (Just "0.3.0") "abc123"]
            , fmPrograms = [ProgramEntry "svc" ["hello", "compute"]]
            , fmBaseImage = "morloc-full:0.67.0"
            , fmEnvLayer = Just "ml"
            }
      _ <- writeConfig path fm
      readBack <- readConfig path :: IO (Either ManagerError FreezeManifest)
      case readBack of
        Left err -> assertFailure ("Read failed: " <> show err)
        Right fm' -> do
          fmMorlocVersion fm' @?= Version 0 67 0
          length (fmModules fm') @?= 1
          length (fmPrograms fm') @?= 1
          peCommands (head (fmPrograms fm')) @?= ["hello", "compute"]
  ]

-- ======================================================================
-- Config flags tests
-- ======================================================================

configFlagsTests :: [TestTree]
configFlagsTests =
  [ testCase "readFlagsFile parses lines, skips comments and blanks" $
      withSystemTempDirectory "mm-test" $ \dir -> do
        let path = dir </> "test.flags"
        writeFile path $ unlines
          [ "# This is a comment"
          , "--gpus all"
          , ""
          , "  -v /data:/data  "
          , "# another comment"
          , "--network host"
          ]
        flags <- readFlagsFile path
        flags @?= ["--gpus all", "-v /data:/data", "--network host"]

  , testCase "readFlagsFile returns empty for missing file" $
      withSystemTempDirectory "mm-test" $ \dir -> do
        flags <- readFlagsFile (dir </> "nope.flags")
        flags @?= []
  ]

-- ======================================================================
-- Container CLI argument construction tests
-- ======================================================================

containerTests :: [TestTree]
containerTests =
  [ testCase "engineExecutable Docker" $
      engineExecutable Docker @?= "docker"

  , testCase "engineExecutable Podman" $
      engineExecutable Podman @?= "podman"

  , testCase "buildRunArgs minimal config" $ do
      let cfg = defaultRunConfig "myimage:latest"
          args = buildRunArgs Docker (engineSpecificRunFlags Docker) cfg
      -- Should have: run, --rm, image
      head args @?= "run"
      assertBool "--rm present" ("--rm" `elem` args)
      assertBool "image present" ("myimage:latest" `elem` args)
      assertBool "no -it" (not ("-it" `elem` args))

  , testCase "buildRunArgs Podman adds --userns=keep-id" $ do
      let cfg = defaultRunConfig "myimage:latest"
          args = buildRunArgs Podman (engineSpecificRunFlags Podman) cfg
      assertBool "--userns=keep-id present" ("--userns=keep-id" `elem` args)

  , testCase "buildRunArgs interactive mode adds -it" $ do
      let cfg = (defaultRunConfig "img") { rcInteractive = True }
          args = buildRunArgs Docker (engineSpecificRunFlags Docker) cfg
      assertBool "-it present" ("-it" `elem` args)

  , testCase "buildRunArgs SELinux suffix on mounts" $ do
      let cfg = (defaultRunConfig "img")
            { rcBindMounts = [("/host", "/container")]
            , rcSELinuxSuffix = ":z"
            }
          args = buildRunArgs Docker (engineSpecificRunFlags Docker) cfg
      assertBool ":z suffix on mount" (any (== "-v") args &&
        any (\a -> a == "/host:/container:z") args)

  , testCase "buildRunArgs workdir" $ do
      let cfg = (defaultRunConfig "img") { rcWorkDir = Just "/work" }
          args = buildRunArgs Docker (engineSpecificRunFlags Docker) cfg
      assertBool "-w present" ("-w" `elem` args)
      assertBool "workdir value" ("/work" `elem` args)

  , testCase "buildRunArgs read-only" $ do
      let cfg = (defaultRunConfig "img") { rcReadOnly = True }
          args = buildRunArgs Docker (engineSpecificRunFlags Docker) cfg
      assertBool "--read-only present" ("--read-only" `elem` args)

  , testCase "buildRunArgs command at end" $ do
      let cfg = (defaultRunConfig "img")
            { rcCommand = Just ["morloc", "make", "-o", "svc", "svc.loc"] }
          args = buildRunArgs Docker (engineSpecificRunFlags Docker) cfg
      -- Image should come before command
      let imgIdx = length (takeWhile (/= "img") args)
          cmdIdx = length (takeWhile (/= "morloc") args)
      assertBool "image before command" (imgIdx < cmdIdx)

  , testCase "buildBuildArgs includes tag and dockerfile" $ do
      let cfg = BuildConfig
            { bcDockerfile = "/tmp/Dockerfile"
            , bcContext = "/tmp/ctx"
            , bcTag = "test:v1"
            , bcBuildArgs = [("BASE", "ubuntu:22.04")]
            }
          args = buildBuildArgs cfg
      head args @?= "build"
      assertBool "-f present" ("-f" `elem` args)
      assertBool "tag present" ("-t" `elem` args)
      assertBool "build-arg present" ("--build-arg" `elem` args)
      assertBool "context at end" (last args == "/tmp/ctx")
  ]

-- ======================================================================
-- SELinux tests
-- ======================================================================

selinuxTests :: [TestTree]
selinuxTests =
  [ testCase "/ is unsafe to relabel" $ do
      safe <- isSafeToRelabel "/"
      assertBool "/ should be unsafe" (not safe)

  , testCase "/tmp is unsafe to relabel" $ do
      safe <- isSafeToRelabel "/tmp"
      assertBool "/tmp should be unsafe" (not safe)

  , testCase "/tmp/foo is unsafe to relabel" $ do
      safe <- isSafeToRelabel "/tmp/foo"
      assertBool "/tmp/foo should be unsafe" (not safe)

  , testCase "subdirectory of home is safe" $ do
      safe <- isSafeToRelabel "/home/user/project"
      assertBool "home subdirectory should be safe" safe

  , testCase "/var/tmp is unsafe" $ do
      safe <- isSafeToRelabel "/var/tmp"
      assertBool "/var/tmp should be unsafe" (not safe)
  ]
