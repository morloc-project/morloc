{- |
Module      : LockFileTests
Description : Install locks are released when their scope ends
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module LockFileTests (lockFileTests) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Morloc.Monad as MM
import Morloc.CodeGenerator.SystemConfig (withInitLock)
import Morloc.Module (withModuleLock)
import Morloc.Namespace.Prim (Defaultable (..))
import Morloc.Namespace.State (Config (..), MorlocError, MorlocMonad)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process (readProcess)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

lockFileTests :: TestTree
lockFileTests =
  testGroup
    "install locks"
    [ testCase "a process started under the init lock does not hold it" $
        withScratch "init" $ \dir -> do
          fds <- withInitLock False dir childDescriptors
          assertBool "the child inherited the init lock" (not (".init.lock" `isInfixOf` fds))
    , testCase "a process started under a module lock does not hold it" $
        withScratch "module" $ \dir -> do
          r <- runMM dir (withModuleLock dir (T.pack "inherit") (liftIO childDescriptors))
          case r of
            Left e -> assertFailure ("the locked action failed: " <> show e)
            Right fds -> assertBool "the child inherited the module lock" (not ("inherit.lock" `isInfixOf` fds))
    , testCase "a module lock is released when an exception escapes it" $
        withScratch "escape" $ \dir -> do
          _ <- try (runMM dir (withModuleLock dir (T.pack "mod") (liftIO (ioError (userError "boom"))))) ::
                 IO (Either SomeException (Either MorlocError ()))
          again <- timeout 5000000 (runMM dir (withModuleLock dir (T.pack "mod") (return ())))
          assertBool "the module lock was still held after the exception" (maybe False (const True) again)
    ]

-- | The descriptors a child process inherits, as `ls -l` prints them.
childDescriptors :: IO String
childDescriptors = readProcess "sh" ["-c", "ls -l /proc/$$/fd"] ""

withScratch :: String -> (FilePath -> IO a) -> IO a
withScratch tag act = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> ("morloc-lockfile-" <> tag)
  createDirectoryIfMissing True dir
  r <- act dir
  removeDirectoryRecursive dir
  return r

runMM :: FilePath -> MorlocMonad a -> IO (Either MorlocError a)
runMM home action = do
  let cfg =
        Config
          { configHome = home
          , configState = home
          , configLibrary = home
          , configPlane = "default"
          , configPlaneCore = "morloclib"
          , configTmpDir = home </> "tmp"
          , configBuildConfig = home </> ".build-config.yaml"
          , configLangOverrides = mempty
          , configRegistry = Nothing
          }
  ((r, _), _) <- MM.runMorlocMonad Nothing 0 cfg defaultValue action
  return r
