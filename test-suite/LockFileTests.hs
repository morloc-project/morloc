{- |
Module      : LockFileTests
Description : Install locks are released when their scope ends
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module LockFileTests (lockFileTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import GHC.IO.Handle.Lock (LockMode (..), hTryLock)
import qualified Morloc.Monad as MM
import Morloc.CodeGenerator.SystemConfig (withInitLock)
import Morloc.Module (withModuleLock)
import Morloc.Namespace.Prim (Defaultable (..))
import Morloc.Namespace.State (Config (..), MorlocError, MorlocMonad)
import Morloc.System (openLockFile)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (hClose)
import System.Process (ProcessHandle, spawnProcess, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

lockFileTests :: TestTree
lockFileTests =
  testGroup
    "install locks"
    [ testCase "a process started under the init lock does not hold it" $
        withScratch "init" $ \dir ->
          assertChildLeavesLockFree (dir </> ".init.lock") (withInitLock False dir)
    , testCase "a process started under a module lock does not hold it" $
        withScratch "module" $ \dir ->
          assertChildLeavesLockFree (dir </> "inherit.lock") $ \start -> do
            r <- runMM dir (withModuleLock dir (T.pack "inherit") (liftIO start))
            either (\e -> assertFailure ("the locked action failed: " <> show e)) return r
    , testCase "a module lock is released when an exception escapes it" $
        withScratch "escape" $ \dir -> do
          _ <- try (runMM dir (withModuleLock dir (T.pack "mod") (liftIO (ioError (userError "boom"))))) ::
                 IO (Either SomeException (Either MorlocError ()))
          again <- timeout 5000000 (runMM dir (withModuleLock dir (T.pack "mod") (return ())))
          assertBool "the module lock was still held after the exception" (maybe False (const True) again)
    ]

-- | Start a child under the lock, leave the lock's scope while the child still
-- runs, and check that the lock can be taken again well before the child
-- exits. Any process forked concurrently holds every descriptor until it
-- execs, so the lock may be briefly busy; only a holder that outlives that
-- window is a failure.
assertChildLeavesLockFree :: FilePath -> (IO ProcessHandle -> IO ProcessHandle) -> Assertion
assertChildLeavesLockFree lockPath underLock =
  bracket (underLock (spawnProcess "sleep" ["60"])) stop $ \_ -> do
    free <- poll (1000 :: Int)
    assertBool "the child still holds the lock" free
  where
    stop p = terminateProcess p >> waitForProcess p
    poll k = do
      free <- bracket (openLockFile lockPath) hClose (`hTryLock` ExclusiveLock)
      if free || k <= 0 then return free else threadDelay 10000 >> poll (k - 1)

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
