module Morloc.Test.ShmTests (shmTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.List (intercalate)
import System.Directory (copyFile, listDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.IO (IOMode(..), hClose, openFile)
import System.IO.Temp (getCanonicalTemporaryDirectory, createTempDirectory)
import System.Process (StdStream(..), createProcess, proc,
                       terminateProcess, waitForProcess)
import qualified System.Process as P
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (testCase, assertFailure)

import Morloc.Test.Common

-- | Compile the SHM stress test program into a temp directory
compileStressProgram :: TestEnv -> IO FilePath
compileStressProgram env = do
  let srcDir = teSuiteDir env </> "shm-tests"
  tmpBase <- getCanonicalTemporaryDirectory
  workDir <- createTempDirectory tmpBase "morloc-shm"
  entries <- listDirectory srcDir
  mapM_ (\f -> do
    let ext = takeExtension f
    when (ext `elem` [".loc", ".py", ".hpp"]) $
      copyFile (srcDir </> f) (workDir </> f)
    ) entries
  (ec, _, err) <- morlocMake workDir "nexus" "main.loc"
  case ec of
    ExitSuccess -> return workDir
    ExitFailure c -> error $
      "SHM stress program compile failed (exit " ++ show c ++ "):\n" ++ err

shmTests :: TestEnv -> TestTree
shmTests env = withResource (compileStressProgram env) removeDirectoryRecursive $
  \getWorkDir -> testGroup "SHM"
    [ normalConcurrentCleanup getWorkDir
    , rapidFireCleanup getWorkDir
    , sigtermBehavior getWorkDir
    ]

-- ======================================================================
-- Test 1: Concurrent SHM cleanup
-- ======================================================================

normalConcurrentCleanup :: IO FilePath -> TestTree
normalConcurrentCleanup getWorkDir = testCase "normalConcurrentCleanup" $ do
  workDir <- getWorkDir
  let conc = 8 :: Int
      rounds = 3 :: Int
  cleanupMorlocResources
  threadDelay 100000  -- 100ms settle
  mapM_ (runRound workDir conc rounds) [1..rounds]

  where
    runRound workDir conc totalRounds roundNum = do
      before <- countShm

      results <- mapConcurrently (\_ ->
        try (runNexus workDir "stress" ["1000", "2.0"])
          :: IO (Either SomeException (ExitCode, String, String))
        ) [1..conc :: Int]

      -- 1s settle
      threadDelay 1000000

      after <- countShm
      afterSegs <- listShmWithAge

      let ecs       = [ec  | Right (ec, _, _) <- results]
          outs      = [strip out | Right (_, out, _) <- results]
          numFailed = length [() | Left _ <- results]
          allExitOk   = all (== ExitSuccess) ecs && numFailed == 0
          allOutputOk = all (== "499500") outs
          shmDelta    = after - before

      when (not allExitOk || not allOutputOk || shmDelta /= 0) $
        assertFailure $ unlines $
          [ "normalConcurrentCleanup: SHM leak detected"
          , "  workDir: " ++ workDir
          , "  round " ++ show roundNum ++ "/" ++ show totalRounds
              ++ ": " ++ show conc ++ " concurrent, "
              ++ show (length ecs) ++ " succeeded"
          , "    before: " ++ show before ++ " segments"
          , "    after:  " ++ show after ++ " segments (after 1s settle)"
          ] ++
          [ "    leaked: " ++ intercalate "\n            "
              [seg ++ " (" ++ show age ++ "s old)" | (seg, age) <- afterSegs]
          | not (null afterSegs)
          ] ++
          [ "    exit codes: " ++ show (map exitToInt ecs)
          , "    outputs: " ++ show outs
          ] ++
          [ "    exceptions: " ++ show numFailed | numFailed > 0 ]

-- ======================================================================
-- Test 2: Rapid-fire sequential cleanup
-- ======================================================================

rapidFireCleanup :: IO FilePath -> TestTree
rapidFireCleanup getWorkDir = testCase "rapidFireCleanup" $ do
  workDir <- getWorkDir
  let iterations = 50 :: Int
  cleanupMorlocResources
  threadDelay 100000  -- 100ms settle
  before <- countShm

  failures <- runIterations workDir iterations (0 :: Int)

  -- 1s settle
  threadDelay 1000000

  after <- countShm
  afterSegs <- listShmWithAge
  let shmDelta = after - before

  when (failures > 0 || shmDelta > 0) $
    assertFailure $ unlines $
      [ "rapidFireCleanup:"
      , "  workDir: " ++ workDir
      , "  iterations: " ++ show iterations
      , "  failures: " ++ show failures
      , "  before: " ++ show before ++ " segments"
      , "  after:  " ++ show after ++ " segments (after 1s settle)"
      ] ++
      [ "  leaked: " ++ intercalate "\n          "
          [seg ++ " (" ++ show age ++ "s old)" | (seg, age) <- afterSegs]
      | not (null afterSegs)
      ]

  where
    runIterations _ 0 failures = return failures
    runIterations wd remaining failures = do
      result <- try (runNexus wd "stress" ["100", "0.0"])
        :: IO (Either SomeException (ExitCode, String, String))
      let failed = case result of
            Right (ExitSuccess, out, _) -> strip out /= "4950"
            _ -> True
      threadDelay 10000  -- 10ms
      runIterations wd (remaining - 1) (if failed then failures + 1 else failures)

-- ======================================================================
-- Test 3: SIGTERM triggers clean SHM cleanup
-- ======================================================================

sigtermBehavior :: IO FilePath -> TestTree
sigtermBehavior getWorkDir = testCase "sigtermBehavior" $ do
  workDir <- getWorkDir
  cleanupMorlocResources
  threadDelay 100000  -- 100ms settle

  before <- countShm

  -- Launch nexus with a long-running command (5s sleep)
  devNull <- openFile "/dev/null" WriteMode
  let cp = (proc (workDir </> "nexus") ["stress", "1000", "5.0"])
            { P.cwd = Just workDir
            , P.std_out = UseHandle devNull
            , P.std_err = UseHandle devNull
            }
  (_, _, _, ph) <- createProcess cp

  -- Wait 1s for the process to start and create SHM segments
  threadDelay 1000000

  -- Send SIGTERM (terminateProcess sends SIGTERM on Unix)
  terminateProcess ph

  -- Wait for exit
  _ec <- waitForProcess ph
  hClose devNull

  -- 1s settle
  threadDelay 1000000

  after <- countShm
  afterSegs <- listShmWithAge
  let shmDelta = after - before

  when (shmDelta > 0) $
    assertFailure $ unlines $
      [ "sigtermBehavior: SHM leak after SIGTERM"
      , "  workDir: " ++ workDir
      , "  before: " ++ show before ++ " segments"
      , "  after:  " ++ show after ++ " segments (after 1s settle)"
      , "  leaked: " ++ intercalate "\n          "
          [seg ++ " (" ++ show age ++ "s old)" | (seg, age) <- afterSegs]
      ]

  -- Clean up leaked segments so they don't affect other tests
  cleanupMorlocResources

-- ======================================================================
-- Helpers
-- ======================================================================

exitToInt :: ExitCode -> Int
exitToInt ExitSuccess = 0
exitToInt (ExitFailure n) = n
