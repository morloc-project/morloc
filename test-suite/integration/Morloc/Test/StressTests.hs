module Morloc.Test.StressTests (stressTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Data.List (intercalate)
import System.Directory (copyFile, doesFileExist, listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.IO.Temp (getCanonicalTemporaryDirectory, createTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)

import Morloc.Test.Common

-- | A workload: a golden test directory + calls to make against its nexus
data Workload = Workload
  { wlName    :: String    -- label (e.g. "cpp", "py-r")
  , wlTestDir :: String    -- relative to test-suite/golden-tests/
  , wlCalls   :: [(String, [String])]  -- (subcommand, args) pairs
  }

workloads :: [Workload]
workloads =
  [ Workload "cpp"    "argument-form-1-c"  [("foo", ["2"])]
  , Workload "py"     "argument-form-1-py" [("foo", ["2"])]
  , Workload "r"      "argument-form-1-r"  [("foo", ["2"])]
  , Workload "cpp-py" "interop-3a-cp"      [("foo", ["[1,2,3]"])]
  , Workload "cpp-r"  "interop-3a-rc"      [("foo", ["[1,2,3]"])]
  , Workload "py-r"   "interop-3a-pr"      [("foo", ["[1,2,3]"])]
  ]

-- | Compile a golden test into a temp directory, returning the work dir
compileWorkload :: TestEnv -> Workload -> IO FilePath
compileWorkload env wl = do
  let goldenDir = teSuiteDir env </> "golden-tests" </> wlTestDir wl
  tmpBase <- getCanonicalTemporaryDirectory
  workDir <- createTempDirectory tmpBase "morloc-stress"

  -- Copy source files
  entries <- listDirectory goldenDir
  mapM_ (\f -> do
    let ext = takeExtension f
    if ext `elem` [".loc", ".py", ".hpp", ".R"]
      then copyFile (goldenDir </> f) (workDir </> f)
      else return ()
    ) entries

  -- Find the .loc file referenced in the Makefile
  locFile <- findLocFile goldenDir
  (ec, _, err) <- morlocMake workDir "nexus" locFile
  case ec of
    ExitSuccess -> return workDir
    ExitFailure c -> do
      assertFailure $ wlName wl ++ ": compile failed (exit " ++ show c ++ "):\n" ++ err
      return workDir

findLocFile :: FilePath -> IO String
findLocFile dir = do
  let mkfile = dir </> "Makefile"
  exists <- doesFileExist mkfile
  if exists
    then do
      (_, out, _) <- readProcessWithExitCode "sh"
        ["-c", "grep 'morloc make' " ++ show mkfile ++ " | head -1 | grep -oP '[^ ]+\\.loc'"]
        ""
      return (strip out)
    else do
      entries <- listDirectory dir
      case filter (\f -> takeExtension f == ".loc") entries of
        (f:_) -> return f
        []    -> assertFailure ("No .loc file found in " ++ dir) >> return ""

-- | Snapshot of resource counts for comparison
data ResourceSnapshot = ResourceSnapshot
  { rsZombies :: Int
  , rsShm     :: Int
  , rsTmp     :: Int
  , rsShmList :: [String]  -- actual segment names
  }

takeSnapshot :: IO ResourceSnapshot
takeSnapshot = ResourceSnapshot
  <$> countZombies
  <*> countShm
  <*> countTmp
  <*> listShm

-- | Format a resource delta for failure messages
formatDelta :: String -> ResourceSnapshot -> ResourceSnapshot -> String
formatDelta label before after = unlines $
  [ label
  , "  shm: " ++ show (rsShm before) ++ " -> " ++ show (rsShm after)
      ++ " (delta: " ++ show (rsShm after - rsShm before) ++ ")"
  , "  tmp: " ++ show (rsTmp before) ++ " -> " ++ show (rsTmp after)
      ++ " (delta: " ++ show (rsTmp after - rsTmp before) ++ ")"
  , "  zombies: " ++ show (rsZombies before) ++ " -> " ++ show (rsZombies after)
      ++ " (delta: " ++ show (rsZombies after - rsZombies before) ++ ")"
  ] ++ if null (rsShmList after)
       then []
       else ["  segments: " ++ intercalate ", " (rsShmList after)]

-- ======================================================================
-- Zombie stress test
-- ======================================================================

zombieTest :: TestEnv -> Workload -> TestTree
zombieTest env wl = testCase ("zombie [" ++ wlName wl ++ "]") $ do
  workDir <- compileWorkload env wl
  cleanupMorlocResources
  let iterations = 50
  before <- takeSnapshot

  -- Track per-iteration leak appearances
  leakIterations <- go workDir 0 1 iterations before

  -- Allow time for async cleanup
  threadDelay 2000000  -- 2s

  after <- takeSnapshot
  let newShm = rsShm after - rsShm before
      newTmp = rsTmp after - rsTmp before
      problems = concat
        [ ["shm leaked: " ++ show newShm | newShm > 0]
        , ["tmp leaked: " ++ show newTmp | newTmp > 0]
        , ["leaks observed during " ++ show leakIterations
           ++ "/" ++ show iterations ++ " iterations" | leakIterations > 0]
        ]

  if null problems then return ()
  else assertFailure $ unlines
    [ "workDir: " ++ workDir
    , formatDelta ("after " ++ show iterations ++ " iterations + 2s settle") before after
    , intercalate "; " problems
    ]

  where
    go _ leakIters _ 0 _ = return leakIters
    go wd leakIters i remaining before0 = do
      let (subcmd, args) = head (wlCalls wl)
      _ <- try (runNexusQuiet wd subcmd args) :: IO (Either SomeException ExitCode)
      threadDelay 20000  -- 20ms
      cur <- takeSnapshot
      let leaked = rsShm cur - rsShm before0 > 0 || rsTmp cur - rsTmp before0 > 0
      go wd (if leaked then leakIters + 1 else leakIters) (i + 1) (remaining - 1) before0

-- ======================================================================
-- Concurrent stress test
-- ======================================================================

concurrentStressTest :: TestEnv -> Workload -> TestTree
concurrentStressTest env wl = testCase ("concurrent [" ++ wlName wl ++ "]") $ do
  workDir <- compileWorkload env wl
  cleanupMorlocResources
  let concurrent = 10
      rounds = 10
  before <- takeSnapshot

  leakRounds <- goRounds workDir 0 1 rounds concurrent before

  -- Allow time for async cleanup
  threadDelay 2000000  -- 2s

  after <- takeSnapshot
  let newShm = rsShm after - rsShm before
      newTmp = rsTmp after - rsTmp before
      problems = concat
        [ ["shm leaked: " ++ show newShm | newShm > 0]
        , ["tmp leaked: " ++ show newTmp | newTmp > 0]
        , ["leaks observed during " ++ show leakRounds
           ++ "/" ++ show rounds ++ " rounds" | leakRounds > 0]
        ]

  if null problems then return ()
  else assertFailure $ unlines
    [ "workDir: " ++ workDir
    , formatDelta ("after " ++ show rounds ++ " rounds x " ++ show concurrent
                   ++ " concurrent + 2s settle") before after
    , intercalate "; " problems
    ]

  where
    goRounds _ leakR _ 0 _ _ = return leakR
    goRounds wd leakR rnd remaining conc before0 = do
      let (subcmd, args) = head (wlCalls wl)
      mapM_ (\_ -> do
        _ <- try (runNexusQuiet wd subcmd args) :: IO (Either SomeException ExitCode)
        return ()
        ) [1..conc :: Int]
      threadDelay 50000  -- 50ms between rounds
      cur <- takeSnapshot
      let leaked = rsShm cur - rsShm before0 > 0 || rsTmp cur - rsTmp before0 > 0
      goRounds wd (if leaked then leakR + 1 else leakR)
               (rnd + 1) (remaining - 1) conc before0

-- ======================================================================
-- Crash recovery test
-- ======================================================================

crashRecoveryTest :: TestEnv -> Workload -> TestTree
crashRecoveryTest env wl = testCase ("crash-recovery [" ++ wlName wl ++ "]") $ do
  workDir <- compileWorkload env wl
  cleanupMorlocResources
  let iterations = 10
  before <- takeSnapshot
  (hangs, leaks) <- go workDir 0 0 iterations before

  threadDelay 2000000  -- 2s settling time

  after <- takeSnapshot
  let newShm = rsShm after - rsShm before
      newTmp = rsTmp after - rsTmp before
      problems = concat
        [ ["hung: " ++ show hangs ++ "/" ++ show iterations
           ++ " (nexus did not exit after child kill)" | hangs > 0]
        , ["shm leaked: " ++ show newShm
           ++ " (" ++ show leaks ++ "/" ++ show iterations
           ++ " iterations had mid-run leaks)" | newShm > 0]
        , ["tmp leaked: " ++ show newTmp | newTmp > 0]
        ]

  if null problems then return ()
  else assertFailure $ unlines
    [ "workDir: " ++ workDir
    , formatDelta ("after " ++ show iterations ++ " crash iterations + 2s settle") before after
    , intercalate "\n" problems
    ]

  where
    go _ hangs leakIters 0 _ = return (hangs, leakIters)
    go wd hangs leakIters remaining before0 = do
      let (subcmd, args) = head (wlCalls wl)
      (_, out, _) <- readProcessWithExitCode "sh"
        [ "-c"
        , "cd " ++ show wd ++ " && "
          ++ "./nexus " ++ subcmd ++ " " ++ unwords args
          ++ " > /dev/null 2>&1 &"
          ++ " NPID=$!; sleep 0.1;"
          ++ " CPID=$(ps -o pid= --ppid $NPID 2>/dev/null | head -1 | tr -d ' ');"
          ++ " if [ -n \"$CPID\" ]; then kill -9 $CPID 2>/dev/null; fi;"
          ++ " HUNG=0; for i in $(seq 1 50); do"
          ++ "   if ! kill -0 $NPID 2>/dev/null; then break; fi;"
          ++ "   sleep 0.1;"
          ++ "   if [ $i -eq 50 ]; then HUNG=1; kill -9 $NPID 2>/dev/null; fi;"
          ++ " done;"
          ++ " wait $NPID 2>/dev/null;"
          ++ " echo $HUNG"
        ] ""
      let hung = strip out == "1"
      cur <- takeSnapshot
      let leaked = rsShm cur - rsShm before0 > 0
      go wd (if hung then hangs + 1 else hangs)
             (if leaked then leakIters + 1 else leakIters)
             (remaining - 1) before0

-- ======================================================================
-- Top-level
-- ======================================================================

stressTests :: TestEnv -> TestTree
stressTests env = testGroup "Stress"
  [ testGroup "Zombie"
    [ zombieTest env wl | wl <- workloads ]
  , testGroup "Concurrent"
    [ concurrentStressTest env wl | wl <- workloads ]
  , testGroup "CrashRecovery"
    [ crashRecoveryTest env wl | wl <- workloads ]
  ]
