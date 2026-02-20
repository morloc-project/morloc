module Morloc.Test.StressTests (stressTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (try, SomeException)
import Data.List (intercalate)
import System.Directory (copyFile, doesFileExist, listDirectory,
                         removeDirectoryRecursive)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.IO.Temp (getCanonicalTemporaryDirectory, createTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup, withResource)
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
-- Zombie stress test: 50 concurrent nexus invocations, check for leaks
-- ======================================================================

zombieTest :: Workload -> IO FilePath -> TestTree
zombieTest wl getWorkDir = testCase "zombie" $ do
  workDir <- getWorkDir
  cleanupMorlocResources
  let iterations = 50 :: Int
      (subcmd, args) = head (wlCalls wl)
  before <- takeSnapshot

  _ <- mapConcurrently (\_ ->
    try (runNexusQuiet workDir subcmd args) :: IO (Either SomeException ExitCode)
    ) [1..iterations]

  threadDelay 2000000  -- 2s settle

  after <- takeSnapshot
  let newShm = rsShm after - rsShm before
      newTmp = rsTmp after - rsTmp before
      problems = concat
        [ ["shm leaked: " ++ show newShm | newShm > 0]
        , ["tmp leaked: " ++ show newTmp | newTmp > 0]
        ]

  if null problems then return ()
  else assertFailure $ unlines
    [ "workDir: " ++ workDir
    , formatDelta ("after " ++ show iterations ++ " concurrent + 2s settle") before after
    , intercalate "; " problems
    ]

-- ======================================================================
-- Concurrent stress test: 10 rounds x 10 concurrent, check for leaks
-- ======================================================================

concurrentStressTest :: Workload -> IO FilePath -> TestTree
concurrentStressTest wl getWorkDir = testCase "concurrent" $ do
  workDir <- getWorkDir
  cleanupMorlocResources
  let concurrent = 10 :: Int
      rounds = 10 :: Int
  before <- takeSnapshot

  leakRounds <- goRounds workDir (0 :: Int) rounds concurrent before

  threadDelay 2000000  -- 2s settle

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
    goRounds _ leakR 0 _ _ = return leakR
    goRounds wd leakR remaining conc before0 = do
      let (subcmd, args) = head (wlCalls wl)
      _ <- mapConcurrently (\_ ->
        try (runNexusQuiet wd subcmd args) :: IO (Either SomeException ExitCode)
        ) [1..conc :: Int]
      threadDelay 50000  -- 50ms between rounds
      cur <- takeSnapshot
      let leaked = rsShm cur - rsShm before0 > 0 || rsTmp cur - rsTmp before0 > 0
      goRounds wd (if leaked then leakR + 1 else leakR)
               (remaining - 1) conc before0

-- ======================================================================
-- Crash recovery: 10 concurrent crash-and-recover cycles
-- ======================================================================

crashRecoveryTest :: Workload -> IO FilePath -> TestTree
crashRecoveryTest wl getWorkDir = testCase "crash-recovery" $ do
  workDir <- getWorkDir
  cleanupMorlocResources
  let iterations = 10 :: Int
      (subcmd, args) = head (wlCalls wl)
  before <- takeSnapshot

  results <- mapConcurrently (\_ -> do
    (_, out, _) <- readProcessWithExitCode "sh"
      [ "-c"
      , "cd " ++ show workDir ++ " && "
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
    return (strip out == "1")
    ) [1..iterations]

  threadDelay 2000000  -- 2s settle

  after <- takeSnapshot
  let hangs = length (filter id results)
      newShm = rsShm after - rsShm before
      newTmp = rsTmp after - rsTmp before
      problems = concat
        [ ["hung: " ++ show hangs ++ "/" ++ show iterations
           ++ " (nexus did not exit after child kill)" | hangs > 0]
        , ["shm leaked: " ++ show newShm | newShm > 0]
        , ["tmp leaked: " ++ show newTmp | newTmp > 0]
        ]

  if null problems then return ()
  else assertFailure $ unlines
    [ "workDir: " ++ workDir
    , formatDelta ("after " ++ show iterations ++ " crash iterations + 2s settle") before after
    , intercalate "\n" problems
    ]

-- ======================================================================
-- Top-level: compile each workload once, share across test types
-- ======================================================================

workloadGroup :: TestEnv -> Workload -> TestTree
workloadGroup env wl =
  withResource (compileWorkload env wl) removeDirectoryRecursive $ \getWorkDir ->
    testGroup (wlName wl)
      [ zombieTest wl getWorkDir
      , concurrentStressTest wl getWorkDir
      , crashRecoveryTest wl getWorkDir
      ]

stressTests :: TestEnv -> TestTree
stressTests env = testGroup "Stress"
  [ workloadGroup env wl | wl <- workloads ]
