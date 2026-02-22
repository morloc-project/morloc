module Morloc.Test.Common
  ( TestEnv (..)
  , withTestCopy
  , withTestDir
  , morlocMake
  , morlocInstall
  , morlocUninstall
  , runProgram
  , runNexus
  , runNexusQuiet
  , assertFileExists
  , assertDirExists
  , assertNotExists
  , assertContains
  , assertJsonEq
  -- Daemon helpers
  , DaemonHandle (..)
  , withDaemon
  , pickFreePort
  , waitForHttp
  , httpGet
  , httpPost
  , lpRequest
  , jsonField
  -- Resource tracking
  , countZombies
  , countShm
  , countTmp
  , listShm
  , listShmWithAge
  -- Utilities
  , strip
  , readDef
  , cleanupMorlocResources
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Stack (HasCallStack)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , getModificationTime
  , listDirectory
  , removeDirectoryRecursive
  )
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), hClose, openFile)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process
  ( CreateProcess (cwd, std_err, std_out)
  , ProcessHandle
  , StdStream (..)
  , createProcess
  , proc
  , readCreateProcessWithExitCode
  , readProcessWithExitCode
  , terminateProcess
  , waitForProcess
  )
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

data TestEnv = TestEnv
  { teSuiteDir :: FilePath
  , teMorlocHome :: FilePath
  }

-- | Create a temp directory, run action, clean up
withTestDir :: (FilePath -> IO a) -> IO a
withTestDir action = bracket setup cleanup action
  where
    setup = do
      tmpBase <- getCanonicalTemporaryDirectory
      createTempDirectory tmpBase "morloc-test"
    cleanup = removeDirectoryRecursive

-- | Copy source dir into a temp dir, run action, clean up
withTestCopy :: FilePath -> (FilePath -> IO a) -> IO a
withTestCopy srcDir action = bracket setup cleanup action
  where
    setup = do
      tmpBase <- getCanonicalTemporaryDirectory
      tmpDir <- createTempDirectory tmpBase "morloc-test"
      copyDirRecursive srcDir tmpDir
      return tmpDir
    cleanup = removeDirectoryRecursive

copyDirRecursive :: FilePath -> FilePath -> IO ()
copyDirRecursive src dst = do
  entries <- listDirectory src
  mapM_ (copyEntry src dst) entries
  where
    copyEntry s d name = do
      let sp = s </> name
          dp = d </> name
      isDir <- doesDirectoryExist sp
      if isDir
        then do
          createDirectoryIfMissing True dp
          copyDirRecursive sp dp
        else do
          createDirectoryIfMissing True (takeDirectory dp)
          copyFile sp dp

-- | Compile a .loc file with morloc make
morlocMake ::
  FilePath ->
  String ->
  String ->
  IO (ExitCode, String, String)
morlocMake workDir outName locFile = do
  let args = ["make", "-o", outName, locFile]
      cp = (proc "morloc" args) {cwd = Just workDir}
  readCreateProcessWithExitCode cp ""

-- | Compile and install a .loc file
morlocInstall ::
  FilePath ->
  String ->
  [String] ->
  String ->
  IO (ExitCode, String, String)
morlocInstall workDir outName extraArgs locFile = do
  let args = ["make", "--install", "--force", "-o", outName] ++ extraArgs ++ [locFile]
      cp = (proc "morloc" args) {cwd = Just workDir}
  readCreateProcessWithExitCode cp ""

morlocUninstall :: String -> IO ()
morlocUninstall progName = do
  _ <- readProcessWithExitCode "morloc" ["uninstall", "--program", progName] ""
  return ()

-- | Run an installed program
runProgram :: FilePath -> String -> [String] -> IO (ExitCode, String, String)
runProgram binPath subcmd args =
  readProcessWithExitCode binPath (subcmd : args) ""

-- | Run a nexus binary in a working directory
runNexus :: FilePath -> String -> [String] -> IO (ExitCode, String, String)
runNexus workDir subcmd args = do
  let cp = (proc (workDir </> "nexus") (subcmd : args)) {cwd = Just workDir}
  readCreateProcessWithExitCode cp ""

{- | Run a nexus binary with output redirected to /dev/null.
Unlike runNexus, this does not capture stdout/stderr via pipes, so child
pool processes inherit /dev/null and won't be affected by pipe closure.
Use this in stress tests to match the shell test behavior.
-}
runNexusQuiet :: FilePath -> String -> [String] -> IO ExitCode
runNexusQuiet workDir subcmd args = do
  devNull <- openFile "/dev/null" WriteMode
  let cp =
        (proc (workDir </> "nexus") (subcmd : args))
          { cwd = Just workDir
          , std_out = UseHandle devNull
          , std_err = UseHandle devNull
          }
  (_, _, _, ph) <- createProcess cp
  ec <- waitForProcess ph
  hClose devNull
  return ec

-- ======================================================================
-- Daemon lifecycle
-- ======================================================================

data DaemonHandle = DaemonHandle
  { dhProcess :: ProcessHandle
  , dhWorkDir :: FilePath
  }

-- | Start a daemon, run action, stop daemon
withDaemon ::
  -- | working directory with compiled nexus
  FilePath ->
  -- | extra daemon args (e.g. ["--http-port", "12345"])
  [String] ->
  (DaemonHandle -> IO a) ->
  IO a
withDaemon workDir extraArgs action = bracket startD stopD action
  where
    startD = do
      devNull <- openFile "/dev/null" WriteMode
      let cp =
            (proc (workDir </> "nexus") ("--daemon" : extraArgs))
              { cwd = Just workDir
              , std_out = UseHandle devNull
              , std_err = UseHandle devNull
              }
      (_, _, _, ph) <- createProcess cp
      return (DaemonHandle ph workDir)
    stopD dh = do
      terminateProcess (dhProcess dh)
      _ <- waitForProcess (dhProcess dh)
      return ()

-- | Pick a random free TCP port
pickFreePort :: IO Int
pickFreePort = do
  (_, out, _) <-
    readProcessWithExitCode
      "python3"
      [ "-c"
      , "import socket; s=socket.socket(); s.bind(('127.0.0.1',0)); print(s.getsockname()[1]); s.close()"
      ]
      ""
  return (read (strip out))

-- | Wait for an HTTP port to respond, with retries
waitForHttp :: Int -> Int -> IO Bool
waitForHttp port maxWaitMs = go 0
  where
    stepMs = 200
    go elapsed
      | elapsed >= maxWaitMs = return False
      | otherwise = do
          result <-
            try $ httpGet ("http://127.0.0.1:" ++ show port ++ "/health") ::
              IO (Either SomeException (ExitCode, String, String))
          case result of
            Right (ExitSuccess, _, _) -> return True
            _ -> do
              threadDelay (stepMs * 1000)
              go (elapsed + stepMs)

-- | HTTP GET via curl
httpGet :: String -> IO (ExitCode, String, String)
httpGet url = readProcessWithExitCode "curl" ["-s", url] ""

-- | HTTP POST via curl
httpPost :: String -> String -> IO (ExitCode, String, String)
httpPost url body =
  readProcessWithExitCode
    "curl"
    ["-s", "-X", "POST", url, "-H", "Content-Type: application/json", "-d", body]
    ""

-- | Send a length-prefixed JSON message to a Unix socket or TCP endpoint
lpRequest :: String -> String -> IO String
lpRequest target jsonMsg = do
  let script =
        unlines
          [ "import socket, struct, sys, json"
          , "target = sys.argv[1]"
          , "msg = sys.argv[2].encode('utf-8')"
          , "if target.startswith('/'):"
          , "    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)"
          , "    s.connect(target)"
          , "else:"
          , "    host, port = target.rsplit(':', 1)"
          , "    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)"
          , "    s.connect((host, int(port)))"
          , "s.settimeout(10)"
          , "s.sendall(struct.pack('>I', len(msg)) + msg)"
          , "resp_len_bytes = b''"
          , "while len(resp_len_bytes) < 4:"
          , "    chunk = s.recv(4 - len(resp_len_bytes))"
          , "    if not chunk: break"
          , "    resp_len_bytes += chunk"
          , "resp_len = struct.unpack('>I', resp_len_bytes)[0]"
          , "resp = b''"
          , "while len(resp) < resp_len:"
          , "    chunk = s.recv(resp_len - len(resp))"
          , "    if not chunk: break"
          , "    resp += chunk"
          , "s.close()"
          , "print(resp.decode('utf-8'))"
          ]
  (_, out, _) <- readProcessWithExitCode "python3" ["-c", script, target, jsonMsg] ""
  return (strip out)

-- | Extract a JSON field value using python3
jsonField :: String -> String -> IO String
jsonField jsonStr field = do
  let script =
        unlines
          [ "import json, sys"
          , "data = json.loads(sys.argv[1])"
          , "val = data.get(sys.argv[2])"
          , "if val is None:"
          , "    print('')"
          , "elif isinstance(val, (dict, list)):"
          , "    print(json.dumps(val, separators=(',', ':')))"
          , "elif isinstance(val, bool):"
          , "    print('true' if val else 'false')"
          , "else:"
          , "    print(val)"
          ]
  (_, out, _) <- readProcessWithExitCode "python3" ["-c", script, jsonStr, field] ""
  return (strip out)

-- ======================================================================
-- Resource tracking
-- ======================================================================

countZombies :: IO Int
countZombies = do
  (_, out, _) <-
    readProcessWithExitCode "sh" ["-c", "ps -eo stat 2>/dev/null | grep -c '^Z' || echo 0"] ""
  return (readDef 0 (strip out))

countShm :: IO Int
countShm = do
  (_, out, _) <-
    readProcessWithExitCode
      "sh"
      ["-c", "ls -1 /dev/shm/morloc-* 2>/dev/null | wc -l || echo 0"]
      ""
  return (readDef 0 (strip out))

countTmp :: IO Int
countTmp = do
  (_, out, _) <-
    readProcessWithExitCode
      "sh"
      ["-c", "ls -1d /tmp/morloc.* 2>/dev/null | wc -l || echo 0"]
      ""
  return (readDef 0 (strip out))

-- | List actual SHM segment names for diagnostics
listShm :: IO [String]
listShm = do
  (_, out, _) <-
    readProcessWithExitCode
      "sh"
      ["-c", "ls -1 /dev/shm/morloc-* 2>/dev/null || true"]
      ""
  return (filter (not . null) (lines out))

-- | List SHM segments with age in seconds (from stat mtime)
listShmWithAge :: IO [(String, Double)]
listShmWithAge = do
  segs <- listShm
  now <- getCurrentTime
  catMaybes <$> mapM (getAge now) segs
  where
    getAge now seg = do
      result <- try (getModificationTime seg) :: IO (Either SomeException UTCTime)
      return $ case result of
        Right mtime -> Just (seg, realToFrac (diffUTCTime now mtime))
        Left _ -> Nothing

-- ======================================================================
-- Assertions
-- ======================================================================

assertFileExists :: (HasCallStack) => String -> FilePath -> Assertion
assertFileExists label path = do
  exists <- doesFileExist path
  if exists
    then return ()
    else assertFailure (label ++ ": file not found: " ++ path)

assertDirExists :: (HasCallStack) => String -> FilePath -> Assertion
assertDirExists label path = do
  exists <- doesDirectoryExist path
  if exists
    then return ()
    else assertFailure (label ++ ": directory not found: " ++ path)

assertNotExists :: (HasCallStack) => String -> FilePath -> Assertion
assertNotExists label path = do
  exists <- doesPathExist path
  if exists
    then assertFailure (label ++ ": should not exist: " ++ path)
    else return ()

assertContains :: (HasCallStack) => String -> String -> String -> Assertion
assertContains label needle haystack =
  assertBool
    (label ++ ": expected to contain " ++ show needle ++ " in " ++ show (take 200 haystack))
    (needle `isInfixOf` haystack)

-- | Assert a JSON field equals an expected value, showing the raw response on failure
assertJsonEq :: (HasCallStack) => String -> String -> String -> String -> Assertion
assertJsonEq label rawJson field expected = do
  val <- jsonField rawJson field
  if val == expected
    then return ()
    else
      assertFailure $
        label
          ++ ": field "
          ++ show field
          ++ " expected "
          ++ show expected
          ++ " but got "
          ++ show val
          ++ "\n  raw response: "
          ++ show (take 500 rawJson)

-- ======================================================================
-- Internal utilities
-- ======================================================================

strip :: String -> String
strip = reverse . dropWhile (== '\n') . reverse . dropWhile (== '\n')

readDef :: Int -> String -> Int
readDef def s = case reads s of
  [(n, _)] -> n
  _ -> def

-- | Remove stale morloc SHM segments and tmp dirs to get a clean baseline
cleanupMorlocResources :: IO ()
cleanupMorlocResources = do
  _ <- readProcessWithExitCode "sh" ["-c", "rm -f /dev/shm/morloc-* 2>/dev/null"] ""
  _ <- readProcessWithExitCode "sh" ["-c", "rm -rf /tmp/morloc.* 2>/dev/null"] ""
  return ()
