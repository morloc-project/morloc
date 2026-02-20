module Morloc.Test.Common
  ( TestEnv(..)
  , withTestCopy
  , morlocInstall
  , morlocUninstall
  , runProgram
  , assertFileExists
  , assertDirExists
  , assertNotExists
  ) where

import Control.Exception (bracket)
import GHC.Stack (HasCallStack)
import System.Directory (doesFileExist, doesDirectoryExist, doesPathExist,
                         copyFile, createDirectoryIfMissing, listDirectory,
                         removeDirectoryRecursive)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (CreateProcess(cwd), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Test.Tasty.HUnit (Assertion, assertFailure)

data TestEnv = TestEnv
  { teSuiteDir   :: FilePath
  , teMorlocHome :: FilePath
  }

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

morlocInstall
  :: FilePath    -- ^ working directory
  -> String      -- ^ output name (-o)
  -> [String]    -- ^ extra args (e.g. ["--include", "foo.py"])
  -> String      -- ^ .loc filename
  -> IO (ExitCode, String, String)
morlocInstall workDir outName extraArgs locFile = do
  let args = ["make", "--install", "--force", "-o", outName] ++ extraArgs ++ [locFile]
      cp = (proc "morloc" args) { cwd = Just workDir }
  readCreateProcessWithExitCode cp ""

morlocUninstall :: String -> IO ()
morlocUninstall progName = do
  _ <- readProcessWithExitCode "morloc" ["uninstall", "--program", progName] ""
  return ()

runProgram :: FilePath -> String -> [String] -> IO (ExitCode, String, String)
runProgram binPath subcmd args =
  readProcessWithExitCode binPath (subcmd : args) ""

assertFileExists :: HasCallStack => String -> FilePath -> Assertion
assertFileExists label path = do
  exists <- doesFileExist path
  if exists
    then return ()
    else assertFailure (label ++ ": file not found: " ++ path)

assertDirExists :: HasCallStack => String -> FilePath -> Assertion
assertDirExists label path = do
  exists <- doesDirectoryExist path
  if exists
    then return ()
    else assertFailure (label ++ ": directory not found: " ++ path)

assertNotExists :: HasCallStack => String -> FilePath -> Assertion
assertNotExists label path = do
  exists <- doesPathExist path
  if exists
    then assertFailure (label ++ ": should not exist: " ++ path)
    else return ()
