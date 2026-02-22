module Morloc.Test.ConcurrencyTests (concurrencyTests) where

import System.Directory (copyFile, doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import Morloc.Test.Common

-- | A concurrency test spec: compile a .loc file, run each subcommand
data ConcSpec = ConcSpec
  { csLocFile :: String -- .loc filename (relative to concurrency-tests/)
  , csSubcommands :: [String] -- exported functions to run
  }

concurrencyTest :: TestEnv -> String -> ConcSpec -> TestTree
concurrencyTest env name spec =
  testGroup
    name
    [ testCase subcmd $ runSubcmd env spec subcmd
    | subcmd <- csSubcommands spec
    ]

runSubcmd :: TestEnv -> ConcSpec -> String -> IO ()
runSubcmd env spec subcmd = do
  let srcDir = teSuiteDir env </> "concurrency-tests"
  withTestDir $ \workDir -> do
    -- Copy .loc file and helpers
    copyLocAndHelpers srcDir (csLocFile spec) workDir

    -- Compile
    (ec, _, err) <- morlocMake workDir "nexus" (csLocFile spec)
    case ec of
      ExitSuccess -> return ()
      ExitFailure c ->
        assertFailure $
          csLocFile spec ++ ": compile failed (exit " ++ show c ++ "):\n" ++ err

    -- Run with timeout (handled by tasty's timeout mechanism)
    (rc, _, stderr) <- runNexus workDir subcmd []
    case rc of
      ExitSuccess -> return ()
      ExitFailure c ->
        assertFailure $
          csLocFile spec ++ ":" ++ subcmd ++ " failed (exit " ++ show c ++ "):\n" ++ stderr

copyLocAndHelpers :: FilePath -> String -> FilePath -> IO ()
copyLocAndHelpers srcDir locFile workDir = do
  copyFile (srcDir </> locFile) (workDir </> locFile)
  let helpersDir = srcDir </> "helpers"
  helpersExist <- doesDirectoryExist helpersDir
  if helpersExist
    then do
      entries <- listDirectory helpersDir
      mapM_ (\f -> copyFile (helpersDir </> f) (workDir </> f)) entries
    else return ()

concurrencyTests :: TestEnv -> TestTree
concurrencyTests env =
  testGroup
    "Concurrency"
    [ concurrencyTest env "bidi-py-r" $
        ConcSpec
          { csLocFile = "bidi-py-r.loc"
          , csSubcommands = ["testUni", "testBidi1", "testBidi5", "testBidi10", "testBidi11", "testBidi15"]
          }
    , concurrencyTest env "bidi-r-py" $
        ConcSpec
          { csLocFile = "bidi-r-py.loc"
          , csSubcommands = ["testBidi1", "testBidi5", "testBidi10", "testBidi11", "testBidi15"]
          }
    , concurrencyTest env "concurrent-uni" $
        ConcSpec
          { csLocFile = "concurrent-uni.loc"
          , csSubcommands = ["testPyToR15", "testRToPy15", "testPyToR20"]
          }
    , concurrencyTest env "deep-callback" $
        ConcSpec
          { csLocFile = "deep-callback.loc"
          , csSubcommands =
              ["testDepth2", "testDepth4", "testDepth6", "testDepth12", "testDeep4x5", "testDeep6x5"]
          }
    ]
