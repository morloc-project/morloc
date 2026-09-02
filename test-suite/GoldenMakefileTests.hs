{- |
Module      : GoldenMakefileTests
Description : Discover and run golden tests that build and execute full morloc programs

A golden test is a directory under @test-suite/golden-tests@ holding a
@Makefile@ (which builds and runs a morloc program, appending its output to
@obs.txt@) and an @exp.txt@ holding the expected output. Every such directory
is discovered and run; none has to be registered anywhere.

A directory containing a @SKIP@ file is not run. The file's contents are the
reason, listed once before the suite starts, so that a disabled test stays
visible and its justification lives next to the test rather than in a source
comment.
-}
module GoldenMakefileTests
  ( discoverGoldenTests
  , goldenMakefileTest
  ) where

import Control.Monad (filterM, unless)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, sort)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  , makeAbsolute
  )
import System.FilePath ((</>))
import qualified System.IO as SI
import qualified System.Process as SP
import Test.Tasty
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.HUnit (assertFailure, testCase)

-- | Every subdirectory of the golden-test root is a test. Runnable and skipped
-- tests are reported as separate groups so that disabled tests stay visible
-- without diluting the pass count.
--
-- Skip reasons are printed once, up front, rather than folded into the test
-- names: tasty pads every line of its report to the longest name in the tree,
-- so a sentence-long name indents the whole suite off the screen.
discoverGoldenTests :: FilePath -> IO [TestTree]
discoverGoldenTests root = do
  absRoot <- makeAbsolute root
  -- Hidden directories hold tooling (.claude), never tests.
  entries <- sort . filter (not . ("." `isPrefixOf`)) <$> listDirectory absRoot
  dirs <- filterM (doesDirectoryExist . (absRoot </>)) entries
  classified <- mapM (classify absRoot) dirs
  let skipped = [(name, reason) | Skipped name reason <- classified]
  unless (null skipped) $ do
    SI.hPutStrLn SI.stderr "Skipped golden tests (see the SKIP file in each):"
    mapM_
      (\(name, reason) -> SI.hPutStrLn SI.stderr ("  " ++ name ++ ": " ++ reason))
      skipped
  return
    [ testGroup "golden" [t | Runnable t <- classified]
    , testGroup "golden (skipped)" [testCase name (return ()) | (name, _) <- skipped]
    ]

data Classified
  = Runnable TestTree
  | Skipped String String

-- | Decide what a directory is. A missing @Makefile@ or @exp.txt@ is a
-- failure, not a silent omission: a stray directory left behind by a crashed
-- run and a test whose author forgot @exp.txt@ both used to vanish from the
-- suite unnoticed.
classify :: FilePath -> FilePath -> IO Classified
classify root name = do
  let dir = root </> name
  skipReason <- readIfPresent (dir </> "SKIP")
  case skipReason of
    Just reason -> return $ Skipped name (unwords (words reason))
    Nothing -> do
      hasMakefile <- doesFileExist (dir </> "Makefile")
      hasExp <- doesFileExist (dir </> "exp.txt")
      return . Runnable $
        case (hasMakefile, hasExp) of
          (False, _) ->
            testCase name . assertFailure $
              name
                ++ ": not a golden test -- no Makefile. Delete the directory if it is a\
                   \ leftover build artifact, or add a Makefile."
          (_, False) ->
            testCase name . assertFailure $
              name
                ++ ": no exp.txt. Every golden test needs its expected output; write\
                   \ one (`touch exp.txt` first if you mean to fill it in with\
                   \ --accept), or add a SKIP file naming the reason it cannot run yet."
          _ -> goldenMakefileTest name dir

readIfPresent :: FilePath -> IO (Maybe String)
readIfPresent path = do
  exists <- doesFileExist path
  if exists then Just <$> SI.readFile' path else return Nothing

goldenMakefileTest :: String -> String -> TestTree
goldenMakefileTest msg testdir =
  goldenVsFile
    msg
    (testdir </> "exp.txt")
    (testdir </> "obs.txt")
    (makeManifoldFile testdir)

-- | Build and run the test program, then clean up after it. @make@'s exit code
-- is deliberately ignored: tests of compiler diagnostics expect the build to
-- fail, and the comparison of obs.txt against exp.txt is the only verdict.
-- Each Makefile captures its own stderr into build.err / obs.err.
--
-- Cleaning is skipped when the run did not match, because most clean targets
-- delete build.err and obs.err -- the two files you need to see why. A failing
-- test leaves its build tree and stderr in place; the next passing run removes
-- them.
makeManifoldFile :: String -> IO ()
makeManifoldFile path = do
  abspath <- makeAbsolute path
  runQuietly ["-C", abspath, "--quiet"]
  matched <- outputMatched abspath
  if matched
    then runQuietly ["-C", abspath, "--quiet", "clean"]
    else return ()

outputMatched :: FilePath -> IO Bool
outputMatched dir = do
  expected <- readIfPresentBytes (dir </> "exp.txt")
  observed <- readIfPresentBytes (dir </> "obs.txt")
  return (expected == observed)

readIfPresentBytes :: FilePath -> IO (Maybe BS.ByteString)
readIfPresentBytes path = do
  exists <- doesFileExist path
  if exists then Just <$> BS.readFile path else return Nothing

runQuietly :: [String] -> IO ()
runQuietly args = do
  _ <- SP.readCreateProcessWithExitCode (SP.proc "make" args) ""
  return ()
