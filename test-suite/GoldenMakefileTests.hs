{- |
Module      : GoldenMakefileTests
Description : Discover and run golden tests that build and execute full morloc programs

A golden test is a directory under @test-suite/golden-tests@ holding a
@Makefile@ (which builds and runs a morloc program, appending its output to
@obs.txt@) and an @exp.txt@ holding the expected output. Every such directory
is discovered and run; none has to be registered anywhere.

With @MORLOC_TEST_SHARD@ set, only this run's share of the directories is
considered (see "GoldenShard").

A directory containing a @SKIP@ file is not run. The file's contents are the
reason, listed once before the suite starts, so that a disabled test stays
visible and its justification lives next to the test rather than in a source
comment.
-}
module GoldenMakefileTests
  ( discoverGoldenTests
  , goldenMakefileTest
  ) where

import Control.Exception (bracket)
import Control.Monad (filterM, unless, void)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, sort)
import GoldenShard (Shard, selectShard)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , findExecutable
  , getTemporaryDirectory
  , listDirectory
  , makeAbsolute
  , removeFile
  )
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory, (</>))
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
discoverGoldenTests :: Maybe Shard -> FilePath -> IO [TestTree]
discoverGoldenTests shard root = do
  absRoot <- makeAbsolute root
  -- Hidden directories hold tooling (.claude), never tests.
  entries <- sort . filter (not . ("." `isPrefixOf`)) <$> listDirectory absRoot
  allDirs <- filterM (doesDirectoryExist . (absRoot </>)) entries
  let dirs = maybe allDirs (`selectShard` allDirs) shard
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
-- Each Makefile captures its own stderr into build.err / obs.err; whatever
-- else reaches make's stderr (shell syntax errors, failed recipe lines, make's
-- own diagnostics) is stored in make.err.
--
-- Cleaning is skipped when the run did not match, because most clean targets
-- delete the .err files -- the files you need to see why. A failing
-- test leaves its build tree and stderr in place; the next passing run removes
-- them.
makeManifoldFile :: String -> IO ()
makeManifoldFile path = do
  abspath <- makeAbsolute path
  let shims = takeDirectory (takeDirectory abspath) </> "shims"
  err <- runQuietly shims ["-C", abspath, "--quiet"]
  BS.writeFile (abspath </> "make.err") err
  matched <- outputMatched abspath
  if matched
    then void (runQuietly shims ["-C", abspath, "--quiet", "clean"])
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

-- | Run @make@ with the suite's build parameters. Rust pools default to
-- cross-crate thin LTO, which re-optimizes rustmorloc and the Arrow crates on
-- every pool link (seconds of CPU per test) and, because concurrent cargo
-- builds serialize on the shared target-dir lock, stalls every other Rust
-- test in flight. The suite turns it off unless the caller has already set
-- @MORLOC_LANG_PARAMS@; a test that must exercise the shipped profile passes
-- @-X rust:lto=thin@ in its Makefile, which outranks the environment.
--
-- Where the system has no @timeout@ (macOS ships none), @shims@ goes on PATH
-- so the tests that bound a run with it still run it.
--
-- Returns make's stderr. It is collected outside the test directory because
-- most recipes begin with @rm -f *.err@.
runQuietly :: FilePath -> [String] -> IO BS.ByteString
runQuietly shims args = do
  env <- getEnvironment
  hasTimeout <- maybe False (const True) <$> findExecutable "timeout"
  let env' = case lookup langParamsVar env of
        Just _ -> env
        Nothing -> (langParamsVar, "rust:lto=off") : env
      env'' =
        if hasTimeout
          then env'
          else ("PATH", shims ++ ":" ++ maybe "" id (lookup "PATH" env')) : filter ((/= "PATH") . fst) env'
  tmp <- getTemporaryDirectory
  bracket (SI.openBinaryTempFile tmp "make.err") (\(p, h) -> SI.hClose h >> removeFile p) $ \(p, h) ->
    SI.withFile "/dev/null" SI.ReadWriteMode $ \devnull -> do
      let cp =
            (SP.proc "make" args)
              { SP.env = Just env''
              , SP.std_in = SP.UseHandle devnull
              , SP.std_out = SP.UseHandle devnull
              , SP.std_err = SP.UseHandle h
              }
      _ <- SP.withCreateProcess cp (\_ _ _ ph -> SP.waitForProcess ph)
      BS.readFile p
  where
    langParamsVar = "MORLOC_LANG_PARAMS"
