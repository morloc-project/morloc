{- |
Module      : GoldenMakefileTests
Description : Discover and run golden tests that build and execute full morloc programs

A golden test is a directory under @test-suite/golden@ holding a
@Makefile@ (which builds and runs a morloc program, appending its output to
@obs.txt@) and an @exp.txt@ holding the expected output. Every such directory
is discovered and run; none has to be registered anywhere.

Tests are grouped in subdirectories (@golden/types/...@,
@golden/spec/gen/...@). A directory holding a @Makefile@, an @exp.txt@ or a
@SKIP@ file is a test; any other directory is a group, and becomes a tasty
group of the same name. A test's directory name is unique across the whole
tree, since spec items and runtime rules cite tests by name alone.

With @MORLOC_TEST_SHARD@ set, only this run's share of the directories is
considered (see "GoldenShard").

With @MORLOC_GOLDEN_SNAPSHOT@ set to a directory, the pool sources and manifests each test
generated are copied under it (@<dir>/<test>/...@) before the test cleans
up, so two runs can be compared file by file. A manifest records its build
time, which differs between runs; compare manifests with it removed.

A directory containing a @SKIP@ file is not run. The file's contents are the
reason, listed once before the suite starts, so that a disabled test stays
visible and its justification lives next to the test rather than in a source
comment.
-}
module GoldenMakefileTests
  ( discoverGoldenTests
  ) where

import Control.Exception (bracket)
import Control.Monad (filterM, forM, unless, void, when)
import qualified Data.ByteString as BS
import Data.List (isInfixOf, isPrefixOf, sort)
import qualified Data.Map.Strict as Map
import GoldenShard (Shard, selectShard)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , findExecutable
  , getTemporaryDirectory
  , listDirectory
  , makeAbsolute
  , copyFile
  , createDirectoryIfMissing
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath (joinPath, makeRelative, takeDirectory, takeExtension, takeFileName, (</>))
import qualified System.IO as SI
import qualified System.Process as SP
import Test.Tasty
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.HUnit (assertFailure, testCase)

-- | Every test directory under the golden root, found by walking the groups.
-- Runnable and skipped tests are reported as separate groups so that disabled
-- tests stay visible without diluting the pass count.
--
-- Skip reasons are printed once, up front, rather than folded into the test
-- names: tasty pads every line of its report to the longest name in the tree,
-- so a sentence-long name indents the whole suite off the screen.
--
-- The shard is taken over the sorted list of test paths, so every test lands
-- in exactly one shard whatever group holds it.
discoverGoldenTests :: Maybe Shard -> FilePath -> FilePath -> IO [TestTree]
discoverGoldenTests shard shims root = do
  absRoot <- makeAbsolute root
  absShims <- makeAbsolute shims
  allTests <- findTests absRoot []
  let tests = maybe allTests (`selectShard` allTests) shard
      dups = Map.keys . Map.filter (> (1 :: Int)) $
        Map.fromListWith (+) [(last t, 1) | t <- allTests]
  classified <- forM tests $ \t -> (,) t <$> classify absShims (absRoot </> joinPath t) (last t)
  let skipped = [(last t, reason) | (t, Skipped _ reason) <- classified]
  unless (null skipped) $ do
    SI.hPutStrLn SI.stderr "Skipped golden tests (see the SKIP file in each):"
    mapM_
      (\(name, reason) -> SI.hPutStrLn SI.stderr ("  " ++ name ++ ": " ++ reason))
      skipped
  let duplicate name =
        testCase ("duplicate name " ++ name) . assertFailure $
          name ++ ": more than one golden test has this directory name. Test names\
                  \ are unique across test-suite/golden; rename one of them."
  return
    [ testGroup "golden" (map duplicate dups ++ grouped [(t, tree) | (t, Runnable tree) <- classified])
    , testGroup "golden (skipped)" [testCase name (return ()) | (name, _) <- skipped]
    ]

-- | Relative paths (as components) of every test directory under @dir@.
-- Hidden directories hold tooling (.claude), never tests. A directory with
-- no test files and no subdirectories is a leftover, and is reported as a
-- test so that it fails rather than vanishing.
findTests :: FilePath -> [String] -> IO [[String]]
findTests root rel = do
  let dir = root </> joinPath rel
  entries <- sort . filter (not . ("." `isPrefixOf`)) <$> listDirectory dir
  subdirs <- filterM (doesDirectoryExist . (dir </>)) entries
  isTest <- or <$> mapM (doesFileExist . (dir </>)) ["Makefile", "exp.txt", "SKIP"]
  if not (null rel) && (isTest || null subdirs)
    then return [rel]
    else concat <$> mapM (\d -> findTests root (rel ++ [d])) subdirs

-- | Nest tests into tasty groups named by their group directories.
grouped :: [([String], TestTree)] -> [TestTree]
grouped xs =
  [tree | ([_], tree) <- xs]
    ++ [ testGroup g (grouped [(rest, tree) | (g' : rest@(_ : _), tree) <- xs, g' == g])
       | g <- groups
       ]
  where
    groups = map fst . Map.toList $ Map.fromList [(g, ()) | (g : _ : _, _) <- xs]

data Classified
  = Runnable TestTree
  | Skipped String String

-- | Decide what a directory is. A missing @Makefile@ or @exp.txt@ is a
-- failure, not a silent omission: a stray directory left behind by a crashed
-- run and a test whose author forgot @exp.txt@ both used to vanish from the
-- suite unnoticed.
classify :: FilePath -> FilePath -> String -> IO Classified
classify shims dir name = do
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
          _ -> goldenMakefileTest shims name dir

readIfPresent :: FilePath -> IO (Maybe String)
readIfPresent path = do
  exists <- doesFileExist path
  if exists then Just <$> SI.readFile' path else return Nothing

goldenMakefileTest :: FilePath -> String -> String -> TestTree
goldenMakefileTest shims msg testdir =
  goldenVsFile
    msg
    (testdir </> "exp.txt")
    (testdir </> "obs.txt")
    (makeManifoldFile shims testdir)

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
makeManifoldFile :: FilePath -> String -> IO ()
makeManifoldFile shims path = do
  abspath <- makeAbsolute path
  err <- runQuietly shims ["-C", abspath, "--quiet"]
  BS.writeFile (abspath </> "make.err") err
  snap <- lookupEnv "MORLOC_GOLDEN_SNAPSHOT"
  case snap of
    Just dir | not (null dir) -> snapshotPools abspath dir
    _ -> return ()
  matched <- outputMatched abspath
  if matched
    then void (runQuietly shims ["-C", abspath, "--quiet", "clean"])
    else return ()

-- | Copy the generated pool sources and manifests under a test directory's
-- build trees into @snap/<test>/@, keeping their paths relative to the test. Staging
-- trees a killed build left behind (@*.tmp.<pid>@) are skipped.
snapshotPools :: FilePath -> FilePath -> IO ()
snapshotPools testdir snap = do
  files <- poolSources testdir
  let dest = snap </> takeFileName testdir
  stale <- doesDirectoryExist dest
  when stale (removeDirectoryRecursive dest)
  mapM_
    ( \f -> do
        let to = dest </> makeRelative testdir f
        createDirectoryIfMissing True (takeDirectory to)
        copyFile f to
    )
    files
  where
    poolSources d = do
      entries <- map (d </>) <$> listDirectory d
      fmap concat . mapM (visit False) $ entries
    visit inPools p = do
      isDir <- doesDirectoryExist p
      let name = takeFileName p
      if isDir
        then
          if name `elem` ["target", ".git"] || ".tmp." `isInfixOf` name
            then return []
            else do
              entries <- map (p </>) <$> listDirectory p
              concat <$> mapM (visit (inPools || name == "pools")) entries
        else
          return [p | name == "manifest.json" || inPools && takeExtension p `elem` [".rs", ".cpp", ".hpp", ".h", ".py", ".R", ".toml"]]

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
-- test in flight. C++ pools are built with @-ffp-contract=off@: compilers
-- that fuse @a*b+c@ by default (clang, and GCC on arm64) round once where
-- x86-64 rounds twice, so a golden float would depend on the host. Both
-- defaults apply unless the caller has already set @MORLOC_LANG_PARAMS@; a
-- test that must exercise the shipped profile passes @-X rust:lto=thin@ in
-- its Makefile, which outranks the environment.
--
-- Where the system has no @timeout@ (macOS ships none), @shims@ goes on PATH
-- so the tests that bound a run with it still run it.
--
-- Every build checks its ownership decisions (@MORLOC_CHECK_OWNERSHIP=1@)
-- unless the caller set the variable.
--
-- Returns make's stderr. It is collected outside the test directory because
-- most recipes begin with @rm -f *.err@.
runQuietly :: FilePath -> [String] -> IO BS.ByteString
runQuietly shims args = do
  env <- getEnvironment
  hasTimeout <- maybe False (const True) <$> findExecutable "timeout"
  let env0 = case lookup "MORLOC_CHECK_OWNERSHIP" env of
        Just _ -> env
        Nothing -> ("MORLOC_CHECK_OWNERSHIP", "1") : env
      env' = case lookup langParamsVar env0 of
        Just _ -> env0
        Nothing -> (langParamsVar, "rust:lto=off;cpp:flags=-ffp-contract=off") : env0
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
