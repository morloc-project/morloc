module Morloc.Test.InstallTests (installTests) where

import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import Morloc.ProgramBuilder.Paths (buildDirName, buildMarker, installedManifestPath)
import Morloc.Test.Common

data InstallSpec = InstallSpec
  { isSourceDir :: String -- relative to test-suite/, e.g. "install-tests/testpy1"
  , isExtraArgs :: [String] -- extra morloc make args
  , isSubcommand :: String -- exported function to call
  , isArgs :: [String] -- arguments to pass
  , isExpected :: String -- expected stdout output
  , isFiles :: [String] -- files that must exist in exe dir
  , isDirs :: [String] -- dirs that must exist in exe dir
  }

installTest :: TestEnv -> String -> InstallSpec -> TestTree
installTest env name spec = testCase name $ do
  let srcDir = teSuiteDir env </> isSourceDir spec
      -- The launcher goes on PATH, under the runtime prefix; the install root
      -- it points at is state. See 'TestEnv'.
      binDir = teMorlocHome env </> "bin"
      exeDir = teMorlocState env </> "exe"
      binPath = binDir </> name
      exePath = exeDir </> name

  withTestCopy srcDir $ \workDir -> do
    -- Build and install
    (ec, _out, err) <- morlocInstall workDir name (isExtraArgs spec) "main.loc"
    case ec of
      ExitSuccess -> return ()
      ExitFailure c ->
        assertFailure $
          name ++ ": morloc make failed (exit " ++ show c ++ "):\n" ++ err

    -- The installed root exe/<name> is a marked build root that holds the
    -- program's sourced files; the build artifacts (manifest + pools) sit in
    -- the nested <name>-build/ tree. Verify the whole layout stringently so a
    -- regression to the old flat shape (or a wrong nesting depth) fails here.
    let buildDir = exePath </> buildDirName name
    assertFileExists (name ++ ": binary installed") binPath
    assertDirExists (name ++ ": exe root created") exePath
    assertFileExists (name ++ ": build marker at root") (exePath </> buildMarker)
    assertDirExists (name ++ ": nested build dir created") buildDir
    assertFileExists (name ++ ": manifest in build dir") (installedManifestPath exePath)
    assertDirExists (name ++ ": pools under build dir") (buildDir </> "pools")
    -- The manifest must live only in the nested build dir, never at the root.
    assertNotExists (name ++ ": no stale flat manifest at root") (exePath </> "manifest.json")

    -- Sourced files/dirs are mirrored at the ROOT (beside the build dir), where
    -- a pool resolves them via ../../.. -- not inside the build dir.
    mapM_
      (\f -> assertFileExists (name ++ ": " ++ f ++ " mirrored at root") (exePath </> f))
      (isFiles spec)
    mapM_
      (\d -> assertDirExists (name ++ ": " ++ d ++ " mirrored at root") (exePath </> d))
      (isDirs spec)
    mapM_
      (\f -> assertNotExists (name ++ ": " ++ f ++ " not duplicated in build dir") (buildDir </> f))
      (isFiles spec)

    -- Run the installed program
    (rc, stdout, stderr) <- runProgram binPath (isSubcommand spec) (isArgs spec)
    let actual = case rc of
          ExitSuccess -> strip stdout
          ExitFailure c -> "ERROR: rc=" ++ show c ++ "\n" ++ stderr
    assertEqual (name ++ ": output") (isExpected spec) actual

    -- Uninstall and verify cleanup: both the launcher and the whole install
    -- root (build dir + mirrored sources) must be gone.
    morlocUninstall name
    assertNotExists (name ++ ": binary removed after uninstall") binPath
    assertNotExists (name ++ ": install root removed after uninstall") exePath

installTests :: TestEnv -> TestTree
installTests env =
  testGroup
    "Install"
    [ testGroup
        "Python"
        [ installTest env "testpy1" $
            InstallSpec
              { isSourceDir = "install-tests/testpy1"
              , isExtraArgs = []
              , isSubcommand = "pygreet"
              , isArgs = ["world"]
              , isExpected = "\"hello world\""
              , isFiles = ["helpers.py"]
              , isDirs = []
              }
        , installTest env "testpy2" $
            InstallSpec
              { isSourceDir = "install-tests/testpy2"
              , isExtraArgs = []
              , isSubcommand = "pyadd"
              , isArgs = ["3", "4"]
              , isExpected = "7"
              , isFiles = ["src/mathutil.py"]
              , isDirs = ["src"]
              }
        , installTest env "testpy3" $
            InstallSpec
              { isSourceDir = "install-tests/testpy3"
              , isExtraArgs = ["--include", "formatter.py", "--include", "fmtlib.py"]
              , isSubcommand = "pyformat"
              , isArgs = ["x", "5"]
              , isExpected = "\"x=5\""
              , isFiles = ["formatter.py", "fmtlib.py"]
              , isDirs = []
              }
        ]
    , testGroup
        "Cpp"
        [ installTest env "testcpp1" $
            InstallSpec
              { isSourceDir = "install-tests/testcpp1"
              , isExtraArgs = []
              , isSubcommand = "cppsquare"
              , isArgs = ["7"]
              , isExpected = "49"
              , isFiles = ["square.hpp"]
              , isDirs = []
              }
        , installTest env "testcpp2" $
            InstallSpec
              { isSourceDir = "install-tests/testcpp2"
              , isExtraArgs = []
              , isSubcommand = "cppdouble"
              , isArgs = ["6"]
              , isExpected = "12"
              , isFiles = ["src/dbl.hpp"]
              , isDirs = ["src"]
              }
        , installTest env "testcpp3" $
            InstallSpec
              { isSourceDir = "install-tests/testcpp3"
              , isExtraArgs = ["--include", "inc.hpp", "--include", "offset.hpp"]
              , isSubcommand = "cppinc"
              , isArgs = ["10"]
              , isExpected = "11"
              , isFiles = ["inc.hpp", "offset.hpp"]
              , isDirs = []
              }
        ]
    , testGroup
        "R"
        [ installTest env "testr1" $
            InstallSpec
              { isSourceDir = "install-tests/testr1"
              , isExtraArgs = []
              , isSubcommand = "rnegate"
              , isArgs = ["5.0"]
              , isExpected = "-5"
              , isFiles = ["negate.R"]
              , isDirs = []
              }
        , installTest env "testr2" $
            InstallSpec
              { isSourceDir = "install-tests/testr2"
              , isExtraArgs = []
              , isSubcommand = "rtriple"
              , isArgs = ["4"]
              , isExpected = "12"
              , isFiles = ["src/triple.R"]
              , isDirs = ["src"]
              }
        , installTest env "testr3" $
            InstallSpec
              { isSourceDir = "install-tests/testr3"
              , isExtraArgs = ["--include", "glue.R", "--include", "rutil.R"]
              , isSubcommand = "rpaste"
              , isArgs = ["foo", "bar"]
              , isExpected = "\"foobar\""
              , isFiles = ["glue.R", "rutil.R"]
              , isDirs = []
              }
        ]
    , testGroup
        "Datafile"
        [ installTest env "testdatafile1" $
            InstallSpec
              { isSourceDir = "install-tests/testdatafile1"
              , isExtraArgs = []
              , isSubcommand = "readData"
              , isArgs = []
              , isExpected = "\"hello from datafile\""
              , isFiles = ["reader.py", "data.txt"]
              , isDirs = []
              }
        ]
    ]
