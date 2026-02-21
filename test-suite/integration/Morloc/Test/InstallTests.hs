module Morloc.Test.InstallTests (installTests) where

import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

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
      binDir = teMorlocHome env </> "bin"
      exeDir = teMorlocHome env </> "exe"
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

    -- Check binary and exe directory exist
    assertFileExists (name ++ ": binary installed") binPath
    assertDirExists (name ++ ": exe directory created") exePath
    assertDirExists (name ++ ": pools directory copied") (exePath </> "pools")

    -- Check expected files
    mapM_
      (\f -> assertFileExists (name ++ ": " ++ f ++ " included") (exePath </> f))
      (isFiles spec)

    -- Check expected directories
    mapM_
      (\d -> assertDirExists (name ++ ": " ++ d ++ " included") (exePath </> d))
      (isDirs spec)

    -- Run the installed program
    (rc, stdout, stderr) <- runProgram binPath (isSubcommand spec) (isArgs spec)
    let actual = case rc of
          ExitSuccess -> strip stdout
          ExitFailure c -> "ERROR: rc=" ++ show c ++ "\n" ++ stderr
    assertEqual (name ++ ": output") (isExpected spec) actual

    -- Uninstall and verify cleanup
    morlocUninstall name
    assertNotExists (name ++ ": binary removed after uninstall") binPath

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
              , isArgs = ["\"world\""]
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
              , isArgs = ["\"x\"", "5"]
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
              , isArgs = ["\"foo\"", "\"bar\""]
              , isExpected = "\"foobar\""
              , isFiles = ["glue.R", "rutil.R"]
              , isDirs = []
              }
        ]
    ]
