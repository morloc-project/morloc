module Main (main) where

import System.Directory (makeAbsolute, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty (defaultMain, testGroup)

import Morloc.Test.Common (TestEnv(..))
import Morloc.Test.InstallTests (installTests)

main :: IO ()
main = do
  suiteDir <- makeAbsolute "test-suite"
  home <- getHomeDirectory
  morlocHome <- maybe (home </> ".local/share/morloc") id <$> lookupEnv "MORLOC_HOME"
  let env = TestEnv
        { teSuiteDir   = suiteDir
        , teMorlocHome = morlocHome
        }
  defaultMain $ testGroup "Integration Tests"
    [ installTests env
    ]
