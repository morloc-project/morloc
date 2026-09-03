module Main (main) where

import System.Directory (getHomeDirectory, makeAbsolute)
import System.Environment (getArgs, lookupEnv, withArgs)
import System.FilePath ((</>))
import Test.Tasty (defaultMain, testGroup)

import Morloc.Test.Common (TestEnv (..))
import Morloc.Test.ConcurrencyTests (concurrencyTests)
import Morloc.Test.DaemonTests (daemonTests)
import Morloc.Test.InstallTests (installTests)
import Morloc.Test.McpTests (mcpTests)
import Morloc.Test.ShmTests (shmTests)
import Morloc.Test.StressTests (stressTests)

main :: IO ()
main = do
  suiteDir <- makeAbsolute "test-suite"
  home <- getHomeDirectory
  morlocHome <- maybe (home </> ".local/share/morloc") id <$> lookupEnv "MORLOC_HOME"
  -- Mirrors Morloc.Config.resolveStateRoots: $MORLOC_STATE overrides, and
  -- defaults to the home prefix so a plain host install stays one directory.
  morlocState <- maybe morlocHome id <$> lookupEnv "MORLOC_STATE"
  let env =
        TestEnv
          { teSuiteDir = suiteDir
          , teMorlocHome = morlocHome
          , teMorlocState = morlocState
          }
  -- Default to sequential execution: stress tests measure global resources
  -- (SHM segments in /dev/shm) and cannot run concurrently with other tests.
  -- Override with: --test-arguments="--num-threads N"
  args <- getArgs
  let hasNumThreads =
        any
          ( \a ->
              "--num-threads" == a
                || "-j" == a
                || take 14 a == "--num-threads="
                || take 3 a == "-j="
          )
          args
      args' = if hasNumThreads then args else "--num-threads" : "1" : args
  withArgs args' $
    defaultMain $
      testGroup
        "Integration Tests"
        [ installTests env
        , concurrencyTests env
        , daemonTests env
        , mcpTests env
        , stressTests env
        , shmTests env
        ]
