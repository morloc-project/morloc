module GoldenMakefileTests
  ( goldenMakefileTest
  ) where

import Test.Tasty
import Test.Tasty.Golden
import qualified System.Process as SP
import qualified System.Directory as SD
import qualified System.IO as SI

goldenMakefileTest :: String -> String -> TestTree
goldenMakefileTest msg testdir =
  let dir = testdir
      expFile = testdir ++ "/exp.txt"
      obsFile = testdir ++ "/obs.txt"
  in
      goldenVsFile
        msg
        expFile
        obsFile
        (makeManifoldFile dir)

makeManifoldFile :: String -> IO ()
makeManifoldFile path = do
  abspath <- SD.makeAbsolute path
  devnull <- SI.openFile "/dev/null" SI.WriteMode
  _ <- SP.runProcess
    "make" -- command
    ["-C", abspath, "--quiet"] -- arguments
    Nothing -- optional path to working diretory
    Nothing -- optional environment
    Nothing -- stdin handle
    (Just devnull) -- stdout handle
    Nothing -- stderr handle
    >>= SP.waitForProcess

  SP.callProcess "make" ["-C", abspath, "--quiet", "clean"]
