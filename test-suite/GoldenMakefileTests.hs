module GoldenMakefileTests
  ( goldenMakefileTest
  ) where

import Morloc.Connect (connect)
import Test.Tasty
import Test.Tasty.Golden
import qualified Morloc.Config as Config
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Parser.API as P
import qualified Morloc.TypeChecker.API as T
import qualified System.Process as SP
import qualified System.Directory as SD

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
  SD.withCurrentDirectory abspath
    (SP.callProcess "make" [] >> SP.callProcess "make" ["clean"])
