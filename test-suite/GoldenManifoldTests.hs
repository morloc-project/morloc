module GoldenManifoldTests
  ( goldenManifoldTest01
  ) where

import Morloc.Connect (connect)
import Test.Tasty
import Test.Tasty.Golden
import qualified Morloc.Config as Config
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Parser.API as P
import qualified Morloc.TypeChecker.API as T

goldenManifoldTest01 :: TestTree
goldenManifoldTest01 =
  let locFile = "test-suite/tests-data/partial-01.loc"
      expFile = "test-suite/tests-data/exp-partial-01.txt"
      obsFile = "test-suite/tests-data/obs-partial-01.txt"
  in
      goldenVsFile
        "Golden test for pretty manifolds of partial-O1.loc"
        expFile
        obsFile
        (makeManifoldFile locFile obsFile)

makeManifoldFile :: String -> String -> IO ()
makeManifoldFile locFile obsFile = do
  code <- MT.readFile locFile
  config <- Config.loadMorlocConfig Nothing -- default config
  ms <- MM.evalMorlocMonad 0 config
    (P.parse (Just (MT.pack locFile)) code >>= T.typecheck >>= connect)
  writeBinaryFile obsFile (MT.unpack $ MT.pretty ms)
