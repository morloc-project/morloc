import Test.Tasty
import qualified System.Directory as SD

import PropertyTypeTests (propertyTypeTests)
import UnitTypeTests (unitTypeTests)
import GoldenMakefileTests (goldenMakefileTest)

main = do
  wd <- SD.getCurrentDirectory >>= SD.makeAbsolute
  let golden = \msg f -> goldenMakefileTest msg (wd ++ "/test-suite/golden-tests/" ++ f)
  defaultMain $
    testGroup
      "Morloc tests"
      [ golden "Golden test for C++" "1_cpp"
      , golden "Golden test for R" "2_r"
      , golden "Golden test for math" "3_math"
      , unitTypeTests
      , propertyTypeTests
      ]
