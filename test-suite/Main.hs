import Test.Tasty
import qualified System.Directory as SD

import PropertyTypeTests (propertyTypeTests)
import UnitTypeTests (typeOrderTests, unitTypeTests)
import GoldenMakefileTests (goldenMakefileTest)

main = do
  wd <- SD.getCurrentDirectory >>= SD.makeAbsolute
  let golden = \msg f -> goldenMakefileTest msg (wd ++ "/test-suite/golden-tests/" ++ f)
  defaultMain $
    testGroup
      "Morloc tests"
      [ unitTypeTests
      , typeOrderTests
      , propertyTypeTests
      , golden "simple C++" "1_cpp"
      , golden "simple R" "2_r"
      , golden "basic math" "3_math"
      , golden "C++ map function" "4_map"
      , golden "nested C++ map functions" "5_nested"
      , golden "C++ / R interop" "6_interop"
      , golden "C++ quadratic eqation" "7_quadraticEq"
      ]
