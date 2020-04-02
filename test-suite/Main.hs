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
      , golden "more basic math" "3_math2"
      , golden "C++ map function" "4_map"
      , golden "C++ more complex map function" "4_map-more"
      , golden "nested C++ map functions" "5_nested"
      , golden "C++ / R interop" "6_interop"
      , golden "C++ quadratic eqation" "7_quadraticEq"
      , golden "C++ currying" "8_partials-cpp"
      , golden "R currying" "8_partials-r"
      , golden "Python currying" "8_partials-py"
      , golden "Export of a simple curried function" "11_curry"
      , golden "interop" "9_minimal-interop"
      , golden "IO test with Unit type" "10_io"
      , golden "IO test with interop over generics" "10_io-interop"
      , golden "Top identity" "12_top-container-identity"
      , golden "Top list" "12_top-container-list"
      , golden "Top tuple" "12_top-container-tuple"
      , golden "Top tuple with app" "12_top-container-app"
      , golden "0th form" "13_t0"
      , golden "1th form" "13_t1"
      , golden "2th form" "13_t2"
      , golden "3th form" "13_t3"
      , golden "4th form C++" "13_t4_c"
      , golden "4th form Python3" "13_t4_py"
      , golden "4th form R" "13_t4_r"
      , golden "5th form C++" "13_t5_c"
      , golden "5th form Python3" "13_t5_py"
      , golden "5th form R" "13_t5_r"
      , golden "6th form C++" "13_t6_c"
      , golden "6th form Python3" "13_t6_py"
      , golden "6th form R" "13_t6_r"
      , golden "7th form C++" "13_t7_c"
      , golden "7th form Python3" "13_t7_py"
      , golden "7th form R" "13_t7_r"
      , golden "Record in R" "14_records-r"
      , golden "Record in Python3" "14_records-py"
      , golden "Record in C++" "14_records-c"
      ]
