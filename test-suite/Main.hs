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
      , golden "(1) basic C functions" "1_basic_c"
      , golden "(1) basic Python3 functions" "1_basic_py"
      , golden "(1) basic R functions" "1_basic_r"

      , golden "(2) C composition" "2_comp_c"
      , golden "(2) Python3 composition" "2_comp_py"
      , golden "(2) R composition" "2_comp_r"

      , golden "(3) zipWith hof in C" "3_hof_c"
      , golden "(3) zipWith hof in Python3" "3_hof_py"
      , golden "(3) zipWith hof in R" "3_hof_r"

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
