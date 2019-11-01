import Test.Tasty

import PropertyTypeTests (propertyTypeTests)
import UnitTypeTests (unitTypeTests)
import GoldenMakefileTests (goldenMakefileTest)

main = do
  defaultMain $
    testGroup
      "Morloc tests"
      [ goldenMakefileTest "Golden test #1" "test-suite/tests-data"
      , unitTypeTests
      , propertyTypeTests
      ]
