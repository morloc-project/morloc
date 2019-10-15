import Test.Tasty

import PropertyTypeTests (propertyTypeTests)
import UnitTypeTests (unitTypeTests)
import GoldenManifoldTests (goldenManifoldTest01)

main = do
  defaultMain $
    testGroup
      "Morloc tests"
      [ unitTypeTests
      , propertyTypeTests
      -- , goldenManifoldTest01
      ]
