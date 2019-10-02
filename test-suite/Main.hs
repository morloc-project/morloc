import Test.Tasty

import PropertyTypeTests (propertyTypeTests)
import UnitTypeTests (unitTypeTests)
import UnitManifoldTests (unitManifoldTests)

main = do
  manifoldTests <- unitManifoldTests
  defaultMain $
    testGroup "Morloc tests" [unitTypeTests, propertyTypeTests, manifoldTests]
