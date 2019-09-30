import Test.Tasty

import PropertyTests (propertyTests)
import UnitTests (unitTests)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]
