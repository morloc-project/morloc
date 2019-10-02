module UnitManifoldTests
  ( unitManifoldTests
  ) where

import Morloc.Connect (connect)
import Morloc.Namespace
import Test.Tasty
import Test.Tasty.HUnit
import qualified Morloc.Config as Config
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Parser.API as P
import qualified Morloc.TypeChecker.API as T

unitManifoldTests :: IO TestTree
unitManifoldTests = do
  ms <- loadManifolds "test-suite/tests-data/partial-01.loc"
  return $ testGroup "Manifold tests" [tests ms]

loadManifolds ::
     Path
  -> IO [Manifold]
loadManifolds path = do
  code <- MT.readFile (MT.unpack path)
  config <- Config.loadMorlocConfig Nothing -- default config
  MM.evalMorlocMonad
    config
    (P.parse (Just path) code >>= T.typecheck >>= connect)

tests :: [Manifold] -> TestTree
tests ms =
  testGroup
    "Checking manifolds compiled from test file 'partial-01.loc'" 
    [ testCase "testing testing" $ assertEqual "" 1 1 ]
