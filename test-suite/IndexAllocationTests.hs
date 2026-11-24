{-# LANGUAGE OverloadedStrings #-}

-- | Code generation names variables, manifolds and expressions by index, so
-- an index it hands out never equals one already in use.
module IndexAllocationTests (indexAllocationTests) where

import Data.List (nub)
import qualified Morloc.Monad as MM
import Morloc.Namespace.Prim (Defaultable (..))
import Morloc.Namespace.State (Config (..), MorlocMonad)
import qualified System.Directory as SD
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

runMM :: MorlocMonad a -> IO a
runMM action = do
  home <- SD.getHomeDirectory
  let cfg =
        Config
          { configHome = home <> "/.local/share/morloc"
          , configState = home <> "/.local/share/morloc"
          , configLibrary = home <> "/.local/share/src/morloc"
          , configTmpDir = home <> "/.morloc/tmp"
          , configBuildConfig = home <> "/.morloc/.build-config.yaml"
          , configLangOverrides = mempty
          }
  ((r, _), _) <- MM.runMorlocMonad Nothing 0 cfg defaultValue action
  either (assertFailure . show) return r

-- | Indices handed out by code generation after the frontend used 0..999.
codegenIndices :: IO [Int]
codegenIndices = runMM $ do
  MM.setCounter 1000
  MM.startCounter
  m1 <- MM.freshManifoldIndex 0
  v1 <- MM.getCounter
  v2 <- MM.getCounter
  m2 <- MM.freshManifoldIndex 0
  vs <- MM.takeFromCounter 3
  m3 <- MM.freshManifoldIndex 0
  v3 <- MM.getCounter
  return ([m1, m2, m3] <> [v1, v2, v3] <> vs)

indexAllocationTests :: TestTree
indexAllocationTests =
  testGroup
    "code generation index allocation"
    [ testCase "variable and manifold indices are above every expression index" $ do
        is <- codegenIndices
        assertBool (show is) (all (>= 1000) is)
    , testCase "no variable index equals a manifold index or another variable" $ do
        is <- codegenIndices
        nub is @?= is
    ]
