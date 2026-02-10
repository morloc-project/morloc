{-|
Module      : Bench
Description : Benchmark suite for morloc compiler
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

This benchmark suite tracks performance of key compiler components:
- Parser: parsing .loc source files
- Type checker: type inference and checking
- Code generator: nexus and pool generation

Run with: stack bench
Run with options: stack bench --benchmark-arguments '--csv bench-results.csv'
-}

module Main (main) where

import Test.Tasty.Bench
import qualified Data.Text as T
import System.FilePath ((</>))
import qualified System.Directory as SD

import Morloc (typecheckFrontend, typecheck)
import Morloc.Namespace.Prim (Code(..), Defaultable(..))
import Morloc.Namespace.State (Config(..), MorlocMonad, MorlocError)
import qualified Morloc.Monad as MM

-- | Helper to read benchmark test files
readTestFile :: FilePath -> IO Code
readTestFile name = do
  let path = "bench" </> "test-data" </> name
  Code . T.pack <$> readFile path

-- | Create a minimal config for benchmarking
emptyConfig :: IO Config
emptyConfig = do
  home <- SD.getHomeDirectory
  return $ Config
    { configHome        = home <> "/.local/share/morloc"
    , configLibrary     = home <> "/.local/share/src/morloc"
    , configPlane       = "default"
    , configPlaneCore   = "morloclib"
    , configTmpDir      = home <> "/.morloc/tmp"
    , configBuildConfig = home <> "/.morloc/.build-config.yaml"
    , configLangPython3 = "python3"
    , configLangR       = "Rscript"
    }

-- | Run a MorlocMonad action for benchmarking
runBench :: MorlocMonad a -> IO (Either MorlocError a)
runBench action = do
  config <- emptyConfig
  ((result, _), _) <- MM.runMorlocMonad Nothing 0 config defaultValue action
  return result

main :: IO ()
main = defaultMain
  [ bgroup "Parser"
    [ bench "parse-simple" $ whnfIO $ do
        code <- readTestFile "simple.loc"
        runBench (typecheckFrontend Nothing code)

    , bench "parse-interop" $ whnfIO $ do
        code <- readTestFile "interop.loc"
        runBench (typecheckFrontend Nothing code)

    , bench "parse-complex-types" $ whnfIO $ do
        code <- readTestFile "complex-types.loc"
        runBench (typecheckFrontend Nothing code)
    ]

  , bgroup "Type Checker"
    [ bench "typecheck-simple" $ whnfIO $ do
        code <- readTestFile "simple.loc"
        runBench (typecheck Nothing code)

    , bench "typecheck-interop" $ whnfIO $ do
        code <- readTestFile "interop.loc"
        runBench (typecheck Nothing code)

    , bench "typecheck-complex-types" $ whnfIO $ do
        code <- readTestFile "complex-types.loc"
        runBench (typecheck Nothing code)
    ]

  -- Note: Code generation benchmarks commented out as they require
  -- file system access and module initialization
  -- Uncomment after setting up appropriate test environment
  {-
  , bgroup "Code Generation"
    [ bench "generate-simple" $ whnfIO $ do
        code <- readTestFile "simple.loc"
        runBench (writeProgram Nothing code)
    ]
  -}
  ]
