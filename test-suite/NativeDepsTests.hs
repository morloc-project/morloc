{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : NativeDepsTests
-- Description : Spec tests of native dependency declarations
--               (model/language/packages.md PKG-12 to PKG-17) that are
--               decided when package.yaml is read, and of the prefix flags
--               a managed native build adds.
module NativeDepsTests
  ( nativeDepsTests
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import Morloc.CodeGenerator.Platform (Platform (..))
import Morloc.CodeGenerator.SystemConfig (prefixFlags)
import qualified Morloc.Module as Mod
import Morloc.Namespace.State (DepImport (..), PackageMeta (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

parse :: [String] -> Either String PackageMeta
parse ls = either (Left . show) Right (Yaml.decodeEither' (BS.pack (unlines ("name: m" : ls))))

refusedWith :: [String] -> [String] -> Assertion
refusedWith needles ls = case parse ls of
  Left e -> mapM_ (\n -> assertBool ("diagnostic lacks " <> show n <> ": " <> e) (n `isInfixOf` e)) needles
  Right _ -> assertFailure ("expected refusal of " <> show ls)

parsed :: [String] -> IO PackageMeta
parsed ls = either (\e -> assertFailure e >> fail e) return (parse ls)

nativeDepsTests :: TestTree
nativeDepsTests =
  testGroup
    "native dependencies"
    [ testCase "spec-pkg-12-1" $
        refusedWith ["dependencies", "libs", "tool-deps", "cxx-flags"] ["dependencies: [hts]"]
    , testCase "spec-pkg-12-2" $
        refusedWith ["dependencies"] ["dependencies: []"]
    , testCase "spec-pkg-12-3" $
        refusedWith ["htslib", "lib"] ["cpp-deps:", "  htslib: {version: \"*\", lib: [hts]}"]
    , testCase "spec-pkg-12-4" $ do
        mapM_
          (\l -> refusedWith ["htslib"] ["cpp-deps:", "  htslib: {libs: [" <> l <> "]}"])
          ["\"-lhts\"", "libhts", "hts.so", "libhts.a", "hts.dylib", "\"h ts\""]
        mapM_
          (\h -> refusedWith ["htslib"] ["cpp-deps:", "  htslib: {import: " <> h <> "}"])
          ["\"<htslib/hts.h>\"", "\"a b.h\"", "'q\"h.h'"]
    , testCase "spec-pkg-12-5" $ do
        m <- parsed ["cpp-deps:", "  htslib: {version: \">=1.20\", libs: [hts]}", "  zlib: {libs: [z]}"]
        packageCppLibs m @?= Map.fromList [("htslib", ["hts"]), ("zlib", ["z"])]
        Map.member "htslib" (packageCppDeps m) @?= True
    , testCase "spec-pkg-12-6" $ do
        m <- parsed ["cpp-deps:", "  htslib: {import: htslib/hts.h}"]
        Map.lookup "htslib" (Map.findWithDefault Map.empty "cpp" (packageDepImports m))
          @?= Just (DepImport Nothing ["htslib/hts.h"])
        refusedWith ["namespace", "import"] ["cpp-deps:", "  nlohmann_json: {namespace: nlohmann}"]
    , testCase "spec-pkg-16-2" $ do
        m <- parsed ["tool-deps:", "  star: {version: \"*\", channel: bioconda, bin: [STAR]}"]
        packageToolBins m @?= Map.fromList [("star", ["STAR"])]
        refusedWith ["star", "bin"] ["tool-deps:", "  star: {bin: [bin/STAR]}"]
        refusedWith ["star", "bin"] ["tool-deps:", "  star: {bin: [\"S TAR\"]}"]
        refusedWith ["star", "bins"] ["tool-deps:", "  star: {bins: [STAR]}"]
    , testCase "a new project's package.yaml parses" $
        case Yaml.decodeEither' (BS.pack (Mod.newPackageYaml "proj")) :: Either Yaml.ParseException PackageMeta of
          Left e -> assertFailure (show e)
          Right _ -> return ()
    , testCase "prefix flags on Linux keep DT_RPATH and quote the prefix" $
        prefixFlags Linux "/a b,c'd"
          @?= ( [ "-isystem", "'/a b,c'\\''d/include'"
                , "-L", "'/a b,c'\\''d/lib'"
                , "-Xlinker", "-rpath", "-Xlinker", "'/a b,c'\\''d/lib'"
                , "-Xlinker", "--disable-new-dtags"
                ] :: [Text] )
    , testCase "prefix flags on macOS add no dtags flag" $
        prefixFlags Darwin "/p"
          @?= ( ["-isystem", "'/p/include'", "-L", "'/p/lib'", "-Xlinker", "-rpath", "-Xlinker", "'/p/lib'"] :: [Text] )
    ]
