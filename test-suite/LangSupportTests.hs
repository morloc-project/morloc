{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : LangSupportTests
Description : Unit tests for the language-support table parsed from requirements.yaml

Verifies that the SHIPPED requirements.yaml files parse and aggregate, that the
session's gaps are encoded (python needs setuptools/pip/numpy, R needs r-bit64,
python capped below the 3.14 break, core toolchain has the C compiler), and pins
the JSON wire format with a directly-constructed minimal table. A drift guard
fails if a new shim language is added to @DataFiles.langSetups@ without a
requirements.yaml entry (or an explicit deferral). The VALUES are provisional, so
structural tests assert presence rather than exact ceilings.
-}
module LangSupportTests (langSupportTests) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Morloc.DataFiles as DF
import Morloc.LangSupport
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

langSupportTests :: TestTree
langSupportTests =
  testGroup
    "LangSupport (requirements.yaml)"
    [ testGroup
        "shipped requirements parse + aggregate"
        [ testCase "core languages present" $ withParsed $ \t -> do
            let langs = lsLanguages t
            assertBool "py" (Map.member "py" langs)
            assertBool "r" (Map.member "r" langs)
            assertBool "cpp" (Map.member "cpp" langs)
            assertBool "rust" (Map.member "rust" langs)
        , testCase "python requires numpy + setuptools + pip (the session's gaps)" $ withParsed $ \t -> do
            let names = reqNames t "py"
            assertBool "numpy" ("numpy" `elem` names)
            assertBool "setuptools" ("setuptools" `elem` names)
            assertBool "pip" ("pip" `elem` names)
        , testCase "R requires r-bit64" $ withParsed $ \t ->
            assertBool "r-bit64" ("r-bit64" `elem` reqNames t "r")
        , testCase "core toolchain provides the C compiler + rust" $ withParsed $ \t -> do
            let names = map pkgName (lsToolchain t)
            assertBool "c-compiler" ("c-compiler" `elem` names)
            assertBool "rust" ("rust" `elem` names)
        , testCase "python runtime is capped below the 3.14 break" $ withParsed $ \t ->
            case Map.lookup "py" (lsLanguages t) >>= leRuntime of
              Just rs -> assertBool "python ceiling <3.14" (T.isInfixOf "<3.14" (rsVersion rs))
              Nothing -> assertFailure "py runtime missing"
        , testCase "pyarrow is an OPTIONAL python requirement; numpy is not" $ withParsed $ \t -> do
            let reqs = maybe [] leRequires (Map.lookup "py" (lsLanguages t))
                opt name = [pkgOptional p | p <- reqs, pkgName p == name]
            opt "pyarrow" @?= [True]
            opt "numpy" @?= [False]
        ]
    , testGroup
        "drift guard vs DataFiles.langSetups"
        [ testCase "every shim language has a requirements entry or is deferred" $ withParsed $ \t ->
            mapM_ (checkCovered t) DF.langSetups
        ]
    , testGroup
        "renderLangSupport (deterministic JSON)"
        [ testCase "minimal table renders exactly" $
            renderLangSupport minimal @?= minimalJson
        ]
    ]
  where
    -- Parse the SHIPPED embedded requirements files (the real data), or fail.
    withParsed :: (LangSupport -> IO ()) -> IO ()
    withParsed k =
      either assertFailure k $
        parseRequirements
          [(n, DF.embededFileText ef) | (n, ef) <- DF.requirementsFiles]
          [(n, DF.embededFileText ef) | (n, ef) <- DF.installScriptFiles]
          (DF.embededFileText DF.requirementsCore)

    reqNames t k = maybe [] (map pkgName . leRequires) (Map.lookup k (lsLanguages t))

    -- langSetup display names (String) -> the canonical short keys (Text).
    canonicalKey :: String -> Text
    canonicalKey n = case n of
      "C++" -> "cpp"
      "python" -> "py"
      "R" -> "r"
      "Julia" -> "julia"
      other -> T.pack other
    -- shim languages morloc can build but that are not yet natively supported
    deferred :: [Text]
    deferred = ["julia"]
    checkCovered t ls =
      let key = canonicalKey (DF.lsName ls)
       in assertBool
            (DF.lsName ls <> " (" <> T.unpack key <> ") must have a requirements.yaml or be deferred")
            (Map.member key (lsLanguages t) || key `elem` deferred)

    minimal =
      LangSupport
        { lsMorlocVersion = "0.0.0"
        , lsToolchain = [PkgReq "c-compiler" "*" Build False]
        , lsLanguages =
            Map.fromList
              [ ("cpp", LangEntry Nothing [PkgReq "cxx-compiler" "*" Build False] Nothing)
              , ( "py"
                , LangEntry
                    (Just (RuntimeSpec "python" ">=3.10,<3.14" (Just "3.12")))
                    [PkgReq "numpy" ">=1.22,<3" Both False, PkgReq "pyarrow" "*" Runtime True]
                    Nothing
                )
              ]
        }
    minimalJson :: Text
    minimalJson =
      "{\"morloc_version\":\"0.0.0\","
        <> "\"toolchain\":[{\"package\":\"c-compiler\",\"constraint\":\"*\",\"phase\":\"build\",\"optional\":false}],"
        <> "\"languages\":{"
        <> "\"cpp\":{\"runtime\":null,\"requires\":[{\"package\":\"cxx-compiler\",\"constraint\":\"*\",\"phase\":\"build\",\"optional\":false}]},"
        <> "\"py\":{\"runtime\":{\"package\":\"python\",\"version\":\">=3.10,<3.14\",\"default\":\"3.12\"},"
        <> "\"requires\":["
        <> "{\"package\":\"numpy\",\"constraint\":\">=1.22,<3\",\"phase\":\"both\",\"optional\":false},"
        <> "{\"package\":\"pyarrow\",\"constraint\":\"*\",\"phase\":\"runtime\",\"optional\":true}"
        <> "]}"
        <> "}}"
