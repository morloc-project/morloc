{- |
Module      : ManifoldNameTests
Description : Unit tests for the names of generated manifold functions
-}
{-# LANGUAGE OverloadedStrings #-}
module ManifoldNameTests (manifoldNameTests) where

import qualified Data.Text as T
import Morloc.CodeGenerator.Grammars.Translator.Generic (manifoldName)
import Morloc.CodeGenerator.LanguageDescriptor
  (LangDescriptor (..), declaredReservedPrefixes, defaultLangDescriptor, loadLangDescriptorFromText)
import Morloc.Data.Doc (render)
import qualified Morloc.DataFiles as DF
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

builtin :: String -> LangDescriptor
builtin lang = case lookup lang DF.langRegistryFiles >>= either (const Nothing) Just . loadLangDescriptorFromText . DF.embededFileText of
  Just d -> d
  Nothing -> error ("no built-in lang.yaml for " <> lang)

r :: LangDescriptor
r = builtin "r"

py :: LangDescriptor
py = builtin "py"

manifoldNameTests :: TestTree
manifoldNameTests =
  testGroup
    "manifold names (generic languages)"
    [ testCase "a name carries the language's prefix" $
        (render (manifoldName r 12), render (manifoldName py 12)) @?= (".__mm_12", "__mm_12")
    , testCase "every built-in language's reserved prefixes decode" $
        mapM_
          (\(n, f) -> either (assertFailure . ((n <> ": ") <>)) (const (return ())) (declaredReservedPrefixes (DF.embededFileText f)))
          DF.langRegistryFiles
    , testCase "a generic pool reserves its manifold names and spells their prefix as lang.yaml does" $
        mapM_ templateAgrees [("py", "_MLC_MANIFOLD_PREFIX = \""), ("r", "MLC_MANIFOLD_PREFIX <- \"")]
    ]

-- The pool template's own spelling of the manifold prefix is the one the
-- compiler emits, and a sourced name cannot take it.
templateAgrees :: (String, T.Text) -> Assertion
templateAgrees (lang, binding) = do
  yaml <- maybe (assertFailure ("no lang.yaml for " <> lang) >> return "") (return . DF.embededFileText) (lookup lang DF.langRegistryFiles)
  desc <- either (\e -> assertFailure e >> return (defaultLangDescriptor "" "")) return (loadLangDescriptorFromText yaml)
  reserved <- either (\e -> assertFailure e >> return []) return (declaredReservedPrefixes yaml)
  let prefix = ldManifoldPrefix desc
      template = DF.embededFileText (DF.poolTemplateGeneric (T.pack lang))
  assertBool (lang <> ": template does not bind " <> T.unpack prefix) (T.isInfixOf (binding <> prefix <> "\"") template)
  assertBool (lang <> ": manifold prefix not reserved") (any (`T.isPrefixOf` prefix) reserved)
