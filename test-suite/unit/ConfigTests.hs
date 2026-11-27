{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ConfigTests
Description : Unit tests for reading the morloc config file
-}
module ConfigTests (configTests) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml
import Morloc.Namespace.State (Config (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

decodeConfig :: BS.ByteString -> Either Yaml.ParseException Config
decodeConfig = Yaml.decodeEither'

configTests :: TestTree
configTests =
  testGroup
    "config file"
    [ testCase "lang_overrides sets a language's command" $
        case decodeConfig "lang_overrides:\n  py: [/opt/py/bin/python3]\n" of
          Left e -> assertFailure (show e)
          Right c -> configLangOverrides c @?= Map.fromList [("py", ["/opt/py/bin/python3"])]
    , testCase "the removed lang_python3 and lang_R keys are not read" $
        case decodeConfig "lang_python3: /opt/py/bin/python3\nlang_R: /opt/r/bin/R\n" of
          Left e -> assertFailure (show e)
          Right c -> configLangOverrides c @?= Map.empty
    ]
