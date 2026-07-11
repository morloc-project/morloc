{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : SchemaHintTests
Description : Unit tests for CLI wire-schema hint peeling and classification
-}
module SchemaHintTests (schemaHintTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Morloc.CodeGenerator.Docstrings
  ( SchemaCat (..)
  , classifySchema
  , peelHint
  )

schemaHintTests :: TestTree
schemaHintTests = testGroup "CLI schema hint"
  [ peelHintTests
  , classifySchemaTests
  ]

peelHintTests :: TestTree
peelHintTests = testGroup "peelHint"
  [ testCase "no hint" $
      peelHint "at2sj" @?= "at2sj"

  , testCase "flat hint" $
      peelHint "<list>ai4" @?= "ai4"

  , testCase "python Dict hint (brackets, no nested angles)" $
      peelHint "<Dict[$1,$2]>at2sj" @?= "at2sj"

    -- Regression: `<std::map<$1,$2>>at2sj` used to peel only through the
    -- inner `>`, leaving `>at2sj` and misrouting Map to CatOtherCompound.
    -- That rejected `form: list` on `Map Str Int` under map-cpp.
  , testCase "cpp map hint with nested angle brackets" $
      peelHint "<std::map<$1,$2>>at2sj" @?= "at2sj"

  , testCase "cpp nested template two deep" $
      peelHint "<std::map<$1,std::vector<$2>>>at2sj" @?= "at2sj"

  , testCase "stacked hints peel all layers" $
      peelHint "<outer><inner>j" @?= "j"

  , testCase "unbalanced hint is left intact" $
      peelHint "<oops" @?= "<oops"
  ]

classifySchemaTests :: TestTree
classifySchemaTests = testGroup "classifySchema"
  [ testCase "scalar prim" $
      classifySchema "i4" @?= CatScalarPrim

  , testCase "str" $
      classifySchema "s" @?= CatStr

  , testCase "plain list" $
      classifySchema "ai4" @?= CatList "i4"

  , testCase "python Map wire schema classifies as list" $
      classifySchema "<Dict[$1,$2]>at2sj" @?= CatList "t2sj"

    -- Regression pin for the C++ template hint peel bug.
  , testCase "cpp Map wire schema classifies as list" $
      classifySchema "<std::map<$1,$2>>at2sj" @?= CatList "t2sj"

  , testCase "cpp nested Map>Vector still classifies as list" $
      classifySchema "<std::map<$1,std::vector<$2>>>at2sj" @?= CatList "t2sj"

  , testCase "optional list still classifies as list" $
      classifySchema "?ai4" @?= CatList "i4"

  , testCase "tuple schema is other compound" $
      classifySchema "t2sj" @?= CatOtherCompound
  ]
