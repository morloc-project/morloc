{- |
Module      : ValueArgsTests
Description : Unit tests for deciding which arguments of a type are values
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module ValueArgsTests (valueArgsTests, kindOfTests) where

import qualified Data.Map as Map
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.ValueArgs (appValueArgs, valueArgsF, valueArgsT)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

declared :: TVar -> [Either (TVar, Kind) TypeU] -> (TVar, [([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool, TypedefKind)])
declared v ps = (v, [(ps, VarU v, ArgDocAlias defaultValue, True, TypedefAlias)])

gscope :: Scope
gscope =
  Map.fromList
    [ declared (TV "Table") [Left (TV "n", KindNat), Left (TV "r", KindRec)]
    , declared (TV "Frame") [Left (TV "r", KindRec), Left (TV "a", KindType)]
    , declared (TV "FixedPair") [Left (TV "n", KindNat), Left (TV "a", KindType)]
    ]

overloaded :: Scope
overloaded =
  Map.fromList
    [ ( TV "Frame"
      , [ ([Left (TV "a", KindType)], VarU (TV "Frame"), ArgDocAlias defaultValue, True, TypedefAlias)
        , ([Left (TV "r", KindRec), Left (TV "a", KindType)], VarU (TV "Frame"), ArgDocAlias defaultValue, True, TypedefAlias)
        ]
      )
    ]

bare :: Scope
bare = Map.fromList [declared (TV "Table") []]

int :: TypeF
int = VarF (FV (TV "Int") (CV "i64"))

real :: TypeF
real = VarF (FV (TV "Real") (CV "f64"))

row :: TypeF
row = NamF NamRecord (FV anonRecordVar (CV "@REC")) [] [(Key "x", int)]

rowT :: Type
rowT = NamT NamRecord anonRecordVar [] [(Key "x", VarT (TV "Int"))]

valueArgsTests :: TestTree
valueArgsTests =
  testGroup
    "valueArgs (kind-level arguments are not values, KIND-2)"
    [ testCase "a table's dimension and row are not values" $
        valueArgsF gscope (TV "Table") [NatVoidF, row] @?= []
    , testCase "a ground row is dropped where a polymorphic one would be" $
        valueArgsF gscope (TV "Frame") [row, real] @?= [real]
    , testCase "a value at a kind-level position is kept (wire shapes do not line up)" $
        valueArgsF gscope (TV "FixedPair") [int, int] @?= [int, int]
    , testCase "an argument list matching no declared arity drops kind shapes" $
        valueArgsF gscope (TV "Frame") [NatVoidF, row, real] @?= [real]
    , testCase "a row is kept under a mismatched head with no Rec parameter" $
        valueArgsF gscope (TV "FixedPair") [row, int, int] @?= [row, int, int]
    , testCase "a definition is chosen by arity" $
        valueArgsF overloaded (TV "Frame") [row, real] @?= [real]
    , testCase "an undeclared head drops only kind-shaped arguments" $
        valueArgsF gscope (TV "Foo") [NatLitF 3, row, int] @?= [row, int]
    , testCase "an undeclared table head drops its row" $
        valueArgsF Map.empty (TV "Table") [NatVoidF, row] @?= []
    , testCase "an application is decided by its head" $
        appValueArgs gscope (VarF (FV (TV "Table") (CV "arrow"))) [NatVoidF, row] @?= []
    , testCase "the Type form agrees" $
        valueArgsT gscope (TV "Table") [NatVoidT, rowT] @?= []
    , testCase "a table head declared without parameters still drops its row" $
        valueArgsF bare (TV "Table") [NatVoidF, row, int] @?= [int]
    , testCase "the Type form drops labels as the TypeF form does" $
        ( valueArgsT Map.empty (TV "Foo") [StrLitT "lbl", StrVoidT, VarT (TV "Int")]
        , valueArgsF Map.empty (TV "Foo") [StrLitF "lbl", StrVoidF, int] )
          @?= ([VarT (TV "Int")], [int])
    ]

kindOfTests :: TestTree
kindOfTests =
  testGroup
    "the kind of an argument is fixed by its form (KIND-1)"
    [ testCase "erasure keeps the kind of a placeholder" $
        map (kindOfT . typeOf) [RecVarU (TV "r"), ListVarU (TV "l"), SetVarU (TV "s"), StrVarU (TV "t"), NatVarU (TV "n")]
          @?= map Just [KindRec, KindList KindStr, KindSet KindStr, KindStr, KindNat]
    , testCase "a placeholder lifts back at its own kind" $
        map (type2typeu . typeOf) [RecVarU (TV "r"), ListVarU (TV "l"), SetVarU (TV "s")]
          @?= [RecVoidU, ListVoidU, SetVoidU]
    , testCase "a Str argument is a kind argument at every level" $
        ( partitionKindArgs kindOfT [StrLitT "id", NatLitT 1, VoidT KindRec, VarT (TV "Real")]
        , partitionKindArgs kindOfF [StrLitF "id", NatLitF 1, VoidF KindRec, real]
        , partitionKindArgs kindOfU [StrLitU "id", NatLitU 1, RecVarU (TV "r"), VarU (TV "Real")] )
          @?= (([VarT (TV "Real")], 3), ([real], 3), ([VarU (TV "Real")], 3))
    , testCase "a ground row's kind depends on its parameter, an open row's does not" $
        ( kindOfU (RecExtendU "x" (VarU (TV "Int")) RecEmptyU)
        , kindOfU (RecExtendU "x" (VarU (TV "Int")) (RecVarU (TV "r"))) )
          @?= (Nothing, Just KindRec)
    , testCase "a Rec placeholder is not a Nat" $
        kindOfT (typeOf (RecVarU (TV "r"))) /= Just KindNat @? "Rec placeholder read as Nat"
    ]
