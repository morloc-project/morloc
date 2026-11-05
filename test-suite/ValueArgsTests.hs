{- |
Module      : ValueArgsTests
Description : Unit tests for deciding which arguments of a type are values
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module ValueArgsTests (valueArgsTests, kindOfTests) where

import qualified Data.Map as Map
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Infer (retagArg)
import Morloc.CodeGenerator.ValueArgs (argKinds, valueArgsT)
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

-- | The type arguments the code generator keeps: each argument tagged at its
-- declared position, as the weave tags it.
woven :: Scope -> TVar -> [ArgF] -> [TypeF]
woven g v as = tyArgs (zipWith tag (argKinds g v (length as)) as)
  where
    tag sl (TyA t) = retagArg sl t
    tag _ a = a

valueArgsTests :: TestTree
valueArgsTests =
  testGroup
    "valueArgs (kind-level arguments are not values, KIND-2)"
    [ testCase "a table's dimension and row are not values" $
        woven gscope (TV "Table") [KiA NatVoidK, TyA row] @?= []
    , testCase "a ground row is dropped where a polymorphic one would be" $
        woven gscope (TV "Frame") [TyA row, TyA real] @?= [real]
    , testCase "a value at a kind-level position is kept (wire shapes do not line up)" $
        woven gscope (TV "FixedPair") [TyA int, TyA int] @?= [int, int]
    , testCase "an argument list matching no declared arity drops kind shapes" $
        woven gscope (TV "Frame") [KiA NatVoidK, TyA row, TyA real] @?= [real]
    , testCase "a row is kept under a mismatched head with no Rec parameter" $
        woven gscope (TV "FixedPair") [TyA row, TyA int, TyA int] @?= [row, int, int]
    , testCase "a definition is chosen by arity" $
        woven overloaded (TV "Frame") [TyA row, TyA real] @?= [real]
    , testCase "an undeclared head drops only kind-shaped arguments" $
        woven gscope (TV "Foo") [KiA (NatLitK 3), TyA row, TyA int] @?= [row, int]
    , testCase "an undeclared table head drops its row" $
        woven Map.empty (TV "Table") [KiA NatVoidK, TyA row] @?= []
    , testCase "the Type form agrees" $
        valueArgsT gscope (TV "Table") [KiA NatVoidT, TyA rowT] @?= []
    , testCase "a table head declared without parameters still drops its row" $
        woven bare (TV "Table") [KiA NatVoidK, TyA row, TyA int] @?= [int]
    , testCase "the Type form drops labels as the TypeF form does" $
        ( valueArgsT Map.empty (TV "Foo") [KiA (StrLitT "lbl"), KiA StrVoidT, TyA (VarT (TV "Int"))]
        , woven Map.empty (TV "Foo") [KiA (StrLitK "lbl"), KiA (VoidK KindStr), TyA int] )
          @?= ([VarT (TV "Int")], [int])
    ]

kindOfTests :: TestTree
kindOfTests =
  testGroup
    "the kind of an argument is fixed by its form (KIND-1)"
    [ testCase "erasure keeps the kind of a placeholder" $
        map argOf [RecVarU (TV "r"), ListVarU (TV "l"), SetVarU (TV "s"), StrVarU (TV "t"), NatVarU (TV "n")]
          @?= map (KiA . VoidT) [KindRec, KindList KindStr, KindSet KindStr, KindStr, KindNat]
    , testCase "a placeholder lifts back at its own kind" $
        map (argU . argOf) [RecVarU (TV "r"), ListVarU (TV "l"), SetVarU (TV "s")]
          @?= [RecVoidU, ListVoidU, SetVoidU]
    , testCase "a Str argument is a kind argument at every level" $
        ( let as = map argOf [StrLitU "id", NatLitU 1, RecVarU (TV "r"), VarU (TV "Real")] in (tyArgs as, length as - length (tyArgs as))
        , let as = [KiA (StrLitK "id"), KiA (NatLitK 1), KiA (VoidK KindRec), TyA real] in (tyArgs as, length as - length (tyArgs as))
        , partitionKindArgs kindOfU [StrLitU "id", NatLitU 1, RecVarU (TV "r"), VarU (TV "Real")] )
          @?= (([VarT (TV "Real")], 3), ([real], 3), ([VarU (TV "Real")], 3))
    , testCase "a ground row's kind depends on its parameter, an open row's does not" $
        ( kindOfU (RecExtendU "x" (VarU (TV "Int")) RecEmptyU)
        , kindOfU (RecExtendU "x" (VarU (TV "Int")) (RecVarU (TV "r"))) )
          @?= (Nothing, Just KindRec)
    , testCase "an undetermined Nat inside a dimension stays undetermined" $
        containsUnk (typeOf (AppU (VarU (TV "Vector")) [NatAddU (ExistU (TV "e") ([], Open) ([], Open)) (NatLitU 1), VarU (TV "Int")]))
          @? "existential erased from a dimension"
    , testCase "a quantified Nat inside a dimension stays undetermined" $
        containsUnk (typeOf (ForallU (TV "m") (AppU (VarU (TV "Vector")) [NatMulU (VarU (TV "m")) (NatLitU 2), VarU (TV "Int")])))
          @? "bound variable erased from a dimension"
    , testCase "an unreduced projection is an undetermined type, named by itself" $
        case argOf (ProjectFieldU (RecVarU (TV "r")) (StrLitU "x")) of
          TyA (UnkT (TV n)) -> n /= "_" @? "projection named _"
          a -> assertFailure ("projection tagged " <> show a)
    , testCase "a Rec placeholder is not a Nat" $
        argOf (RecVarU (TV "r")) /= KiA NatVoidT @? "Rec placeholder read as Nat"
    ]
