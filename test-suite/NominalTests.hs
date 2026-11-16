{-# LANGUAGE OverloadedStrings #-}

-- | The keys code generation reads off declared types (NOM-1).
module NominalTests (nominalTests) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Nominal
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

int, str :: TypeF
int = VarF (FV (TV "Int") (CV "int"))
str = VarF (FV (TV "Str") (CV "std::string"))

container :: T.Text -> TypeF -> TypeF
container cv a = NamF NamRecord (FV (TV "Container") (CV cv)) [TyA a] [(Key "val", a), (Key "sub", OptionalF (RecF (FV (TV "Container") (CV cv)) [TyA a]))]

nominalTests :: TestTree
nominalTests =
  testGroup
    "NOM keys"
    [ testCase "a record's key is its name and arguments" $
        declaredKey (container "struct" int) @?= Just (NomKey (TV "Container") [KTy (KVar (TV "Int"))] NativeLevel)
    , testCase "the native form takes no part in a key" $
        declaredKey (container "struct" int) @?= declaredKey (container "dict" int)
    , testCase "other arguments are another key" $
        assertBool "keys equal" (declaredKey (container "struct" int) /= declaredKey (container "struct" str))
    , testCase "a nested declared type is its key" $
        declaredKey (container "struct" (container "struct" int))
          @?= Just (NomKey (TV "Container") [KTy (KNom (NomKey (TV "Container") [KTy (KVar (TV "Int"))] NativeLevel))] NativeLevel)
    , testCase "a tied back-reference has its record's key" $
        declaredKey (RecF (FV (TV "Container") (CV "struct")) [TyA int]) @?= declaredKey (container "struct" int)
    , testCase "a data back-edge without arms has its instance's key" $
        let choice as = VariantF (FV (TV "Choice") (CV "Choice<$1>")) [TyA int] as
         in declaredKey (choice []) @?= declaredKey (choice [("A", [int])])
    , testCase "an anonymous record has no key and compares by fields" $ do
        let anon f = NamF NamRecord (FV anonRecordVar (CV "@REC")) [] [(Key f, int)]
        declaredKey (anon "x") @?= Nothing
        assertBool "fields ignored" (typeFKey (anon "x") /= typeFKey (anon "y"))
    , testCase "a type named by its morloc name alone is not read as declared" $
        declaredKey (AppF (VarF (FV (TV "Container") (CV "Container"))) [TyA int]) @?= Nothing
    ]
