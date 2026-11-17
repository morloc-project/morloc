{- |
Module      : GenDefsTests
Description : Generated record definitions and their uses (GEN-9)
-}
{-# LANGUAGE OverloadedStrings #-}
module GenDefsTests (genDefsTests) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Morloc.CodeGenerator.GenDefs (Disagreement (..), GenDef (..), checkUse, instantiate, nativeParams, placeTypes, untiedSelf)
import Morloc.CodeGenerator.Namespace
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

var :: TVar -> CVar -> TypeF
var g c = VarF (FV g c)

pa :: TypeF
pa = var (TV "&a") (CV "T_a")

int64, str :: TypeF
int64 = var (TV "Int") (CV "int64_t")
str = var (TV "Str") (CV "std::string")

-- `Date` is packed: native int64, wire Str.
dateNative :: TypeF
dateNative = var (TV "Date") (CV "int64_t")

pairOf :: [TypeF] -> TypeF -> TypeF
pairOf ps t = NamF NamRecord (FV (TV "Pair") (CV "struct")) (map TyA ps) [(Key "fst", t), (Key "snd", t)]

pairDef, outerDef :: GenDef
pairDef = GenDef (TV "Pair") "struct" [(TV "a", KindType)] [(Key "fst", pa), (Key "snd", pa)]
outerDef = GenDef (TV "Outer") "struct" [(TV "a", KindType)] [(Key "p", pairOf [pa] pa), (Key "n", int64)]

-- A file may name the standard library and the builtin scalars.
stdOnly :: T.Text -> Bool
stdOnly f = all ok (T.split (\c -> c `elem` ("<>, " :: String)) f)
  where
    ok w = T.null w || "std::" `T.isPrefixOf` w || w `elem` ["int64_t", "int", "double"]

-- record Tree where lab :: Pair Int; kids :: [Tree]
treeDef :: GenDef
treeDef = GenDef (TV "Tree") "struct" [] [(Key "lab", pairOf [int64] int64), (Key "kids", listOf (RecF (FV (TV "Tree") (CV "struct")) []))]

listOf :: TypeF -> TypeF
listOf t = AppF (var (TV "List") (CV "std::vector<$1>")) [TyA t]

-- A Tree occurrence, expanded where the definition refers back to Tree.
leafTree :: TypeF
leafTree = NamF NamRecord (FV (TV "Tree") (CV "struct")) [] [(Key "lab", pairOf [int64] int64), (Key "kids", listOf (RecF (FV (TV "Tree") (CV "struct")) []))]

-- record Cpp => UPair a = "upair_t<$1>"
userPairDef :: GenDef
userPairDef = GenDef (TV "UPair") "upair_t<$1>" [(TV "a", KindType)] [(Key "fst", pa), (Key "snd", pa)]

-- record Cpp => Holder a = "holder_t<$1>", holding a generated Pair a
holderDef :: GenDef
holderDef = GenDef (TV "Holder") "holder_t<$1>" [(TV "a", KindType)] [(Key "x", pa), (Key "g", pairOf [pa] pa)]

genDefsTests :: TestTree
genDefsTests =
  testGroup
    "GEN-9 definitions and uses"
    [ testCase "a use at its arguments agrees with its definition" $
        checkUse pairDef [TyA int64] [(Key "fst", int64), (Key "snd", int64)] @?= Nothing
    , testCase "a use whose field is not its definition's at its arguments is reported" $
        assertDiffers (checkUse pairDef [TyA int64] [(Key "fst", int64), (Key "snd", str)])
    , testCase "a use carrying another number of arguments is reported" $
        assertUnmatched (checkUse pairDef [] [(Key "fst", int64), (Key "snd", int64)])
    , testCase "a use with its fields in another order is reported" $
        assertUnmatched (checkUse pairDef [TyA int64] [(Key "snd", int64), (Key "fst", int64)])
    , testCase "a use missing a field is reported" $
        assertUnmatched (checkUse pairDef [TyA int64] [(Key "fst", int64)])
    , testCase "a recursive use whose field refers to the use agrees" $
        checkUse treeDef [] [(Key "lab", pairOf [int64] int64), (Key "kids", listOf (RecF (FV (TV "Tree") (CV "struct")) []))] @?= Nothing
    , testCase "a recursive record unrolled one level deeper agrees" $
        checkUse treeDef [] [(Key "lab", pairOf [int64] int64), (Key "kids", listOf leafTree)] @?= Nothing
    , testCase "a nested record is compared at its arguments, not its fields" $ do
        checkUse outerDef [TyA str] [(Key "p", pairOf [str] str), (Key "n", int64)] @?= Nothing
        assertDiffers (checkUse outerDef [TyA str] [(Key "p", pairOf [int64] int64), (Key "n", int64)])
    , testCase "a field's Nat argument is not compared" $
        let arr k t = AppF (var (TV "Arr") (CV "std::vector<$1>")) [KiA k, TyA t]
            def = GenDef (TV "W") "struct" [(TV "a", KindType)] [(Key "xs", arr (NatLitK 3) pa)]
         in checkUse def [TyA int64] [(Key "xs", AppF (var (TV "Arr") (CV "std::vector<$1>")) [TyA int64])] @?= Nothing
    , testCase "an unpacked user-mapped use agrees at its unpacked arguments" $
        checkUse userPairDef [TyA str] [(Key "fst", str), (Key "snd", str)] @?= Nothing
    , testCase "a parameter held at two levels is reported" $
        -- A user-mapped record holding a generated one, unpacked: its own
        -- field is unpacked, the generated record's stays native.
        assertDiffers (checkUse holderDef [TyA str] [(Key "x", str), (Key "g", pairOf [dateNative] dateNative)])
    , testCase "a reference of a record to itself by name is found" $
        untiedSelf (NamF NamRecord (FV (TV "Tree") (CV "struct")) [] [(Key "kids", listOf (var (TV "Tree") (CV "Tree")))]) @?= Just (TV "Tree", True)
    , testCase "a reference of a record to itself at other arguments is found, after a missed tie" $
        let pairS = FV (TV "Pair") (CV "struct")
            atInt = AppF (var (TV "Pair") (CV "Pair")) [TyA int64]
            atOwn = AppF (var (TV "Pair") (CV "Pair")) [TyA str]
         in do
              untiedSelf (NamF NamRecord pairS [TyA str] [(Key "x", atInt)]) @?= Just (TV "Pair", False)
              untiedSelf (NamF NamRecord pairS [TyA str] [(Key "x", atInt), (Key "y", atOwn)]) @?= Just (TV "Pair", True)
    , testCase "a tied reference and a reference to another record are not untied" $
        untiedSelf (NamF NamRecord (FV (TV "Tree") (CV "struct")) [] [(Key "kids", listOf (RecF (FV (TV "Tree") (CV "struct")) [])), (Key "o", var (TV "Other") (CV "Other"))]) @?= Nothing
    , testCase "a parameter only a nested record's arguments carry is not native" $
        nativeParams
          (GenDef (TV "Holder") "struct" [(TV "a", KindType)]
             [(Key "t", NamF NamRecord (FV (TV "Tag") (CV "struct")) [TyA pa] [(Key "n", int64)]), (Key "k", int64)])
          @?= []
    , testCase "placement: a record naming a sourced type is private, and so is one holding it" $
        placeTypes stdOnly (Set.fromList [TV "Wrap", TV "Outer", TV "Plain"])
          (Map.fromList
             [ (TV "Wrap", [var (TV "Point") (CV "Point")])
             , (TV "Outer", [NamF NamRecord (FV (TV "Wrap") (CV "struct")) [] []])
             , (TV "Plain", [int64, pa])
             ])
          @?= Set.fromList [TV "Plain"]
    , testCase "placement: a native form's slots are checked on their own" $
        placeTypes stdOnly (Set.fromList [TV "M"])
          (Map.fromList [(TV "M", [AppF (var (TV "Map") (CV "std::map<$1,$2>")) [TyA str, TyA (var (TV "Point") (CV "Point"))]])])
          @?= Set.empty
    , testCase "placement: a variant whose arm holds a private record is private" $
        placeTypes stdOnly (Set.fromList [TV "Wrap", TV "V"])
          (Map.fromList
             [ (TV "Wrap", [var (TV "Point") (CV "Point")])
             , (TV "V", [NamF NamRecord (FV (TV "Wrap") (CV "struct")) [] [], int64])
             ])
          @?= Set.empty
    , testCase "instantiation replaces every placeholder" $
        instantiate (Map.fromList [(TV "a", str)]) (pairOf [pa] pa) @?= pairOf [str] str
    ]
  where
    assertDiffers d = case d of
      Just (FieldDiffers {}) -> return ()
      other -> assertFailure ("expected a differing field, got " <> show other)
    assertUnmatched d = case d of
      Just (Unmatched _) -> return ()
      other -> assertFailure ("expected an unmatched use, got " <> show other)

