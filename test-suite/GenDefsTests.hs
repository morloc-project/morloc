{- |
Module      : GenDefsTests
Description : Arguments of a generated record's use, inferred from its fields (GEN-9)
-}
{-# LANGUAGE OverloadedStrings #-}
module GenDefsTests (genDefsTests) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Morloc.CodeGenerator.GenDefs (GenDef (..), canon, checkUse, inferArgs, instantiate, nativeParams, placeTypes, recordArgs, untiedSelf, useArgs)
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

boxOf :: [TypeF] -> TypeF -> TypeF
boxOf ps t = VariantF (FV (TV "Box") (CV "Box")) (map TyA ps) [("Empty", []), ("Full", [t])]

-- A file may name the standard library and the builtin scalars.
stdOnly :: T.Text -> Bool
stdOnly f = all ok (T.split (\c -> c `elem` ("<>, " :: String)) f)
  where
    ok w = T.null w || "std::" `T.isPrefixOf` w || w `elem` ["int64_t", "int", "double"]

defs :: Map.Map TVar GenDef
defs = Map.fromList [(gdVar d, d) | d <- [pairDef, outerDef, treeDef]]

-- record Tree where lab :: Pair Int; kids :: [Tree]
treeDef :: GenDef
treeDef = GenDef (TV "Tree") "struct" [] [(Key "lab", pairOf [int64] int64), (Key "kids", listOf (RecF (FV (TV "Tree") (CV "struct")) []))]

listOf :: TypeF -> TypeF
listOf t = AppF (var (TV "List") (CV "std::vector<$1>")) [TyA t]

-- A Tree with no children, and one with a leaf child: one type unrolled
-- to two depths.
leafTree, deepTree :: TypeF
leafTree = NamF NamRecord (FV (TV "Tree") (CV "struct")) [] [(Key "lab", pairOf [int64] int64), (Key "kids", listOf (RecF (FV (TV "Tree") (CV "struct")) []))]
deepTree = NamF NamRecord (FV (TV "Tree") (CV "struct")) [] [(Key "lab", pairOf [int64] int64), (Key "kids", listOf leafTree)]

genDefsTests :: TestTree
genDefsTests =
  testGroup
    "GEN-9 argument inference"
    [ testCase "a use binds a parameter from its fields" $
        inferArgs defs pairDef [(Key "fst", dateNative), (Key "snd", dateNative)]
          @?= Right (Map.fromList [(TV "a", dateNative)])
    , testCase "a wire use binds the wire type, not the native argument it carries" $
        inferArgs defs outerDef [(Key "p", pairOf [dateNative] str), (Key "n", int64)]
          @?= Right (Map.fromList [(TV "a", str)])
    , testCase "a nested data type binds through its arms, not its arguments" $
        inferArgs
          defs
          (GenDef (TV "Holder") "struct" [(TV "a", KindType)] [(Key "b", boxOf [pa] pa)])
          [(Key "b", boxOf [dateNative] str)]
          @?= Right (Map.fromList [(TV "a", str)])
    , testCase "a parameter bound two ways is refused" $
        assertLeft (inferArgs defs pairDef [(Key "fst", int64), (Key "snd", str)])
    , testCase "a missing field is refused" $
        assertLeft (inferArgs defs pairDef [(Key "fst", int64)])
    , testCase "a use with its fields in another order is refused" $
        assertLeft (inferArgs defs pairDef [(Key "snd", int64), (Key "fst", int64)])
    , testCase "a parameter reached only through carried arguments is refused" $
        assertLeft
          ( inferArgs
              defs
              (GenDef (TV "Tagged") "struct" [(TV "a", KindType)] [(Key "t", NamF NamRecord (FV (TV "Tag") (CV "Tag<$1>")) [TyA pa] [])])
              [(Key "t", NamF NamRecord (FV (TV "Tag") (CV "Tag<$1>")) [TyA dateNative] [])]
          )
    , testCase "an unpacked use is spelled at the arguments its fields bind" $
        recordArgs defs pairDef [TyA dateNative] [(Key "fst", str), (Key "snd", str)] @?= Right [str]
    , testCase "a parameter no field binds keeps the carried argument" $
        recordArgs
          defs
          (GenDef (TV "Tag") "Tag<$1>" [(TV "a", KindType)] [(Key "n", int64)])
          [TyA dateNative]
          [(Key "n", int64)]
          @?= Right [dateNative]
    , testCase "a user-mapped unpacked use is rebuilt at its unpacked argument" $
        useArgs
          (Map.fromList [(TV "Pair", pairDef {gdForm = "PairT<$1>"})])
          (TV "Pair")
          "PairT<$1>"
          [TyA dateNative]
          [(Key "fst", str), (Key "snd", str)]
          @?= [TyA str]
    , testCase "one recursive record unrolled to two depths binds one parameter" $
        fmap Map.keys (inferArgs defs pairDef [(Key "fst", deepTree), (Key "snd", leafTree)]) @?= Right [TV "a"]
    , testCase "the canonical forms of two unrollings are equal" $
        canon defs deepTree @?= canon defs leafTree
    , testCase "references to a record at different arguments differ" $
        assertBool "distinct" (canon defs (RecF (FV (TV "Pair") (CV "struct")) [TyA int64]) /= canon defs (RecF (FV (TV "Pair") (CV "struct")) [TyA str]))
    , testCase "a recursive use whose field refers to the use agrees" $
        assertBool "agrees" (isNothing (checkUse defs treeDef [] [(Key "lab", pairOf [int64] int64), (Key "kids", listOf (RecF (FV (TV "Tree") (CV "struct")) []))]))
    , testCase "an untied reference of a record to itself is found" $
        untiedSelf (NamF NamRecord (FV (TV "Tree") (CV "struct")) [] [(Key "kids", listOf (var (TV "Tree") (CV "Tree")))]) @?= [(TV "Tree", True)]
    , testCase "a tied reference and a reference to another record are not untied" $
        untiedSelf (NamF NamRecord (FV (TV "Tree") (CV "struct")) [] [(Key "kids", listOf (RecF (FV (TV "Tree") (CV "struct")) [])), (Key "o", var (TV "Other") (CV "Other"))]) @?= []
    , testCase "Pair (Pair Int) keeps its inner Pair distinct" $
        assertBool "distinct" (canon defs (pairOf [pairOf [int64] int64] (pairOf [int64] int64)) /= canon defs (pairOf [int64] int64))
    , testCase "a use that agrees with its definition passes" $
        assertBool "agrees" (isNothing (checkUse defs outerDef [TyA str] [(Key "p", pairOf [dateNative] str), (Key "n", int64)]))
    , testCase "a use whose field differs from its definition is reported" $
        assertBool "differs" (isJust (checkUse defs outerDef [TyA str] [(Key "p", pairOf [str] str), (Key "n", str)]))
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
    assertLeft = either (const (return ())) (const (assertFailure "inferred"))

