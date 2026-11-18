{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Translator.PyClasses
Description : The Python modules of the generated record classes (GEN-6, GEN-11)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A Python record of a class kind (@dataclass@, @pydantic@) is an instance of
a class the compiler writes, one Python module per declaring morloc module,
below @pools/types/py@. A class's qualified name is its record's concrete
name and its schema hint. This module renders those files, the statements a
pool runs to import and register the classes, and refuses names Python
cannot spell (GEN-5).
-}
module Morloc.CodeGenerator.Grammars.Translator.PyClasses
  ( PyClasses (..)
  , DataShape (..)
  , pythonClasses
  , pyCtorName
  , pyCtorRef
  ) where

import Data.Char (isAlphaNum, isAscii)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Morloc.CodeGenerator.Grammars.Common (collectRecordsWhere, isIdentifier, poolNativeTypes)
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Zip (indexedFrom)
import qualified Morloc.LangRegistry as LR

data PyClasses = PyClasses
  { pcFiles :: [([Text], Text)]
  -- ^ Each file's path below @pools/types/py@, and its text.
  , pcLoad :: [Text]
  -- ^ Statements, run before any sourced file is imported, that import the
  -- class modules and register each class with the runtime.
  }

-- | A `data` type's constructors (GEN-12): those of an enumeration, or each
-- constructor with its number of fields.
data DataShape = EnumShape [Text] | VariantShape [(Text, Int)]

-- | A constructor's attribute name on its type's class: its own name, with
-- a trailing underscore when it is a Python keyword (`None_`), as PEP 8
-- has a name that would clash with one.
pyCtorName :: Set.Set Text -> Text -> Text
pyCtorName keywords n = if Set.member n keywords then n <> "_" else n

-- | A constructor of a class, read by attribute where its name is plain
-- ASCII, and by its exact string otherwise: Python normalizes a non-ASCII
-- identifier in source (NFKC), so two names it normalizes alike would meet.
pyCtorRef :: Set.Set Text -> Text -> Text -> Text
pyCtorRef keywords ty n
  | T.all (\c -> isAscii c && (isAlphaNum c || c == '_')) a = ty <> "." <> a
  | otherwise = "getattr(" <> ty <> ", \"" <> a <> "\")"
  where
    a = pyCtorName keywords n

-- | The class modules the pool's records and the program's `data` types
-- need, or the reasons they cannot be written, given the record classes,
-- those whose record has type parameters, the `data` types by qualified
-- name, and the language's keywords.
pythonClasses :: Map.Map Text LR.ClassKind -> Set.Set Text -> Map.Map Text DataShape -> Set.Set Text -> [SerialManifold] -> Either [Text] PyClasses
pythonClasses classes parameterized datas0 keywords es
  | Map.null used && Map.null datas = Right (PyClasses [] [])
  | not (null problems) = Left problems
  | otherwise = Right (PyClasses files load)
  where
    occurrences = concatMap (collectRecordsWhere (`Map.member` classes)) es
    used =
      Map.fromListWith (<>)
        [ (c, [rs])
        | (FV _ (CV c), _, rs) <- occurrences
        ]
    -- Every `data` type of the program has its class, since a sourced file
    -- may import it; one the pool's code never holds and whose name Python
    -- cannot spell is left out rather than refused.
    usedData = Set.fromList [c | t <- concatMap poolNativeTypes es, c <- dataNames t]
    dataNames t = case t of
      VariantF (FV _ (CV c)) _ _ -> c : concatMap dataNames (childrenOf t)
      EnumF (FV _ (CV c)) _ _ -> [c]
      _ -> concatMap dataNames (childrenOf t)
    spellable c = all (\p -> isIdentifier p && not (Set.member p keywords)) (T.splitOn "." c)
    datas = Map.filterWithKey (\c _ -> Set.member c usedData || spellable c) datas0
    kindOf c = Map.findWithDefault LR.DataclassKind c classes
    firstFields c = concat (take 1 (Map.findWithDefault [] c used))
    moduleOf c = T.intercalate "." (init (T.splitOn "." c))
    leafOf c = last (T.splitOn "." c)
    byModule = Map.fromListWith (<>) [(moduleOf c, [c]) | c <- Map.keys used <> Map.keys datas]

    -- A record with type parameters annotates every field as any value: a
    -- field's type at one use is not its declared type, and annotating it
    -- by its uses could make two class modules import each other.
    fieldsOf c = case Map.findWithDefault [] c used of
      [] -> []
      (rs : _) ->
        [ (k, if Set.member c parameterized then "typing.Any" else annotation (moduleOf c) t)
        | (k, t) <- rs
        ]

    -- Another module's class is named through an alias of its module: a
    -- dotted path to a child module is not bound on its parent while the
    -- parent is being imported, and pydantic evaluates the annotation then.
    aliasOf m = "_mlc_m" <> T.pack (show (Map.findIndex m byModule))
    annotation here t = case t of
      NamF NamRecord (FV _ (CV c)) _ _
        | Map.member c classes -> if moduleOf c == here then leafOf c else aliasOf (moduleOf c) <> "." <> leafOf c
        | c == "dict" -> "dict"
      VarF (FV _ (CV c)) | Set.member c builtins -> c
      AppF (VarF (FV _ (CV c))) args
        | c `elem` ["list", "set", "frozenset"], [a] <- tyArgs args -> c <> "[" <> annotation here a <> "]"
        | c == "dict", [a, b] <- tyArgs args -> "dict[" <> annotation here a <> ", " <> annotation here b <> "]"
        | c == "tuple", as@(_ : _) <- tyArgs args -> "tuple[" <> T.intercalate ", " (map (annotation here) as) <> "]"
      OptionalF x -> "typing.Optional[" <> annotation here x <> "]"
      _ -> "typing.Any"
    builtins = Set.fromList ["int", "float", "str", "bool", "bytes", "bytearray"]

    referenced here t = case t of
      NamF _ (FV _ (CV c)) _ _ | Map.member c classes -> [moduleOf c | moduleOf c /= here]
      AppF h args -> referenced here h <> concatMap (referenced here) (tyArgs args)
      OptionalF x -> referenced here x
      _ -> []

    pydanticReserved =
      Set.fromList
        [ "dict", "json", "copy", "schema", "schema_json", "construct", "validate", "fields"
        , "parse_obj", "parse_raw", "parse_file", "from_orm", "update_forward_refs"
        , "typing", "pydantic", "dataclasses", "mlc_types"
        ]

    problems =
      [ "the generated Python class " <> c <> " cannot be written: `" <> w <> "` " <> why
      | c <- Map.keys used <> Map.keys datas
      , (w, why) <-
          [ (p, "is not a Python identifier or is a keyword, and names a package of it")
          | p <- T.splitOn "." c, not (isIdentifier p) || Set.member p keywords
          ]
            <> [ (k, "is a Python keyword or a name Python mangles in a class, and names one of its fields")
               | (Key k, _) <- firstFields c
               , Set.member k keywords || not (isIdentifier k) || "__" `T.isPrefixOf` k
               ]
            <> [ (k, "is a name a pydantic model reserves, and names one of its fields")
               | kindOf c == LR.PydanticKind
               , (Key k, _) <- firstFields c
               , "_" `T.isPrefixOf` k || "model_" `T.isPrefixOf` k || Set.member k pydanticReserved
               ]
      ]
        <> [ "the generated Python class " <> c <> " is both a record's and a `data` type's"
           | c <- Map.keys (Map.intersection used datas)
           ]
    ctorsOf c = case Map.lookup c datas of
      Just (EnumShape ns) -> ns
      Just (VariantShape as) -> map fst as
      Nothing -> []

    files = packageInits <> map moduleFile (Map.toList byModule)
    -- Every package directory above a class module needs its own
    -- __init__.py; a module's own file is its directory's __init__.py.
    packageInits =
      [ (prefix <> ["__init__.py"], "")
      | prefix <- Set.toList (Set.fromList [take n parts | m <- Map.keys byModule, let parts = T.splitOn "." m, n <- [1 .. length parts - 1]])
      , T.intercalate "." prefix `Map.notMember` byModule
      ]

    moduleFile (m, cs) =
      let cs' = sort cs
          imports = Set.toList (Set.fromList (concat [referenced m t | c <- cs', (_, t) <- firstFields c]))
          records = filter (`Map.member` used) cs'
          shapes = [(c, d) | c <- cs', Just d <- [Map.lookup c datas]]
          anyPydantic = any ((== LR.PydanticKind) . kindOf) records
          anyVariant = not (null [() | (_, VariantShape _) <- shapes])
          anyEnum = not (null [() | (_, EnumShape _) <- shapes])
          anyDataclass = any ((== LR.DataclassKind) . kindOf) records || anyVariant
          anyTyping = or [T.isInfixOf "typing." a | c <- records, (_, a) <- fieldsOf c] || anyVariant
          body =
            [ "# Generated by morloc; do not edit."
            , "from __future__ import annotations"
            ]
              <> ["import dataclasses" | anyDataclass]
              <> ["import enum" | anyEnum]
              <> ["import typing" | anyTyping]
              <> ["import pydantic" | anyPydantic]
              <> ["import " <> i <> " as " <> aliasOf i | i <- imports]
              <> concatMap dataDef shapes
              <> concatMap classDef records
              <> concat [["", leafOf c <> ".model_rebuild()"] | c <- records, kindOf c == LR.PydanticKind]
       in (T.splitOn "." m <> ["__init__.py"], T.unlines body)

    classDef c =
      let fields = ["    " <> unKey k <> ": " <> a | (k, a) <- fieldsOf c]
          getter =
            [ ""
            , "    def __getitem__(self, key):"
            , "        return getattr(self, key)"
            ]
       in case kindOf c of
            LR.DataclassKind ->
              ["", "", "@dataclasses.dataclass(frozen=True, slots=True)", "class " <> leafOf c <> ":"]
                <> fields
                <> getter
            LR.PydanticKind ->
              [ ""
              , ""
              , "class " <> leafOf c <> "(pydantic.BaseModel):"
              , "    model_config = pydantic.ConfigDict(frozen=True, arbitrary_types_allowed=True)"
              ]
                <> fields
                <> getter

    -- An enumeration is an `enum.Enum` whose member values are the tags; any
    -- other `data` type is a base class whose constructors are its
    -- attributes: a frozen slotted dataclass per constructor with fields,
    -- and one instance of a fieldless one per constructor without. Each
    -- constructor's class carries its tag.
    -- Constructors are set by their exact names, never written as
    -- identifiers (see 'pyCtorRef').
    dataDef (c, EnumShape ns) =
      let d = leafOf c
       in [ ""
          , ""
          , d <> " = enum.Enum(" <> quoted d <> ", ["
              <> T.intercalate ", " ["(" <> quoted (pyCtorName keywords n) <> ", " <> T.pack (show i) <> ")" | (i, n) <- indexedFrom 0 ns]
              <> "], module=__name__, qualname=" <> quoted d <> ")"
          ]
    dataDef (c, VariantShape as) =
      let d = leafOf c
       in ["", "", "class " <> d <> ":", "    __slots__ = ()"]
            <> concat
              [ let tmp = "_mlc_arm" <> T.pack (show i)
                    a = pyCtorName keywords n
                 in [ ""
                    , ""
                    , "@dataclasses.dataclass(frozen=True, slots=True)"
                    , "class " <> tmp <> "(" <> d <> "):"
                    , "    _mlc_tag: typing.ClassVar[int] = " <> T.pack (show i)
                    ]
                      <> ["    f" <> T.pack (show j) <> ": typing.Any" | j <- [0 .. k - 1]]
                      <> [ ""
                         , tmp <> ".__name__ = " <> quoted a
                         , tmp <> ".__qualname__ = " <> quoted (d <> "." <> a)
                         , "setattr(" <> d <> ", " <> quoted a <> ", " <> tmp <> (if k == 0 then "()" else "") <> ")"
                         , "del " <> tmp
                         ]
              | (i, (n, k)) <- indexedFrom 0 as
              ]
    quoted x = "\"" <> x <> "\""

    load =
      [ "sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), \"..\", \"types\", \"py\"))"
      ]
        <> ["import " <> m | m <- Map.keys byModule]
        <> [ "morloc.register_class(\"" <> c <> "\", " <> c <> ", " <> (if kindOf c == LR.PydanticKind then "1" else "0") <> ")"
           | c <- Map.keys used
           ]
        -- Each constructor in tag order: an enumeration's members, or a
        -- class (fields) or an instance (none) per constructor.
        <> [ "morloc.register_data(\"" <> c <> "\", " <> (case d of EnumShape _ -> "0"; VariantShape _ -> "1")
               <> ", (" <> T.concat [pyCtorRef keywords c n <> ", " | n <- ctorsOf c] <> "))"
           | (c, d) <- Map.toList datas
           ]
