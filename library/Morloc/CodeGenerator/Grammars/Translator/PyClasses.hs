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
  , pythonClasses
  ) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Morloc.CodeGenerator.Grammars.Common (collectRecordsWhere, isIdentifier)
import Morloc.CodeGenerator.Namespace
import qualified Morloc.LangRegistry as LR

data PyClasses = PyClasses
  { pcFiles :: [([Text], Text)]
  -- ^ Each file's path below @pools/types/py@, and its text.
  , pcLoad :: [Text]
  -- ^ Statements, run before any sourced file is imported, that import the
  -- class modules and register each class with the runtime.
  }

-- | The class modules the pool's records need, or the reasons they cannot
-- be written, given the classes, those whose record has type parameters,
-- and the language's keywords.
pythonClasses :: Map.Map Text LR.ClassKind -> Set.Set Text -> Set.Set Text -> [SerialManifold] -> Either [Text] PyClasses
pythonClasses classes parameterized keywords es
  | Map.null used = Right (PyClasses [] [])
  | not (null problems) = Left problems
  | otherwise = Right (PyClasses files load)
  where
    occurrences = concatMap (collectRecordsWhere (`Map.member` classes)) es
    used =
      Map.fromListWith (<>)
        [ (c, [rs])
        | (FV _ (CV c), _, rs) <- occurrences
        ]
    kindOf c = Map.findWithDefault LR.DataclassKind c classes
    firstFields c = concat (take 1 (Map.findWithDefault [] c used))
    moduleOf c = T.intercalate "." (init (T.splitOn "." c))
    leafOf c = last (T.splitOn "." c)
    byModule = Map.fromListWith (<>) [(moduleOf c, [c]) | c <- Map.keys used]

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
      | c <- Map.keys used
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
          anyPydantic = any ((== LR.PydanticKind) . kindOf) cs'
          anyDataclass = any ((== LR.DataclassKind) . kindOf) cs'
          anyTyping = or [T.isInfixOf "typing." a | c <- cs', (_, a) <- fieldsOf c]
          body =
            [ "# Generated by morloc; do not edit."
            , "from __future__ import annotations"
            ]
              <> ["import dataclasses" | anyDataclass]
              <> ["import typing" | anyTyping]
              <> ["import pydantic" | anyPydantic]
              <> ["import " <> i <> " as " <> aliasOf i | i <- imports]
              <> concatMap classDef cs'
              <> concat [["", leafOf c <> ".model_rebuild()"] | c <- cs', kindOf c == LR.PydanticKind]
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

    load =
      [ "sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), \"..\", \"types\", \"py\"))"
      ]
        <> ["import " <> m | m <- Map.keys byModule]
        <> [ "morloc.register_class(\"" <> c <> "\", " <> c <> ", " <> (if kindOf c == LR.PydanticKind then "1" else "0") <> ")"
           | c <- Map.keys used
           ]
