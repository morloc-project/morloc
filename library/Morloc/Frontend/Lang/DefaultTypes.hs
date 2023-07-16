{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Frontend.Lang.DefaultTypes
Description : Define default types for each language
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

Concrete types are usually inferred from concrete function types. But in some
cases, it is necessary to guess the type. For example, in the statement @f x =
[x]@, a concrete list container is needed. When @f@ is called from another
function, the concrete list type can be inferred. But if @f@ is directly
exported, the concrete list type is unknown. Providing a concrete signature for
@f@, however, limits the use of @f@ in other functions. Thus a default type is
needed.
-}

module Morloc.Frontend.Lang.DefaultTypes
  (
  -- * retrieve default types by language
    defaultBool
  , defaultList
  , defaultNull
  , defaultReal
  , defaultInt
  , defaultRecord
  , defaultString
  , defaultTuple
  -- * very partial convenience functions
  , defaultGeneralType
  -- * dictionaries of base collection names
  , listG
  , listC
  , listGC
  , tupleG
  , tupleC
  , tupleGC
  , recordG
  , recordC
  , recordGC
  -- * functions for concrete synthesis
  , generalDefaultToConcrete
  ) where

import Morloc.Frontend.Namespace
import qualified Morloc.Data.Text as MT

generalDefaultToConcrete :: MT.Text -> Int -> Lang -> MT.Text
generalDefaultToConcrete = undefined
-- generalDefaultToConcrete v n lang
--     | v == listG = listC lang
--     | v == tupleG = tupleG lang
--     | _ = []

listG :: MT.Text
listG = "List"

listC :: Lang -> [MT.Text]
listC Python3Lang = ["list"]
listC RLang = ["list"]
listC CLang = ["$1*"]
listC CppLang = ["std::vector<$1>"]
listC RustLang = ["Vec<$1>"]
listC PerlLang = ["array"]

listGC :: Maybe Lang -> [MT.Text]
listGC Nothing = [listG]
listGC (Just lang) = listC lang

defaultList :: Maybe Lang -> TypeU -> [TypeU]
defaultList Nothing t = [AppU (VarU (TV Nothing listG)) [t]]
defaultList lang@(Just l) t = [AppU (VarU (TV lang v)) [t] | v <- listC l] 



tupleG :: Int -> MT.Text
tupleG i = MT.pack $ "Tuple" ++ show i

tupleC :: Int -> Lang -> [MT.Text]
tupleC _ Python3Lang = ["tuple"]
tupleC _ RLang = ["tuple"]
tupleC _ CLang = []
tupleC i CppLang =
  let vars = ["$" <> MT.show' i' | i' <- [1..i]]
  in ["std::tuple<" <> MT.intercalate "," vars <> ">"]
tupleC i RustLang =
  let vars = ["$" <> MT.show' i' | i' <- [1..i]]
  in ["(" <> MT.intercalate "," vars <> ")"]
tupleC _ PerlLang = ["array"]

tupleGC :: Maybe Lang -> Int -> [MT.Text]
tupleGC Nothing i = [tupleG i]
tupleGC (Just lang) i = tupleC i lang

defaultTuple :: Maybe Lang -> [TypeU] -> [TypeU]
defaultTuple Nothing ts = [AppU (VarU (TV Nothing (tupleG (length ts)))) ts]
defaultTuple lang@(Just l) ts = [AppU (VarU (TV lang v)) ts | v <- tupleC (length ts) l]



recordG :: MT.Text
recordG = "Record"

recordC :: Lang -> [MT.Text]
recordC Python3Lang = ["dict"]
recordC RLang = ["list"]
recordC CLang = []
recordC CppLang = ["struct"]
recordC RustLang = ["struct"]
recordC PerlLang = ["hash"]

recordGC :: Maybe Lang -> [MT.Text]
recordGC Nothing = [recordG]
recordGC (Just lang) = recordC lang

defaultRecord :: Maybe Lang -> [(MT.Text, TypeU)] -> [TypeU]
defaultRecord Nothing entries = [NamU NamRecord (TV Nothing recordG) [] entries]
defaultRecord lang@(Just l) entries = [NamU NamRecord (TV lang v) [] entries | v <- recordC l]

-- this is an internal convenience wrapper, it is partial, so don't misuse it
defaultGeneralType :: SExpr g f c -> TypeU
defaultGeneralType UniS = head $ defaultNull Nothing
defaultGeneralType (LogS _) = head $ defaultBool Nothing
defaultGeneralType (StrS _) = head $ defaultString Nothing
defaultGeneralType (RealS _) = head $ defaultReal Nothing
defaultGeneralType (IntS _) = head $ defaultInt Nothing
defaultGeneralType _ = error "Fill this out if you feel like it, not my problem"

-- | This is the value returned by a functions that doesn't return, for example,
-- an print statement. It needs to be defined even for languages that don't
-- have an explicit NULL (such as Rust).
defaultNull :: Maybe Lang -> [TypeU]
defaultNull lang@Nothing = [VarU (TV lang "Unit")]
defaultNull lang@(Just Python3Lang) = [VarU (TV lang "None")]
defaultNull lang@(Just RLang) = [VarU (TV lang "NULL")]
defaultNull lang@(Just CLang) = [VarU (TV lang "void")] -- need to add Unit
defaultNull lang@(Just CppLang) = [VarU (TV lang "Unit")] -- the Unit enum is from mlccpptypes/prelude.hpp
defaultNull lang@(Just RustLang) = [VarU (TV lang "<VOID>")] -- Rust doesn't have an explicit NULL
defaultNull lang@(Just PerlLang) = [VarU (TV lang "NULL")]

defaultBool :: Maybe Lang -> [TypeU]
defaultBool lang@Nothing = [VarU (TV lang "Bool")]
defaultBool lang@(Just Python3Lang) = [VarU (TV lang "bool")]
defaultBool lang@(Just RLang) = [VarU (TV lang "logical" )]
defaultBool lang@(Just CLang) = [VarU (TV lang "bool")]
defaultBool lang@(Just CppLang) = [VarU (TV lang "bool")]
defaultBool lang@(Just RustLang) = [VarU (TV lang "bool")]
defaultBool lang@(Just PerlLang) = [VarU (TV lang "bool")]

defaultString :: Maybe Lang -> [TypeU]
defaultString lang@Nothing = [VarU (TV lang "Str")]
defaultString lang@(Just Python3Lang) = [VarU (TV lang "str")]
defaultString lang@(Just RLang) = [VarU (TV lang "character")]
defaultString lang@(Just CLang) = [VarU (TV lang "char*")]
defaultString lang@(Just CppLang) = [VarU (TV lang "std::string")]
defaultString lang@(Just RustLang) = [VarU (TV lang "String"), VarU (TV lang "str")]
defaultString lang@(Just PerlLang) = [VarU (TV lang "str")]

-- a primitive number can automatically be promoted into any of the default numbers listed below
defaultReal :: Maybe Lang -> [TypeU]
defaultReal lang@Nothing = [VarU (TV lang "Real")]
defaultReal lang@(Just Python3Lang) = [VarU (TV lang "float")]
defaultReal lang@(Just RLang) = [VarU (TV lang "numeric")]
defaultReal lang@(Just CLang) = [VarU (TV lang "double")]
defaultReal lang@(Just CppLang) =
  [ VarU (TV lang "double")
  , VarU (TV lang "float")
  ]
defaultReal lang@(Just RustLang) =
  [ VarU (TV lang "f64")
  , VarU (TV lang "f32")
  ]
defaultReal lang@(Just PerlLang) = [VarU (TV lang "double")]

defaultInt :: Maybe Lang -> [TypeU]
defaultInt lang@Nothing = [VarU (TV lang "Int")]
defaultInt lang@(Just Python3Lang) = [VarU (TV lang "int")]
defaultInt lang@(Just RLang) = [VarU (TV lang "integer")]
defaultInt lang@(Just CLang) = [VarU (TV lang "int")]
defaultInt lang@(Just CppLang) =
  [ VarU (TV lang "int")
  , VarU (TV lang "long")
  , VarU (TV lang "size_t")
  ]
defaultInt lang@(Just RustLang) =
  [ VarU (TV lang "i64")
  , VarU (TV lang "i128")
  , VarU (TV lang "i32")
  , VarU (TV lang "i16")
  , VarU (TV lang "i8")
  , VarU (TV lang "isize")
  , VarU (TV lang "u8")
  , VarU (TV lang "u16")
  , VarU (TV lang "u32")
  , VarU (TV lang "u64")
  , VarU (TV lang "u128")
  , VarU (TV lang "usize")
  ]
defaultInt lang@(Just PerlLang) = [VarU (TV lang "integer")]
