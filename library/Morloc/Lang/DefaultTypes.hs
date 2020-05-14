{-|
Module      : Morloc.Lang.DefaultTypes
Description : Define default types for each language
Copyright   : (c) Zebulun Arendsee, 2020
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

module Morloc.Lang.DefaultTypes
  ( defaultBool
  , defaultList
  , defaultNull
  , defaultNumber
  , defaultRecord
  , defaultString
  , defaultTuple
  ) where

import Morloc.Namespace
import qualified Morloc.Data.Text as MT

defaultList :: Maybe Lang -> Type -> [DefaultType]
defaultList lang@Nothing t = [DefaultType $ ArrT (TV lang "List") [t]]
defaultList lang@(Just Python3Lang) t = [DefaultType $ ArrT (TV lang "list") [t]]
defaultList lang@(Just RLang) t = [DefaultType $ ArrT (TV lang "list") [t]]
defaultList lang@(Just CLang) t = [DefaultType $ ArrT (TV lang "$1*") [t]]
defaultList lang@(Just CppLang) t = [DefaultType $ ArrT (TV lang "std::vector<$1>") [t]]
defaultList lang@(Just PerlLang) t = [DefaultType $ ArrT (TV lang "array") [t]]

defaultTuple :: Maybe Lang -> [Type] -> [DefaultType]
defaultTuple lang@Nothing ts = [DefaultType $ ArrT (TV lang (MT.pack $ "Tuple" ++ show (length ts))) ts]
defaultTuple lang@(Just Python3Lang) ts = [DefaultType $ ArrT (TV lang "tuple") ts]
defaultTuple lang@(Just RLang) ts = [DefaultType $ ArrT (TV lang "tuple") ts]
defaultTuple lang@(Just CLang) ts = []
defaultTuple lang@(Just CppLang) ts = [DefaultType $ ArrT (TV lang t) ts] where
  vars = ["$" <> MT.show' i | i <- [1 .. length ts]]
  t = "std::tuple<" <> MT.intercalate "," vars <> ">"
defaultTuple lang@(Just PerlLang) ts = [DefaultType $ ArrT (TV lang "array") ts]

defaultRecord :: Maybe Lang -> [(MT.Text, Type)] -> [DefaultType]
defaultRecord lang@Nothing entries = [DefaultType $ NamT (TV lang "Record") entries]
defaultRecord lang@(Just Python3Lang) entries = [DefaultType $ NamT (TV lang "record") entries]
defaultRecord lang@(Just RLang) entries = [DefaultType $ NamT (TV lang "record") entries]
defaultRecord lang@(Just CLang) entries = []
defaultRecord lang@(Just CppLang) entries = []
defaultRecord lang@(Just PerlLang) entries = [DefaultType $ NamT (TV lang "hash") entries]

defaultNull :: Maybe Lang -> [DefaultType]
defaultNull lang@Nothing = [DefaultType $ VarT (TV lang "Unit")]
defaultNull lang@(Just Python3Lang) = [DefaultType $ VarT (TV lang "None")]
defaultNull lang@(Just RLang) = [DefaultType $ VarT (TV lang "NULL")]
defaultNull lang@(Just CLang) = [DefaultType $ VarT (TV lang "null")]
defaultNull lang@(Just CppLang) = [DefaultType $ VarT (TV lang "null")]
defaultNull lang@(Just PerlLang) = [DefaultType $ VarT (TV lang "NULL")]

defaultBool :: Maybe Lang -> [DefaultType]
defaultBool lang@Nothing = [DefaultType $ VarT (TV lang "Bool")]
defaultBool lang@(Just Python3Lang) = [DefaultType $ VarT (TV lang "bool")]
defaultBool lang@(Just RLang) = [DefaultType $ VarT (TV lang "logical" )]
defaultBool lang@(Just CLang) = [DefaultType $ VarT (TV lang "bool")]
defaultBool lang@(Just CppLang) = [DefaultType $ VarT (TV lang "bool")]
defaultBool lang@(Just PerlLang) = [DefaultType $ VarT (TV lang "bool")]

defaultString :: Maybe Lang -> [DefaultType]
defaultString lang@Nothing = [DefaultType $ VarT (TV lang "Str")]
defaultString lang@(Just Python3Lang) = [DefaultType $ VarT (TV lang "str")]
defaultString lang@(Just RLang) = [DefaultType $ VarT (TV lang "character")]
defaultString lang@(Just CLang) = [DefaultType $ VarT (TV lang "char*")]
defaultString lang@(Just CppLang) = [DefaultType $ VarT (TV lang "std::string")]
defaultString lang@(Just PerlLang) = [DefaultType $ VarT (TV lang "str")]

defaultNumber :: Maybe Lang -> [DefaultType]
defaultNumber lang@Nothing = [DefaultType $ VarT (TV lang "Num")]
defaultNumber lang@(Just Python3Lang) = [DefaultType $ VarT (TV lang "float")]
defaultNumber lang@(Just RLang) = [DefaultType $ VarT (TV lang "numeric")]
defaultNumber lang@(Just CLang) = [DefaultType $ VarT (TV lang "double")]
defaultNumber lang@(Just CppLang) = [DefaultType $ VarT (TV lang "double")]
defaultNumber lang@(Just PerlLang) = [DefaultType $ VarT (TV lang "double")]
