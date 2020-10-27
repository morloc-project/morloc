{-|
Module      : Morloc.Frontend.Lang.DefaultTypes
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

module Morloc.Frontend.Lang.DefaultTypes
  ( defaultBool
  , defaultList
  , defaultNull
  , defaultNumber
  , defaultRecord
  , defaultString
  , defaultTuple
  ) where

import Morloc.Frontend.Namespace
import qualified Morloc.Data.Text as MT

defaultList :: Maybe Lang -> UnresolvedType -> [UnresolvedType]
defaultList lang@Nothing t = [ArrU (TV lang "List") [t]]
defaultList lang@(Just Python3Lang) t = [ArrU (TV lang "list") [t]]
defaultList lang@(Just RLang) t = [ArrU (TV lang "list") [t]]
defaultList lang@(Just CLang) t = [ArrU (TV lang "$1*") [t]]
defaultList lang@(Just CppLang) t = [ArrU (TV lang "std::vector<$1>") [t]]
defaultList lang@(Just PerlLang) t = [ArrU (TV lang "array") [t]]

defaultTuple :: Maybe Lang -> [UnresolvedType] -> [UnresolvedType]
defaultTuple lang@Nothing ts = [ArrU (TV lang (MT.pack $ "Tuple" ++ show (length ts))) ts]
defaultTuple lang@(Just Python3Lang) ts = [ArrU (TV lang "tuple") ts]
defaultTuple lang@(Just RLang) ts = [ArrU (TV lang "tuple") ts]
defaultTuple lang@(Just CLang) ts = []
defaultTuple lang@(Just CppLang) ts = [ArrU (TV lang t) ts] where
  vars = ["$" <> MT.show' i | i <- [1 .. length ts]]
  t = "std::tuple<" <> MT.intercalate "," vars <> ">"
defaultTuple lang@(Just PerlLang) ts = [ArrU (TV lang "array") ts]

defaultRecord :: Maybe Lang -> [(MT.Text, UnresolvedType)] -> [UnresolvedType]
defaultRecord lang@Nothing entries = [NamU NamRecord (TV lang "Record") entries]
defaultRecord lang@(Just Python3Lang) entries = [NamU NamRecord (TV lang "dict") entries]
defaultRecord lang@(Just RLang) entries = [NamU NamRecord (TV lang "list") entries]
defaultRecord lang@(Just CLang) entries = []
defaultRecord lang@(Just CppLang) entries = [NamU NamRecord (TV lang "struct") entries]
defaultRecord lang@(Just PerlLang) entries = [NamU NamRecord (TV lang "hash") entries]

defaultNull :: Maybe Lang -> [UnresolvedType]
defaultNull lang@Nothing = [VarU (TV lang "Unit")]
defaultNull lang@(Just Python3Lang) = [VarU (TV lang "None")]
defaultNull lang@(Just RLang) = [VarU (TV lang "NULL")]
defaultNull lang@(Just CLang) = [VarU (TV lang "null")]
defaultNull lang@(Just CppLang) = [VarU (TV lang "null")]
defaultNull lang@(Just PerlLang) = [VarU (TV lang "NULL")]

defaultBool :: Maybe Lang -> [UnresolvedType]
defaultBool lang@Nothing = [VarU (TV lang "Bool")]
defaultBool lang@(Just Python3Lang) = [VarU (TV lang "bool")]
defaultBool lang@(Just RLang) = [VarU (TV lang "logical" )]
defaultBool lang@(Just CLang) = [VarU (TV lang "bool")]
defaultBool lang@(Just CppLang) = [VarU (TV lang "bool")]
defaultBool lang@(Just PerlLang) = [VarU (TV lang "bool")]

defaultString :: Maybe Lang -> [UnresolvedType]
defaultString lang@Nothing = [VarU (TV lang "Str")]
defaultString lang@(Just Python3Lang) = [VarU (TV lang "str")]
defaultString lang@(Just RLang) = [VarU (TV lang "character")]
defaultString lang@(Just CLang) = [VarU (TV lang "char*")]
defaultString lang@(Just CppLang) = [VarU (TV lang "std::string")]
defaultString lang@(Just PerlLang) = [VarU (TV lang "str")]

defaultNumber :: Maybe Lang -> [UnresolvedType]
defaultNumber lang@Nothing = [VarU (TV lang "Num"), VarU (TV lang "Int")]
defaultNumber lang@(Just Python3Lang) = [VarU (TV lang "float"), VarU (TV lang "int")]
defaultNumber lang@(Just RLang) = [VarU (TV lang "numeric"), VarU (TV lang "integer")]
defaultNumber lang@(Just CLang) = [VarU (TV lang "double"), VarU (TV lang "int")]
defaultNumber lang@(Just CppLang) = [VarU (TV lang "double"), VarU (TV lang "int")]
defaultNumber lang@(Just PerlLang) = [VarU (TV lang "double")]
