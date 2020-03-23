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

tv :: Lang -> MT.Text -> TVar
tv MorlocLang t = TV Nothing t
tv lang t = TV (Just lang) t

arr :: Lang -> MT.Text -> [Type] -> Type
arr lang t ts = ArrT (tv lang t) ts

var :: Lang -> MT.Text -> Type
var lang t = VarT (tv lang t)

rec :: Lang -> MT.Text -> [(MT.Text, Type)] -> Type
rec lang name entries = NamT (tv lang name) entries



defaultList :: Maybe Lang -> Type -> DefaultType
defaultList Nothing t = DefaultType $ arr MorlocLang "List" [t]
defaultList (Just Python3Lang) t = DefaultType $ arr Python3Lang "list" [t]
defaultList (Just RLang) t@(VarT (TV (Just RLang) "numeric")) = DefaultType $ arr RLang "vector" [t]
defaultList (Just RLang) t@(VarT (TV (Just RLang) "character")) = DefaultType $ arr RLang "vector" [t]
defaultList (Just RLang) t@(VarT (TV (Just RLang) "logical")) = DefaultType $ arr RLang "vector" [t]
defaultList (Just RLang) t = DefaultType $ arr RLang "list" [t]
defaultList (Just CLang) t = DefaultType $ arr CLang "$1*" [t]
defaultList (Just CppLang) t = DefaultType $ arr CppLang "std::vector<$1>" [t]
defaultList (Just PerlLang) t = DefaultType $ arr CppLang "array" [t]

defaultTuple :: Maybe Lang -> [Type] -> DefaultType
defaultTuple Nothing ts = DefaultType $ arr MorlocLang (MT.pack $ "Tuple" ++ show (length ts)) ts

defaultTuple (Just Python3Lang) ts = DefaultType $ arr Python3Lang "tuple" ts
defaultTuple (Just RLang) ts = DefaultType $ arr RLang "list" ts
defaultTuple (Just CLang) ts = undefined -- TODO: structure
defaultTuple (Just CppLang) ts = DefaultType $ arr CppLang t ts where
  vars = ["$" <> MT.show' i | i <- [1 .. length ts]]
  t = "std::tuple<" <> MT.intercalate ", " vars <> ">"
defaultTuple (Just PerlLang) ts = DefaultType $ arr CppLang "array" ts

defaultRecord :: Maybe Lang -> [(MT.Text, Type)] -> DefaultType
defaultRecord Nothing entries = DefaultType $ rec MorlocLang "Record" entries
defaultRecord (Just Python3Lang) entries = DefaultType $ rec Python3Lang "dict" entries
defaultRecord (Just RLang) entries = DefaultType $ rec RLang "list" entries
defaultRecord (Just CLang) entries = undefined -- TODO: structure
defaultRecord (Just CppLang) entries = undefined -- TODO: structure
defaultRecord (Just PerlLang) entries = DefaultType $ rec PerlLang "hash" entries

defaultNull :: Maybe Lang -> DefaultType
defaultNull Nothing = DefaultType $ var MorlocLang "Unit"
defaultNull (Just Python3Lang) = DefaultType $ var Python3Lang "None"
defaultNull (Just RLang) = DefaultType $ var RLang "NULL"
defaultNull (Just CLang) = DefaultType $ var CLang "null"
defaultNull (Just CppLang) = DefaultType $ var CppLang "null"
defaultNull (Just PerlLang) = DefaultType $ var PerlLang "NULL"

defaultBool :: Maybe Lang -> DefaultType
defaultBool Nothing = DefaultType $ var MorlocLang "Bool"
defaultBool (Just Python3Lang) = DefaultType $ var Python3Lang "bool"
defaultBool (Just RLang) = DefaultType $ var RLang "logical" 
defaultBool (Just CLang) = DefaultType $ var CLang "bool"
defaultBool (Just CppLang) = DefaultType $ var CppLang "bool"
defaultBool (Just PerlLang) = DefaultType $ var PerlLang "bool"

defaultString :: Maybe Lang -> DefaultType
defaultString Nothing = DefaultType $ var MorlocLang "Str"
defaultString (Just Python3Lang) = DefaultType $ var Python3Lang "str"
defaultString (Just RLang) = DefaultType $ var RLang "character"
defaultString (Just CLang) = DefaultType $ var CLang "char*"
defaultString (Just CppLang) = DefaultType $ var CppLang "std::string"
defaultString (Just PerlLang) = DefaultType $ var PerlLang "str"

defaultNumber :: Maybe Lang -> DefaultType
defaultNumber Nothing = DefaultType $ var MorlocLang "Num"
defaultNumber (Just Python3Lang) = DefaultType $ var Python3Lang "float"
defaultNumber (Just RLang) = DefaultType $ var RLang "numeric"
defaultNumber (Just CLang) = DefaultType $ var CLang "double"
defaultNumber (Just CppLang) = DefaultType $ var CppLang "double"
defaultNumber (Just PerlLang) = DefaultType $ var PerlLang "double"
