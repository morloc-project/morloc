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

defaultList :: Lang -> Type -> Type
defaultList MorlocLang t = arr MorlocLang "List" [t]
defaultList Python3Lang t = arr Python3Lang "list" [t]
defaultList RLang t@(VarT (TV (Just RLang) "numeric")) = arr RLang "vector" [t]
defaultList RLang t@(VarT (TV (Just RLang) "character")) = arr RLang "vector" [t]
defaultList RLang t@(VarT (TV (Just RLang) "logical")) = arr RLang "vector" [t]
defaultList RLang t = arr RLang "list" [t]
defaultList CLang t = arr CLang "$1*" [t]
defaultList CppLang t = arr CppLang "std::vector<$1>" [t]
defaultList PerlLang t = arr CppLang "array" [t]

defaultTuple :: Lang -> [Type] -> Type
defaultTuple MorlocLang ts = arr MorlocLang "List" ts
defaultTuple Python3Lang ts = arr Python3Lang "tuple" ts
defaultTuple RLang ts = arr RLang "list" ts
defaultTuple CLang ts = undefined -- TODO: structure
defaultTuple CppLang ts = arr CppLang t ts where
  vars = ["$" <> MT.show' i | i <- [1 .. length ts]]
  t = "std::tuple<" <> MT.intercalate ", " vars <> ">"
defaultTuple PerlLang ts = arr CppLang "array" ts

defaultRecord :: Lang -> [(MT.Text, Type)] -> Type
defaultRecord MorlocLang entries =  rec MorlocLang "Record" entries
defaultRecord Python3Lang entries = rec Python3Lang "dict" entries
defaultRecord RLang entries = rec RLang "list" entries
defaultRecord CLang entries = undefined -- TODO: structure
defaultRecord CppLang entries = undefined -- TODO: structure
defaultRecord PerlLang entries = rec PerlLang "hash" entries

defaultNull :: Lang -> Type
defaultNull MorlocLang = var MorlocLang "Null"
defaultNull Python3Lang = var Python3Lang "None"
defaultNull RLang = var RLang "NULL"
defaultNull CLang = var CLang "null"
defaultNull CppLang = var CppLang "null"
defaultNull PerlLang = var PerlLang "NULL"

defaultBool :: Lang -> Type
defaultBool MorlocLang = var MorlocLang "Bool"
defaultBool Python3Lang = var Python3Lang "bool"
defaultBool RLang = var RLang "logical" 
defaultBool CLang = var CLang "bool"
defaultBool CppLang = var CppLang "bool"
defaultBool PerlLang = var PerlLang "bool"

defaultString :: Lang -> Type
defaultString MorlocLang = var MorlocLang "Str"
defaultString Python3Lang = var Python3Lang "str"
defaultString RLang = var RLang "character"
defaultString CLang = var CLang "char*"
defaultString CppLang = var CppLang "std::string"
defaultString PerlLang = var PerlLang "str"

defaultNumber :: Lang -> Type
defaultNumber MorlocLang = var MorlocLang "Num"
defaultNumber Python3Lang = var Python3Lang "float"
defaultNumber RLang = var RLang "numeric"
defaultNumber CLang = var CLang "double"
defaultNumber CppLang = var CppLang "double"
defaultNumber PerlLang = var PerlLang "double"
