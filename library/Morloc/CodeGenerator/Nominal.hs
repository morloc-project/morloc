{- |
Module      : Morloc.CodeGenerator.Nominal
Description : Keys identifying declared types in code generation (NOM-1)

The key of a declared record, object, table or `data` type: its name, its
arguments and its level (NOM-1). A key's arguments are types in which every
declared type is itself a key, so a key is finite and compared by value.
-}
module Morloc.CodeGenerator.Nominal
  ( Level (..)
  , NomKey (..)
  , KeyType (..)
  , KeyArg (..)
  , KeyKind (..)
  ) where

import qualified Data.Text as T
import Morloc.Namespace.Prim (Key, Kind, TVar)
import Morloc.Namespace.Type (NamType)

-- | Whether a key names the type a pool holds (native) or the type it is
-- written as on the wire side of a packed argument (unpacked, NOM-4).
data Level = NativeLevel | UnpackedLevel
  deriving (Show, Eq, Ord)

-- | A declared type at arguments, at a level.
data NomKey = NomKey
  { nkCon :: TVar
  , nkArgs :: [KeyArg]
  , nkLevel :: Level
  }
  deriving (Show, Eq, Ord)

-- | A key argument's type: morloc names, with every declared type a key.
data KeyType
  = KVar TVar
  -- ^ a type by its morloc name: a primitive or an opaque type
  | KUnk TVar
  -- ^ a type nothing determined
  | KFun [KeyType] KeyType
  | KApp KeyType [KeyArg]
  | KOpt KeyType
  | KNom NomKey
  | KAnon NamType [(Key, KeyType)]
  -- ^ an anonymous record, compared structurally
  deriving (Show, Eq, Ord)

data KeyArg = KTy KeyType | KKind KeyKind
  deriving (Show, Eq, Ord)

-- | A key argument that is not a type.
data KeyKind
  = KNat Integer
  | KStr T.Text
  | KVoid Kind
  | KRow [(Key, KeyType)]
  deriving (Show, Eq, Ord)
