{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Morloc.Namespace.TypeF
Description : The woven types of code generation
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Namespace.TypeF
  ( FVar (..)
  , TypeF (..)
  , KArgF (.., NatVoidK)
  , ArgF
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Morloc.Namespace.Prim
import Morloc.Namespace.Type

-- The final types used in code generation. The language annotation is removed,
-- since the language for all types within a pool are the same.
--
-- The general type annotation will be used for documentation only
data FVar = FV TVar CVar
  deriving (Show, Ord, Eq)

-- The most minimal type that contains both general and concrete types
data TypeF
  = UnkF FVar -- this should be parameterized by `Type`, since the general type should be known
  | VarF FVar
  | FunF [TypeF] TypeF
  | AppF TypeF [ArgF]
  | NamF NamType FVar [ArgF] [(Key, TypeF)]
  -- | A reference to a record or `data` type, not expanded, at the given
  -- arguments. Inference ties a type's reference to an enclosing
  -- occurrence of itself to this form, with that occurrence's name and
  -- resolved arguments; a serial back-reference projects to it.
  | RecF FVar [ArgF]
  -- | A `data` type whose constructors take no arguments: the type's
  -- name, the type arguments it is applied to, and its constructor names
  -- in declaration order. Position in that list is the wire tag, so the
  -- order is part of the type's wire contract.
  --
  -- The arguments are carried for the same reason 'NamF' carries its
  -- parameters: a user's per-language form may be a template, and the
  -- template is instantiated with them. A type the compiler generates
  -- has a monomorphic name per instantiation and leaves them unread.
  | EnumF FVar [ArgF] [T.Text]
  -- | A `data` type with at least one payload-bearing constructor: the
  -- type's name, its type arguments, and each arm's name with its field
  -- types. Position in the list is the wire tag.
  | VariantF FVar [ArgF] [(T.Text, [TypeF])]
  | OptionalF TypeF
  deriving (Show, Ord, Eq)

-- | A woven argument that is not a type: a Nat or Str value, or an erased
-- placeholder of its kind.
data KArgF
  = NatLitK Integer
  | VoidK Kind
  | StrLitK Text
  | -- | A ground row filling a Rec parameter: its columns.
    RowK [(Key, TypeF)]
  deriving (Show, Ord, Eq)

pattern NatVoidK :: KArgF
pattern NatVoidK = VoidK KindNat

type ArgF = TArg KArgF TypeF

