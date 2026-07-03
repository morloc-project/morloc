{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.BaseTypes
Description : Predefined base type names and constructors
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Smart constructors for the built-in morloc types (Unit, Int, Real, Bool, Str,
List, Tuple, etc.) as both 'TVar' names and 'TypeU' values. These correspond
to the types that are hardwired into the type system and have special
serialization\/deserialization support.
-}
module Morloc.BaseTypes
  ( unit
  , real
  , f32
  , f64
  , int
  , i8
  , i16
  , i32
  , i64
  , uint
  , u8
  , u16
  , u32
  , u64
  , bool
  , str
  , tuple
  , list
  , vector
  , matrix
  , tensor
  , table
  , record
  , ifileVar
  , istreamVar
  , ostreamVar
  , isIFileHead
  , mlcKindIFile
  , mlcKindIStream
  , mlcKindOStream
  , patternChainVar
  , patternAccessibleClass
  , extractPatternMethod
  , patternChainGrammarVersion
  , unitU
  , realU
  , f32U
  , f64U
  , intU
  , i8U
  , i16U
  , i32U
  , i64U
  , uintU
  , u8U
  , u16U
  , u32U
  , u64U
  , boolU
  , strU
  , tableU
  , tupleU
  , listU
  , effectU
  , optionalU
  , isIntegerBaseType
  , isRealBaseType
  ) where

import Data.Word (Word8)
import Morloc.Data.Text (Text, pretty)
import Morloc.Namespace.Prim (TVar (..))
import Morloc.Namespace.Type (Type (..), TypeU (..), emptyEffectSet)
import Prelude hiding (log)

unit :: TVar
unit = TV "Unit"

real :: TVar
real = TV "Real"

f32 :: TVar
f32 = TV "Float32"

f64 :: TVar
f64 = TV "Float64"

int :: TVar
int = TV "Int"

i8 :: TVar
i8 = TV "Int8"

i16 :: TVar
i16 = TV "Int16"

i32 :: TVar
i32 = TV "Int32"

i64 :: TVar
i64 = TV "Int64"

u8 :: TVar
u8 = TV "UInt8"

uint :: TVar
uint = TV "UInt"

u16 :: TVar
u16 = TV "UInt16"

u32 :: TVar
u32 = TV "UInt32"

u64 :: TVar
u64 = TV "UInt64"

bool :: TVar
bool = TV "Bool"

str :: TVar
str = TV "Str"

list :: TVar
list = TV "List"

tuple :: Int -> TVar
tuple k = TV $ "Tuple" <> pretty k

vector :: TVar
vector = TV "Vector"

matrix :: TVar
matrix = TV "Matrix"

tensor :: Int -> TVar
tensor k = TV $ "Tensor" <> pretty k

table :: TVar
table = TV "Table"

record :: TVar
record = TV "Record"

-- | Handle-type sentinels for cross-pool stream handles. The morloc
-- type system treats them as parameterised newtypes; codegen routes
-- pattern access on these heads through the IFile walker (Express.hs)
-- and the @open kind-byte dispatch (Imperative.hs / Nexus.hs). The
-- 'mlcKind*' Word8 values must agree with morloc.h's MLC_KIND_*
-- defines and 'morloc-runtime-types::packet::MLC_KIND_*'.
ifileVar :: TVar
ifileVar = TV "IFile"

istreamVar :: TVar
istreamVar = TV "IStream"

ostreamVar :: TVar
ostreamVar = TV "OStream"

mlcKindIFile :: Word8
mlcKindIFile = 0

mlcKindIStream :: Word8
mlcKindIStream = 1

mlcKindOStream :: Word8
mlcKindOStream = 2

-- | Names for the PatternAccessible typeclass and its canonical string
-- newtype. Instances of 'PatternAccessible w' provide
-- '__extract_pattern__ :: PatternChain (w a) b -> [?Int64] -> w a -> b',
-- which the compiler and runtime cooperate to dispatch pattern accessors
-- (.foo, .[i], .[s:e:p], .0, .(a, b), and chains) through.
patternChainVar :: TVar
patternChainVar = TV "PatternChain"

patternAccessibleClass :: TVar
patternAccessibleClass = TV "PatternAccessible"

extractPatternMethod :: Text
extractPatternMethod = "__extract_pattern__"

-- | Grammar version for the canonical PatternChain string form. Bumped
-- for every backward-incompatible grammar change so out-of-tree
-- instances can pin against a compatible compiler.
patternChainGrammarVersion :: Int
patternChainGrammarVersion = 1

-- | True if @t@'s head (after peeling EffectT / AppT / OptionalT
-- wrappers) is the IFile sentinel. Used by Express.hs to route
-- pattern access on IFile-typed receivers through the runtime walker
-- rather than the generic Sliceable/Indexable dispatch, and by
-- Nexus.hs to detect IFile-returning intrinsics.
isIFileHead :: Type -> Bool
isIFileHead t = case peel t of
    VarT v -> v == ifileVar
    _      -> False
  where
    peel (EffectT _ inner) = peel inner
    peel (OptionalT inner) = peel inner
    peel (AppT h _)        = peel h
    peel other             = other

unitU :: TypeU
unitU = VarU $ TV "Unit"

realU :: TypeU
realU = VarU $ TV "Real"

f32U :: TypeU
f32U = VarU $ TV "Float32"

f64U :: TypeU
f64U = VarU $ TV "Float64"

intU :: TypeU
intU = VarU $ TV "Int"

i8U :: TypeU
i8U = VarU $ TV "Int8"

i16U :: TypeU
i16U = VarU $ TV "Int16"

i32U :: TypeU
i32U = VarU $ TV "Int32"

i64U :: TypeU
i64U = VarU $ TV "Int64"

u8U :: TypeU
u8U = VarU $ TV "UInt8"

uintU :: TypeU
uintU = VarU $ TV "UInt"

u16U :: TypeU
u16U = VarU $ TV "UInt16"

u32U :: TypeU
u32U = VarU $ TV "UInt32"

u64U :: TypeU
u64U = VarU $ TV "UInt64"

boolU :: TypeU
boolU = VarU $ TV "Bool"

strU :: TypeU
strU = VarU $ TV "Str"

tableU :: TypeU
tableU = VarU $ TV "Table"

listU :: TypeU -> TypeU
listU t = AppU (VarU list) [t]

tupleU :: [TypeU] -> TypeU
tupleU ts = AppU (VarU $ tuple (length ts)) ts

effectU :: TypeU -> TypeU
effectU = EffectU emptyEffectSet

optionalU :: TypeU -> TypeU
optionalU = OptionalU

-- | True if `t` names one of the integer base types: Int / Int8..Int64 /
-- UInt / UInt8..UInt64. Used by the typechecker to allow integer literals
-- to inhabit any integer-family type when annotated (Haskell-style numeric
-- literal defaulting). Int8..Int64 and the unsigned variants are now
-- standalone base types (no longer aliases of Int/UInt) because their
-- wire representations differ; this predicate keeps them treated as a
-- coherent family for literal type-checking.
isIntegerBaseType :: TypeU -> Bool
isIntegerBaseType t = t `elem`
  [intU, i8U, i16U, i32U, i64U, uintU, u8U, u16U, u32U, u64U]

-- | True if `t` names one of the real-valued base types: Real / Float32 /
-- Float64. Counterpart to isIntegerBaseType for real literal defaulting.
isRealBaseType :: TypeU -> Bool
isRealBaseType t = t `elem` [realU, f32U, f64U]
