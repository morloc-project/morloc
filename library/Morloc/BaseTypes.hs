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
  , closeTmpUnlinkMarker
  , isIFileHead
  , mlcKindIFile
  , mlcKindIStream
  , mlcKindOStream
  , handleStorageType
  , handleStorageTypeF
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
import Morloc.CodeGenerator.Namespace (TypeF (..), FVar (..), CVar (..))
import Morloc.Namespace.Prim (TVar (..))
import Morloc.Namespace.Type (Type (..), TypeU (..), emptyEffectSet)
import Prelude hiding (log)

unit :: TVar
unit = TV "Unit"

real :: TVar
real = TV "Real"

f32 :: TVar
f32 = TV "F32"

f64 :: TVar
f64 = TV "F64"

int :: TVar
int = TV "Int"

i8 :: TVar
i8 = TV "I8"

i16 :: TVar
i16 = TV "I16"

i32 :: TVar
i32 = TV "I32"

i64 :: TVar
i64 = TV "I64"

u8 :: TVar
u8 = TV "U8"

uint :: TVar
uint = TV "UInt"

u16 :: TVar
u16 = TV "U16"

u32 :: TVar
u32 = TV "U32"

u64 :: TVar
u64 = TV "U64"

bool :: TVar
bool = TV "Bool"

str :: TVar
str = TV "Str"

-- | Sentinel stored in an @close@ intrinsic's schema slot when its argument is
-- a @Str@ path rather than a stream handle. Codegen sees this marker and emits
-- the registered-temp-file unlink (mlc_unlink_tmp) instead of a handle close.
-- Not a real msgpack schema; only ever compared, never parsed.
closeTmpUnlinkMarker :: Text
closeTmpUnlinkMarker = "@close-unlink-tmp"

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

-- | The storage type of a handle target: the type that lives on the
-- wire / on disk under the handle. IFile is a single-value container
-- (like @Maybe a@ in Haskell); IStream and OStream are list-shaped
-- (like @List a@ -- a stream is a sequence of a-values).
--
-- Given a handle constructor's 'TVar' and its element type @a@:
--
--   * @IFile a@   -> @a@       (single value)
--   * @IStream a@ -> @[a]@     (list-shaped)
--   * @OStream a@ -> @[a]@     (list-shaped)
--   * anything else -> @a@     (identity fallback; unreachable at
--                              typechecked call sites)
--
-- This is the single source of truth for the "streams are list-shaped"
-- invariant. Every codegen path that emits an on-wire schema for a
-- handle operation (@open, @append, @stdin/out/err) computes the
-- storage type here and passes it to its type-to-SerialAST converter.
--
-- The runtime enforces the same invariant on the read side
-- (@reject_non_list_stream_schema@ in @morloc-runtime/stream.rs@).
handleStorageType :: TVar -> Type -> Type
handleStorageType v a
  | v == ostreamVar || v == istreamVar = AppT (VarT list) [a]
  | v == ifileVar                      = a
  | otherwise                          = a

-- | 'TypeF' twin of 'handleStorageType'. Same rule, different type
-- representation. The 'CVar' slot on the synthesised list 'FVar' is
-- empty because the pool-side @makeSerialAST@ will resolve the
-- concrete language binding via the alias chain.
handleStorageTypeF :: TVar -> TypeF -> TypeF
handleStorageTypeF v a
  | v == ostreamVar || v == istreamVar =
      AppF (VarF (FV list (CV ""))) [a]
  | v == ifileVar = a
  | otherwise     = a

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
f32U = VarU $ TV "F32"

f64U :: TypeU
f64U = VarU $ TV "F64"

intU :: TypeU
intU = VarU $ TV "Int"

i8U :: TypeU
i8U = VarU $ TV "I8"

i16U :: TypeU
i16U = VarU $ TV "I16"

i32U :: TypeU
i32U = VarU $ TV "I32"

i64U :: TypeU
i64U = VarU $ TV "I64"

u8U :: TypeU
u8U = VarU $ TV "U8"

uintU :: TypeU
uintU = VarU $ TV "UInt"

u16U :: TypeU
u16U = VarU $ TV "U16"

u32U :: TypeU
u32U = VarU $ TV "U32"

u64U :: TypeU
u64U = VarU $ TV "U64"

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
