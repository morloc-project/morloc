{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.CodeGenerator.NumericLiteral
Description : Single source of truth for numeric-literal codegen
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

Every wire-form emission for @IntS@ or @RealS@ at any numeric slot
flows through 'resolveNumericLiteral'. The AST node kind (@IntS@ vs
@RealS@) records source syntax; the target 'SerialAST' decides the
wire form. @IntS@ accepts any numeric slot (promotion to Real is total
for Integer values within Float32/Float64 range); @RealS@ only accepts
real slots -- Scientific-to-integer is lossy and rejected at
typecheck.

Bounds checking (integer overflow against target width, real overflow
against IEEE-754 max) lives here so the two callers (nexus wire codegen
and any future pool-side check) share the same table.
-}
module Morloc.CodeGenerator.NumericLiteral
  ( NumLitSrc (..)
  , IntKind (..)
  , RealKind (..)
  , ResolvedNumLit (..)
  , resolveNumericLiteral
  ) where

import qualified Data.Scientific as DS
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Monad as MM

-- | Source-level numeric literal: what the user wrote at the syntax
-- level. Preserved through typecheck so error messages can distinguish
-- "integer literal N" from "float literal x" post-promotion.
data NumLitSrc
  = IntSrc  Integer
  | RealSrc RealLit

-- | Fixed-width integer targets. IVar/UVar are the variable-width
-- Int/UInt whose actual width depends on the backend (32-bit in R and
-- C++, unbounded in Python) -- bounds-checked against UInt max where
-- meaningful.
data IntKind
  = I8 | I16 | I32 | I64 | IVar
  | U8 | U16 | U32 | U64 | UVar
  deriving (Show, Eq)

-- | Real targets. Real and Float64 share IEEE-754 binary64 storage;
-- Float32 is distinct only in its narrower overflow bound.
data RealKind = F32 | F64 deriving (Show, Eq)

-- | The resolved literal after (source, target) dispatch. Callers
-- pattern-match to render into their target form: 'Nexus.hs' maps to
-- the 'LitType' wire markers (I8X, F32X, ...); pool codegen could map
-- to 'IIntLit'/'IRealLit'.
data ResolvedNumLit
  = ResolvedInt  IntKind Integer
  | ResolvedReal RealKind RealLit

-- | Resolve a numeric literal against its target slot. The source
-- index is used for error localization -- pass the literal's own
-- source-map index (litIx), not the wrapping AnnoS index.
resolveNumericLiteral
  :: Int          -- ^ source-map index for error caret
  -> NumLitSrc    -- ^ what the user wrote
  -> SerialAST   -- ^ what the wire slot needs
  -> MorlocMonad ResolvedNumLit
resolveNumericLiteral i src s = case (src, s) of
  (IntSrc n, SerialInt8  _)   -> checkIntRange i "Int8"   n (-128) 127                >> pure (ResolvedInt I8   n)
  (IntSrc n, SerialInt16 _)   -> checkIntRange i "Int16"  n (-32768) 32767            >> pure (ResolvedInt I16  n)
  (IntSrc n, SerialInt32 _)   -> checkIntRange i "Int32"  n (-2147483648) 2147483647  >> pure (ResolvedInt I32  n)
  (IntSrc n, SerialInt64 _)   -> checkIntRange i "Int64"  n (-9223372036854775808) 9223372036854775807
                                                                                     >> pure (ResolvedInt I64  n)
  (IntSrc n, SerialInt   _)   -> pure (ResolvedInt IVar n)
  (IntSrc n, SerialUInt8  _)  -> checkIntRange i "UInt8"  n 0 255                     >> pure (ResolvedInt U8   n)
  (IntSrc n, SerialUInt16 _)  -> checkIntRange i "UInt16" n 0 65535                   >> pure (ResolvedInt U16  n)
  (IntSrc n, SerialUInt32 _)  -> checkIntRange i "UInt32" n 0 4294967295              >> pure (ResolvedInt U32  n)
  (IntSrc n, SerialUInt64 _)  -> checkIntRange i "UInt64" n 0 18446744073709551615    >> pure (ResolvedInt U64  n)
  (IntSrc n, SerialUInt   _)  -> checkIntRange i "UInt"   n 0 18446744073709551615    >> pure (ResolvedInt UVar n)

  -- Integer promoted to real: bounds-check against the IEEE-754 max of
  -- the target width. fromInteger is total on Scientific (arbitrary
  -- precision); toRealFloat collapses out-of-range to infinity, which
  -- isInfinite detects.
  (IntSrc n, SerialFloat32 _) -> promoteInt i F32 f32Bound n
  (IntSrc n, SerialFloat64 _) -> promoteInt i F64 f64Bound n
  (IntSrc n, SerialReal    _) -> promoteInt i F64 f64Bound n

  (RealSrc r, SerialFloat32 _) -> checkRealRange i "Float32" f32Bound r checkF32Overflow >> pure (ResolvedReal F32 r)
  (RealSrc r, SerialFloat64 _) -> checkRealRange i "Float64" f64Bound r checkF64Overflow >> pure (ResolvedReal F64 r)
  (RealSrc r, SerialReal    _) -> checkRealRange i "Float64" f64Bound r checkF64Overflow >> pure (ResolvedReal F64 r)

  -- Real source at integer target is rejected at typecheck (RealS's
  -- checkE only accepts real base types); non-numeric SerialAST likewise
  -- should never reach codegen for a numeric literal.
  (RealSrc _, _) | Just name <- integerSerialName s ->
      MM.throwCompilerBugAt i $
        "RealS reached codegen against" <+> name <+> "target; typecheck should have rejected"
  _ -> MM.throwCompilerBugAt i
         "numeric literal reached codegen at a non-numeric wire slot"

integerSerialName :: SerialAST -> Maybe MDoc
integerSerialName (SerialInt8   _) = Just "Int8"
integerSerialName (SerialInt16  _) = Just "Int16"
integerSerialName (SerialInt32  _) = Just "Int32"
integerSerialName (SerialInt64  _) = Just "Int64"
integerSerialName (SerialInt    _) = Just "Int"
integerSerialName (SerialUInt8  _) = Just "UInt8"
integerSerialName (SerialUInt16 _) = Just "UInt16"
integerSerialName (SerialUInt32 _) = Just "UInt32"
integerSerialName (SerialUInt64 _) = Just "UInt64"
integerSerialName (SerialUInt   _) = Just "UInt"
integerSerialName _                = Nothing

-- Integer promoted to a real target: report as an "Integer literal"
-- overflow, not a "Float literal" one, so the diagnostic matches the
-- source token even after the fromInteger conversion.
promoteInt :: Int -> RealKind -> MDoc -> Integer -> MorlocMonad ResolvedNumLit
promoteInt i kind bound n =
  let sci = fromInteger n :: DS.Scientific
      (targetName, overflows) = case kind of
        F32 -> ("Float32" :: MDoc, checkF32Overflow)
        F64 -> ("Float64",         checkF64Overflow)
  in if overflows sci
       then MM.throwSourcedError i $
              "Integer literal" <+> pretty n
              <+> "overflows" <+> targetName
              <+> parens bound
       else pure (ResolvedReal kind (RealFinite sci))

checkIntRange :: Int -> MDoc -> Integer -> Integer -> Integer -> MorlocMonad ()
checkIntRange i name v lo hi
  | v < lo || v > hi = MM.throwSourcedError i $
      "Integer literal" <+> pretty v
      <+> "overflows" <+> name
      <+> "(range" <+> pretty lo <+> "to" <+> pretty hi <> ")"
  | otherwise = pure ()

-- Non-finite RealLit variants are user-written keywords with no
-- magnitude to overflow; only finite payloads are bound-checked.
checkRealRange :: Int -> MDoc -> MDoc -> RealLit -> (DS.Scientific -> Bool) -> MorlocMonad ()
checkRealRange i name bound (RealFinite v) overflows
  | overflows v = MM.throwSourcedError i $
      "Float literal" <+> pretty (show v)
      <+> "overflows" <+> name
      <+> parens bound
  | otherwise = pure ()
checkRealRange _ _ _ _ _ = pure ()

checkF32Overflow :: DS.Scientific -> Bool
checkF32Overflow v = isInfinite (DS.toRealFloat v :: Float)

checkF64Overflow :: DS.Scientific -> Bool
checkF64Overflow v = isInfinite (DS.toRealFloat v :: Double)

f32Bound :: MDoc
f32Bound = "|x| > 3.4e38"

f64Bound :: MDoc
f64Bound = "|x| > 1.8e308"
