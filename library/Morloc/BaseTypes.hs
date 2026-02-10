{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.BaseTypes
Description : Definitions and functions for handling base types
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
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
  , record
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
  , tupleU
  , listU
  ) where

import Morloc.Data.Text (pretty)
import Morloc.Namespace.Prim (TVar(..))
import Morloc.Namespace.Type (TypeU(..))
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

record :: TVar
record = TV "Record"

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

listU :: TypeU -> TypeU
listU t = AppU (VarU list) [t]

tupleU :: [TypeU] -> TypeU
tupleU ts = AppU (VarU $ tuple (length ts)) ts
