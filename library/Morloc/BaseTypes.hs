{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.BaseTypes
Description : Definitions and functions for handling base types
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.BaseTypes
  ( unit
  , real
  , f32, f64
  , int
  , i8, i16, i32, i64
  , uint
  , u8, u16, u32, u64
  , bool
  , str
  , tuple
  , list
  , record
  , unitU
  , realU
  , intU
  , boolU
  , strU
  , tupleU
  , listU
  ) where

import Prelude hiding(log)
import Morloc.Namespace
import Morloc.Data.Text (pretty)

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
unitU = VarU unit

realU :: TypeU
realU = VarU real

intU :: TypeU
intU = VarU int

boolU :: TypeU
boolU = VarU bool

strU :: TypeU
strU = VarU str

listU :: TypeU -> TypeU
listU t = AppU (VarU list) [t]

tupleU :: [TypeU] -> TypeU
tupleU ts = AppU (VarU $ tuple (length ts)) ts
