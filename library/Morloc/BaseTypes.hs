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
  , int
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

int :: TVar
int = TV "Int"

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
