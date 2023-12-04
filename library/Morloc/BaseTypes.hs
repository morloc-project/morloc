{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.BaseTypes
Description : Definitions and functions for handling base types
Copyright   : (c) Zebulun Arendsee, 2023
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
import Morloc.Data.Text (pretty, Text)

unit :: Text
unit = "Unit"

real :: Text
real = "Real"

int :: Text
int = "Int"

bool :: Text
bool = "Bool"

str :: Text
str = "Str"

list :: Text
list = "List"

tuple :: Int -> Text
tuple k = "Tuple" <> pretty k

record :: Text
record = "Record"


unitU :: TypeU
unitU = VarU . TV Nothing $ unit

realU :: TypeU
realU = VarU . TV Nothing $ real

intU :: TypeU
intU = VarU . TV Nothing $ int

boolU :: TypeU
boolU = VarU . TV Nothing $ bool

strU :: TypeU
strU = VarU . TV Nothing $ str

listU :: TypeU -> TypeU
listU t = AppU (VarU . TV Nothing $ list) [t]

tupleU :: [TypeU] -> TypeU
tupleU ts = AppU (VarU . TV Nothing $ tuple (length ts)) ts
