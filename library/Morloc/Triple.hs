-- I want to translate the entire Morloc program into an RDF triplet store.

module Morloc.Triple (
    Triple(..)
  , Relation(..)
  , Key
) where

import Morloc.Syntax
import Morloc.Data

type Key = Int

data Triple = Triple Key Relation

data Relation 
  = RIsA String -- replace string with some algebraic type describing all
                -- possible language types
  | RPosArg Int MData  -- the nth positional parameter
  | RKeyArg Int MData
  | RHasValue MData
  | RType MType
  | RConstraint
