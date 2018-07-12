-- I want to translate the entire Morloc program into an RDF triplet store.

module Morloc.Triple (
    Triple
  , Subject
  , RelObj(..)
  , Thing(..)
  , Primitive(..)
) where

import Morloc.Syntax
import Morloc.Data

type Triple = (Subject, RelObj)

type Subject = Int

data RelObj
  = IsA'     Thing
  | Name'    String
  | Parent'  Subject
  | Lang'    String
  | File'    String
  | Alias'   String
  | Role'    String
  | Cons'    Subject
  | Binop'   Subject
  | LHS'     Subject
  | RHS'     Subject
  | Appl'    [Subject]
  | Args'    [Subject]
  | Param'   [Subject]
  | Value'   Primitive 
  deriving (Ord, Eq, Show)

data Thing
  = FunctionSignature'
  | TypeSignature'
  | Declaration'
  | Import'
  | Module'
  | List'
  | Tuple'
  | Record'
  deriving (Ord, Eq, Show)

data Primitive 
  = Int' Integer
  | Num' Double
  | Log' Bool
  | Str' String
  | Arr' [Subject]
  deriving (Ord, Eq, Show)
