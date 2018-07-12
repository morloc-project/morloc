-- I want to translate the entire Morloc program into an RDF triplet store.

module Morloc.Triple (
    Triple(..)
  , Subject
  , Relation
  , Object(..)
) where

type Triple = (Subject, Relation, Object)
type Subject  = Int
type Relation = String
data Object
  = Id' Subject
  | Int' Integer
  | Num' Double
  | Log' Bool
  | Str' String
  deriving(Ord, Eq)

instance Show Object where
  show (Id'  x ) = show x
  show (Int' x ) = show x
  show (Num' x ) = show x
  show (Log' x ) = if x then "true" else "false"
  show (Str' x ) = x
