module Morloc.Triple (
    Triple(..)
  , Subject
  , Relation
  , Object(..)
  , showRDF
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

-- write triplets in TAB-delimited format
showRDF :: [Triple] -> String 
showRDF = unlines . map (\(i,r,o) -> show i ++ "\t" ++ r ++ "\t" ++ show o)
