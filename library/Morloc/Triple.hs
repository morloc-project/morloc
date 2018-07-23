module Morloc.Triple (
    RDF(..)
  , Triple(..)
  , Subject
  , Relation
  , Object(..)
  , rdfId
  , rdfTriple
  , showRDF
  , adoptAs
  , addTriples
) where

data RDF = RDF
  Subject  -- the top element
  [Triple] -- RDF list
  deriving(Ord, Eq, Show)

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
showRDF :: RDF -> String 
showRDF (RDF _ xs) = unlines . map writeTriple $ xs where
  writeTriple :: (Subject, Relation, Object) -> String
  writeTriple (i, r, o) = show i ++ "\t" ++ r ++ "\t" ++ show o

rdfId :: RDF -> Subject
rdfId (RDF i _) = i

rdfTriple :: RDF -> [Triple]
rdfTriple (RDF _ xs) = xs

adoptAs :: Relation -> Subject -> [RDF] -> [Triple]
adoptAs r i = concat . map (\(RDF j xs) -> (i, r, Id' j):xs)

addTriples :: RDF -> [Triple] -> RDF
addTriples (RDF i xs) ys = RDF i (xs ++ ys)
