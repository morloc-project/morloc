module Morloc.Triple (
    TopRDF(..)
  , RDF
  , DR.Triple
  , makeTopRDF
  , tripleL -- make leaf triple (value object)
  , tripleN -- make node triple (URI object)
  , asId
  , fromId
  , adoptAs
  , rdfId
  , showTopRDF
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Map.Strict as DMS

type RDF = DR.RDF DR.AdjHashMap

data TopRDF = TopRDF DR.Node RDF deriving(Show)

makeTopRDF :: Int -> [DR.Triple] -> TopRDF
makeTopRDF i ts = TopRDF (asId i) (makeRDF ts)

makeRDF :: [DR.Triple] -> RDF
makeRDF xs = DR.mkRdf xs Nothing (DR.PrefixMappings DMS.empty)

asId :: Int -> DR.Node
asId i = DR.UNode (DT.pack . show $ i)

fromId :: DR.Node -> Int
fromId (DR.UNode s) = read . show $ s

tripleL :: Show a => Int -> String -> String -> a -> DR.Triple
tripleL s r t o = DR.Triple
  (DR.UNode (DT.pack $ show s))
  (DR.UNode (DT.pack r))
  (DR.LNode (DR.TypedL (DT.pack t) (DT.pack $ show o)))

tripleN :: Int -> String -> DR.Node -> DR.Triple
tripleN s r o = DR.Triple
  (DR.UNode (DT.pack $ show s)) -- URI of the subject
  (DR.UNode (DT.pack r))        -- relation
  o                             -- URI of the object

rdfAppend :: RDF -> RDF -> RDF
rdfAppend x y = DR.mkRdf
  (DR.triplesOf x ++ DR.triplesOf y)
  Nothing
  (DR.PrefixMappings DMS.empty)

adoptAs :: String -> Int -> [TopRDF] -> [DR.Triple]
adoptAs r i ys =
       map (link r i) ys
    ++ concat (map (\(TopRDF _ y) -> DR.triplesOf y) ys)
  where
    link :: String -> Int -> TopRDF -> DR.Triple
    link r' i' (TopRDF s _) = tripleN i' r' s

showTopRDF :: TopRDF -> String
showTopRDF (TopRDF _ rdf) = DR.showGraph rdf
  
rdfId :: TopRDF -> Int
rdfId (TopRDF (DR.UNode s) _) = read (show s)
