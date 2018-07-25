{-# LANGUAGE OverloadedStrings #-}

module Morloc.Triple (
    TopRDF(..)
  , RDF
  , DR.Triple
  , makeTopRDF
  , tripleL -- make leaf triple (value object)
  , tripleN -- make node triple (URI object)
  , tripleN' -- make node triple (URI object)
  , asId
  , adoptAs
  , adoptAs'
  , rdfId
  , showTopRDF
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Map.Strict as DMS

import qualified Morloc.Util as MU

prefixMap :: DR.PrefixMappings
prefixMap = DR.PrefixMappings $
  DMS.singleton "morloc" "http://www.morloc.io/ontology/000/" 

type RDF = DR.RDF DR.TList

data TopRDF = TopRDF DR.Node RDF deriving(Show)

makeTopRDF :: Int -> [DR.Triple] -> TopRDF
makeTopRDF i ts = TopRDF (asId i) (makeRDF ts)

makeRDF :: [DR.Triple] -> RDF
makeRDF xs = DR.mkRdf xs Nothing prefixMap

asId :: Int -> DR.Node
asId i = DR.UNode (DT.pack . show $ i)

tripleL :: Show a => Int -> String -> String -> a -> DR.Triple
tripleL s r t o = DR.triple
  (DR.UNode (DT.pack $ show s))
  (DR.UNode (DT.pack r))
  (DR.LNode (DR.TypedL (DT.pack t) (DT.pack $ show o)))

tripleN :: Int -> String -> DR.Node -> DR.Triple
tripleN s r o = DR.Triple
  (DR.UNode (DT.pack $ show s)) -- URI of the subject
  (DR.UNode (DT.pack r))        -- relation
  o                             -- URI of the object

tripleN' :: Int -> String -> String -> DR.Triple
tripleN' s r o = DR.triple
  (DR.UNode (DT.pack $ show s)) -- URI of the subject
  (DR.UNode (DT.pack r))        -- relation
  (DR.UNode (DT.pack o))        -- URI of the object

rdfAppend :: RDF -> RDF -> RDF
rdfAppend x y = makeRDF (DR.triplesOf x ++ DR.triplesOf y)

adoptAs :: String -> Int -> [TopRDF] -> [DR.Triple]
adoptAs r i ys =
       map (link r i) ys
    ++ concat (map (\(TopRDF _ y) -> DR.triplesOf y) ys)
  where
    link :: String -> Int -> TopRDF -> DR.Triple
    link r' i' (TopRDF s _) = tripleN i' r' s

adoptAs' :: String -> Int -> [TopRDF] -> [DR.Triple]
adoptAs' r i ys =
       zipWith (link r i) [0..] ys
    ++ concat (map (\(TopRDF _ y) -> DR.triplesOf y) ys)
  where
    link :: String -> Int -> Int -> TopRDF -> DR.Triple
    link r' i' index (TopRDF s _) = tripleN i' (r' ++ "_" ++ show index) s

showTopRDF :: TopRDF -> String
showTopRDF (TopRDF _ rdf) = DR.showGraph rdf

rdfId :: TopRDF -> Int
rdfId (TopRDF (DR.UNode s) _) = read (show s)
