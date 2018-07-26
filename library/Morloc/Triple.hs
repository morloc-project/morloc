{-# LANGUAGE OverloadedStrings #-}

module Morloc.Triple (
    TopRDF(..)
  , RDF
  , DR.Triple
  , makeTopRDF
  , iuu
  , iun
  , iui
  , iut
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

rdfAppend :: RDF -> RDF -> RDF
rdfAppend x y = makeRDF (DR.triplesOf x ++ DR.triplesOf y)

adoptAs :: String -> Int -> [TopRDF] -> [DR.Triple]
adoptAs r i ys =
       map (link r i) ys
    ++ concat (map (\(TopRDF _ y) -> DR.triplesOf y) ys)
  where
    link :: String -> Int -> TopRDF -> DR.Triple
    link r' i' (TopRDF s _) = iun i' r' s

adoptAs' :: String -> Int -> [TopRDF] -> [DR.Triple]
adoptAs' r i ys =
       zipWith (link r i) [0..] ys
    ++ concat (map (\(TopRDF _ y) -> DR.triplesOf y) ys)
  where
    link :: String -> Int -> Int -> TopRDF -> DR.Triple
    link r' i' index (TopRDF s _) = iun i' (r' ++ "_" ++ show index) s

showTopRDF :: TopRDF -> String
showTopRDF (TopRDF _ rdf) = DR.showGraph rdf

rdfId :: TopRDF -> Int
rdfId (TopRDF (DR.UNode s) _) = read (DT.unpack s)

-- convenience functions for building triples
-- * URI object
iuu :: Int -> String -> String -> DR.Triple
iuu s r o = DR.triple
  (DR.UNode (DT.pack $ show s)) -- URI of the subject
  (DR.UNode (DT.pack r))        -- relation
  (DR.UNode (DT.pack o))        -- URI of the object

-- * indexed URI object as a UNode
iun :: Int -> String -> DR.Node -> DR.Triple
iun s r o = DR.Triple
  (DR.UNode (DT.pack $ show s)) -- URI of the subject
  (DR.UNode (DT.pack r))        -- relation
  o                             -- URI of the object

-- * indexed URI object
iui :: Int -> String -> Int -> DR.Triple
iui s p o = iun s p (DR.UNode (DT.pack . show $ o))

-- * typed object
iut :: Int -> String -> String -> String -> DR.Triple
iut s r t o = DR.triple
  (DR.UNode (DT.pack $ show s))
  (DR.UNode (DT.pack r))
  (DR.LNode (DR.TypedL (DT.pack t) (DT.pack $ show o)))
