{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Triple
Description : Convenience functions for working with triples
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Triple (
    TopRDF(..)
  , RDF
  , DR.Triple
  , makeTopRDF
  , uss
  , usu
  , ust
  , idUri
  , rdfId
  , adoptAs
  , adopt
  , showTopRDF
  , rdfAppend
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Map.Strict as DMS

prefixMap :: DR.PrefixMappings
prefixMap = DR.PrefixMappings $
  DMS.singleton "morloc" "http://www.morloc.io/ontology/000/" 

type RDF = DR.RDF DR.TList

data TopRDF = TopRDF DR.Node RDF deriving(Show)

makeTopRDF :: DR.Node -> [DR.Triple] -> TopRDF
makeTopRDF i ts = TopRDF i (makeRDF ts)

makeRDF :: [DR.Triple] -> RDF
makeRDF xs = DR.mkRdf xs Nothing prefixMap

rdfAppend :: RDF -> RDF -> RDF
rdfAppend x y = makeRDF (DR.triplesOf x ++ DR.triplesOf y)

adoptAs :: String -> DR.Node -> [TopRDF] -> [DR.Triple]
adoptAs rel sbj objs =
       map (link rel sbj) objs
    ++ concat (map (\(TopRDF _ obj) -> DR.triplesOf obj) objs)
  where
    link :: String -> DR.Node -> TopRDF -> DR.Triple
    link rel' sbj' (TopRDF obj' _) = usu sbj' rel' obj'

adopt :: DR.Node -> [TopRDF] -> [DR.Triple]
adopt sbj objs =
       zipWith (link sbj) [0..] objs
    ++ concat (map (\(TopRDF _ obj) -> DR.triplesOf obj) objs)
  where
    link :: DR.Node -> Int -> TopRDF -> DR.Triple
    link sbj' index (TopRDF obj' _) = usu obj' ("rdf:_" ++ show index) sbj'

showTopRDF :: TopRDF -> String
showTopRDF (TopRDF _ rdf) = DR.showGraph rdf

idUri :: Maybe String -> Int -> DR.Node
idUri Nothing  i = DR.UNode . DT.pack $ "mid:" ++ show i
idUri (Just s) i = DR.UNode . DT.pack $ "mid_" ++ s ++ ":" ++ show i

rdfId :: TopRDF -> DR.Node
rdfId (TopRDF i _) = i

-- convenience functions for building triples
-- * URI object
uss :: DR.Node -> String -> String -> DR.Triple
uss s r o = DR.triple s (DR.UNode (DT.pack r)) (DR.UNode (DT.pack o))

-- * indexed URI object as a UNode
usu :: DR.Node -> String -> DR.Node -> DR.Triple
usu s r o = DR.triple s (DR.UNode (DT.pack r)) o

-- * typed object
ust :: DR.Node -> String -> String -> String -> DR.Triple
ust s r t o = DR.triple
  s
  (DR.UNode (DT.pack r))
  (DR.LNode (DR.TypedL (DT.pack t) (DT.pack o)))
