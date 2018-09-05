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
  , prefixMap
  , DR.Triple
  , makeTopRDF
  , idUri
  , rdfId
  , adoptAs
  , adopt
  , showTopRDF
  , rdfAppend
  -- * RDF Prefixes
  , mlcPre
  , midPre
  , rdfPre
  , xsdPre
  -- * RDF access
  , getImportedFiles
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Map.Strict as DMS
import qualified Data.Maybe as DM
import qualified Safe

import Morloc.Operators
import Morloc.Util (show')

mlcPre :: DT.Text
mlcPre = "http://www.morloc.io/ontology/000/"

midPre :: DT.Text
midPre = "http://www.morloc.io/XXX/mid/"

rdfPre :: DT.Text
rdfPre = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

xsdPre :: DT.Text
xsdPre = "http://www.w3.org/2001/XMLSchema#"

prefixMap :: DR.PrefixMappings
prefixMap = DR.PrefixMappings $ DMS.fromList
  [("mlc", mlcPre),
   ("mid", midPre),
   ("rdf", rdfPre),
   ("xsd", xsdPre)
  ]

type RDF = DR.RDF DR.TList

data TopRDF = TopRDF DR.Node RDF deriving(Show)

makeTopRDF :: DR.Node -> [DR.Triple] -> TopRDF
makeTopRDF i ts = TopRDF i (makeRDF ts)

makeRDF :: [DR.Triple] -> RDF
makeRDF xs = DR.mkRdf xs Nothing prefixMap

rdfAppend :: RDF -> RDF -> RDF
rdfAppend x y = makeRDF (DR.triplesOf x ++ DR.triplesOf y)

adoptAs :: DR.Node -> DR.Node -> [TopRDF] -> [DR.Triple]
adoptAs rel sbj objs =
       map (link rel sbj) objs
    ++ concat (map (\(TopRDF _ obj) -> DR.triplesOf obj) objs)
  where
    link :: DR.Node -> DR.Node -> TopRDF -> DR.Triple
    link rel' sbj' (TopRDF obj' _) = DR.triple sbj' rel' obj'

adopt :: DR.Node -> [TopRDF] -> [DR.Triple]
adopt sbj objs =
       zipWith (link sbj) [0..] objs
    ++ concat (map (\(TopRDF _ obj) -> DR.triplesOf obj) objs)
  where
    link :: DR.Node -> Int -> TopRDF -> DR.Triple
    link sbj' index (TopRDF obj' _)
      = DR.triple obj' (rdfPre .:. ("_" <> show' index)) sbj'

showTopRDF :: TopRDF -> DT.Text
showTopRDF (TopRDF _ rdf) = DT.pack $ DR.showGraph rdf

idUri :: Maybe DT.Text -> Int -> DR.Node
idUri Nothing  i = midPre .:. show' i
idUri (Just s) i = midPre .:. (s <> "_" <> show' i)

rdfId :: TopRDF -> DR.Node
rdfId (TopRDF i _) = i


-- The last survivors of the Walker module
-- =======================================

valueOf :: DR.Node -> [DT.Text]
valueOf (DR.LNode (DR.TypedL s _)) = [s]
valueOf (DR.LNode (DR.PlainL s)) = [s]
valueOf _ = []

-- Down :: Subject -> [Object]
down :: DR.Rdf a
  => DR.RDF a
  -> DR.Predicate
  -> DR.Subject    -- (Dr.Subject -> [Dr.Object]) is the monadic
  -> [DR.Object]   -- chain function, allows searching in parallel
down rdf p' s' = DR.query rdf (Just s') (Just p') Nothing |>> DR.objectOf

up :: DR.Rdf a
  => DR.RDF a
  -> DR.Predicate
  -> DR.Object      -- (Dr.Subject -> [Dr.Object]) is the monadic
  -> [DR.Subject]   -- chain function, allows searching in parallel
up rdf p' o' = DR.query rdf Nothing (Just p') (Just o') |>> DR.subjectOf

getImportedFiles :: DR.Rdf a => DR.RDF a -> [DT.Text]
getImportedFiles rdf
  =   up rdf (rdfPre .:. "type") (mlcPre .:. "import")
  >>= down rdf (mlcPre .:. "name")
  >>= valueOf
