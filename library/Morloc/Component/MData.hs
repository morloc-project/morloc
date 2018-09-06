{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Component.MData
Description : Build manifolds for code generation from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.MData (fromSparqlDb) where

import Morloc.Types
import Morloc.Operators
import Morloc.Quasi
import qualified Morloc.Triple as M3
import qualified Morloc.Component.Util as MCU

import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))
import Morloc.Database.HSparql.Connection
import qualified Data.Map.Strict as Map
import qualified Data.List.Extra as DLE
import qualified Data.Text as DT

fromSparqlDb :: SparqlEndPoint -> IO (Map.Map Key MData)
fromSparqlDb = MCU.simpleGraph toMData getParentData id sparqlQuery

getParentData :: [Maybe DT.Text] -> (DT.Text, Maybe DT.Text) 
getParentData [Just t, v] = (t, v)
getParentData _ = error "Unexpected SPARQL result"

toMData :: Map.Map Key ((DT.Text, Maybe DT.Text), [Key]) -> Key -> MData
toMData h k = toMData' (Map.lookup k h) where
  toMData' :: (Maybe ((DT.Text, Maybe DT.Text), [Key])) -> MData
  -- primitive "leaf" data
  toMData' (Just ((mtype, Just x), _))
    | mtype == M3.mlcPre <> "number"  = Num' x
    | mtype == M3.mlcPre <> "string"  = Str' x
    | mtype == M3.mlcPre <> "boolean" = Log' (x == "true")
    | otherwise = error "Unexpected type ..."
  -- containers "node" data
  toMData' (Just ((mtype, _), xs))
    | mtype == M3.mlcPre <> "list"   = Lst' (map (toMData h) xs)
    | mtype == M3.mlcPre <> "tuple"  = Tup' (map (toMData h) xs)
    | mtype == M3.mlcPre <> "record" = error "Records not yet supported"
    | otherwise = error "Unexpected type ..."
  -- shit happens
  toMData' _ = error "Unexpected type"

sparqlQuery :: SparqlEndPoint -> IO [[Maybe DT.Text]]
sparqlQuery = [sparql|
PREFIX mlc: <http://www.morloc.io/ontology/000/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX mid: <http://www.morloc.io/XXX/mid/>
SELECT ?id ?element ?child ?type ?v 
WHERE {
    ?id rdf:type mlc:data ;
        rdf:type ?type .
    FILTER(?type != mlc:data)
    OPTIONAL {
        ?id rdf:value ?v
    }
    OPTIONAL {
        ?id ?element ?child
        FILTER(regex(str(?element), "_[0-9]+$", "i"))
    }
}
|]
