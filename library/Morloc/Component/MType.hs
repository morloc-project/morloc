{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Component.MType
Description : Build manifolds for code generation from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.MType (fromSparqlDb) where

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

fromSparqlDb :: SparqlEndPoint -> IO (Map.Map Key MType)
fromSparqlDb = MCU.simpleGraph toMType getParentData id sparqlQuery

getParentData :: [Maybe DT.Text] -> (DT.Text, DT.Text) 
getParentData [Just t, Just v] = (t, v)
getParentData _ = error "Unexpected SPARQL result"

toMType :: Map.Map Key ((DT.Text, DT.Text), [Key]) -> Key -> MType
toMType h k = toMType' (Map.lookup k h) where
  toMType' :: (Maybe ((DT.Text, DT.Text), [Key])) -> MType
  toMType' (Just ((t, v), xs)) = MType v (map (toMType h) xs)

sparqlQuery :: SparqlEndPoint -> IO [[Maybe DT.Text]]
sparqlQuery = [sparql|
PREFIX mlc: <http://www.morloc.io/ontology/000/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX mid: <http://www.morloc.io/XXX/mid/>
SELECT ?id ?element ?child ?type ?v 
WHERE {
    ?id rdf:type mlc:type ;
        rdf:type ?type .
    FILTER(?type != mlc:type)
    OPTIONAL {
        ?id rdf:value ?v
    }
    OPTIONAL {
        ?id ?element ?child
        FILTER(regex(str(?element), "_[0-9]+$", "i"))
    }
}
|] 
