{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Component.Serializer
Description : Build JSON packers and unpackers from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.Serializer (
    fromSparqlDb
  , SerialMap(..)
) where

import Morloc.Types
import Morloc.Operators
import Morloc.Quasi
import qualified Morloc.Util as MU
import qualified Morloc.Component.MType as MCM 
import qualified Data.Maybe as DM

import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))
import Morloc.Database.HSparql.Connection
import qualified Data.Map.Strict as Map
import qualified Data.Text as DT

type SerialData =
  ( Key  -- type_id - 
  , Name -- property - "packs" or "unpackes"
  , Bool -- is_generic - is this a generic packer/unpacker
  , Name -- name
  , Path -- path
  )

-- TODO: update this to limit results to one language
-- OR return a hash of hashes by language
fromSparqlDb :: Lang -> SparqlEndPoint -> IO SerialMap
fromSparqlDb lang ep
  =   toSerialMap
  <$> MCM.fromSparqlDb ep
  <*> (map tuplify <$> sparqlQuery lang' ep)
  where

    tuplify :: [Maybe Text] -> SerialData
    -- typename | property | is_generic | name | path
    tuplify [Just t, Just p, Just g, Just n, Just s] = (t,p,g == "true",n,s)
    tuplify e = error ("Unexpected SPARQL result: " ++ show e)

    lang' = dquotes (text' lang)

    toSerialMap
      :: Map.Map Key ConcreteType 
      -> [SerialData]
      -> SerialMap
    toSerialMap h xs = case
      ( Map.fromList [(getIn  (lookupOrDie t h), p) | (t, "packs"  , False, p, _) <- xs]
      , Map.fromList [(getOut (lookupOrDie t h), p) | (t, "unpacks", False, p, _) <- xs]
      , [p | (_, "packs"  , True, p, _) <- xs]
      , [p | (_, "unpacks", True, p, _) <- xs]
      , MU.unique [s | (_, _, _, _, s) <- xs]
      ) of
        (phash, uhash, [p], [u], srcs) -> SerialMap
          { serialLang = lang
          , serialPacker = phash
          , serialUnpacker = uhash
          , serialGenericPacker = p
          , serialGenericUnpacker = u
          , serialSources = srcs
          }
        e -> error ("Expected exactly one generic packer/unpacker: " ++ show xs)

    getOut :: MType -> MType
    getOut (MFuncType _ _ x) = x
    getOut _ = error "Expected packer to have signature: a -> JSON"

    getIn :: MType -> MType
    getIn (MFuncType _ [x] _) = x
    getIn _ = error "Expected unpacker to have signature: JSON -> a"

    lookupOrDie :: (Ord a, Show a) => a -> Map.Map a b -> b
    lookupOrDie k h = case Map.lookup k h of
      (Just x) -> x
      Nothing -> error ("Could not find SerialMap for key: "
                        ++ show k ++ " for " ++ DT.unpack lang)

sparqlQuery :: Doc -> SparqlEndPoint -> IO [[Maybe Text]]
sparqlQuery lang = [sparql|
PREFIX mlc: <http://www.morloc.io/ontology/000/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX mid: <http://www.morloc.io/XXX/mid/>
SELECT DISTINCT ?type_id ?property ?is_generic ?name ?path
WHERE {
    # Get serialization functions of type `a -> JSON`
    ?id rdf:type mlc:typeDeclaration ;
          mlc:lang ${lang} ;
          mlc:lhs ?name ;
          mlc:rhs ?rhs .
    ?rhs rdf:type mlc:functionType ;
                 mlc:property ?property_id ;
                 mlc:output ?output .
    ?property_id rdf:type mlc:name .
    BIND(?rhs as ?type_id)
    {
        ?property_id rdf:value ?property .
        ?output rdf:type mlc:atomicType ;
                rdf:value "JSON" .
        ?rhs rdf:_0 ?packer_input .
        BIND(exists{?packer_input rdf:type mlc:atomicGeneric} AS ?is_generic)
        FILTER(?property = "packs")
    } UNION {
        ?property_id rdf:value ?property .
        ?rhs rdf:_0 ?unpacker_input ;
             mlc:output ?unpacker_output .
        ?unpacker_input rdf:type mlc:atomicType ;
                        rdf:value "JSON" .
        BIND(exists{?unpacker_output rdf:type mlc:atomicGeneric} AS ?is_generic)
        FILTER(?property = "unpacks")
    }
    OPTIONAL{
       ?source_id rdf:type mlc:source ;
                  mlc:lang ${lang} ;
                  mlc:path ?path ;
                  mlc:import ?import_id .
       ?import_id mlc:alias ?name .
    }
}
ORDER BY ?type_id ?property
|]
