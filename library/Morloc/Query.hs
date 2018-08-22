{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

{-|
Module      : Morloc.Query
Description : SPARQL queries for checking and building code
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Query (
    exports
  , forNexusCall
  , sources
  , packers
  , arg2type
) where

import Text.RawString.QQ
import Data.RDF hiding (Query)
import qualified Data.Text as DT
import qualified Data.Maybe as DM

import Morloc.Database.HSparql.Connection

maybeValue :: BindingValue -> Maybe DT.Text
maybeValue (Bound (LNode (PlainL  s  ))) = Just s
maybeValue (Bound (LNode (PlainLL s _))) = Just s
maybeValue (Bound (LNode (TypedL  s _))) = Just s
maybeValue (Bound (UNode s))             = Just s
maybeValue _ = Nothing

values :: Maybe [[BindingValue]] -> [[Maybe DT.Text]]
values Nothing = []
values (Just xss) = (fmap . fmap) maybeValue xss

values' :: Maybe [[BindingValue]] -> [[DT.Text]]
values' = map DM.catMaybes . values 

four :: [a] -> (a,a,a,a)
four [a,b,c,d] = (a,b,c,d)
four _ = error "Expected 4 element list"

exports :: SparqlEndPoint -> IO [DT.Text]
exports e = fmap (concat . values') (selectQuery' e sparql) where
  sparql = [r|
    PREFIX mlc: <http://www.morloc.io/ontology/000/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?o
    WHERE {
      ?s rdf:type mlc:export ;
         rdf:value ?o
  |]

forNexusCall :: SparqlEndPoint -> IO [(DT.Text, DT.Text, DT.Text, Int)]
forNexusCall e = fmap (map toInt . map four . values') (selectQuery' e sparql) where
  sparql = [r|
    PREFIX mlc: <http://www.morloc.io/ontology/000/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?fname ?lang ?typedec (count (distinct ?element) as ?nargs)
    WHERE {
      ?typedec rdf:type mlc:typeDeclaration ;
         mlc:lang "Morloc" ;
         mlc:lhs ?fname ;
         mlc:rhs ?type .
      ?type rdf:type mlc:functionType .
      ?arg ?element ?type .
      FILTER(regex(str(?element), "_[0-9]+$", "i"))
      ?src rdf:type mlc:source ;
           mlc:lang ?lang ;
           mlc:import ?i .
      ?i mlc:alias ?fname .
      ?e rdf:type mlc:export ;
         rdf:value ?fname .
    }
    GROUP BY ?typedec ?fname ?lang
  |]

  toInt :: (a, a, a, DT.Text) -> (a, a, a, Int)
  toInt (a,b,c,i) = case (reads (DT.unpack i) :: [(Int, String)]) of
    [(i, "")] -> (a,b,c,i)
    _ -> error ("Failed to parse number of arguments. Exected integer, got: " ++ show i)

sources :: SparqlEndPoint -> IO [[Maybe DT.Text]]
sources e = fmap values (selectQuery' e sparql) where
  sparql = [r|
    PREFIX mlc: <http://www.morloc.io/ontology/000/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?lang ?fname ?alias ?path 
    WHERE {
      ?s rdf:type mlc:source ;
         mlc:lang ?lang ;
         mlc:import ?i .
      ?i mlc:name ?fname ;
         mlc:alias ?alias .
      OPTIONAL { ?s mlc:path ?path }
    }
  |]

-- | Find all packers
-- @
--   <function> <lang> :: packs => <from> -> JSON
-- @
packers :: SparqlEndPoint -> IO [[Maybe DT.Text]]
packers e = fmap values (selectQuery' e sparql) where
  sparql = [r|
    PREFIX mlc: <http://www.morloc.io/ontology/000/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT DISTINCT ?lang ?mlc_name ?function_name ?pack_type ?pack_value
    WHERE {
      # find language-specific type declarations
      ?type rdf:type mlc:typeDeclaration ;
            mlc:lang ?lang ;
            mlc:lhs ?mlc_name ;
            mlc:rhs ?type_rhs .
      # that have the property "packs"
      ?type_rhs rdf:type mlc:functionType ;
                mlc:property ?property .
      # properties may by compound (e.g. when used to specify type constraints)
      ?property rdf:type mlc:name ;
                rdf:value "packs" .
      ?input rdf:_0 ?type_rhs;
             rdf:type ?pack_type .
      ?output rdf:type mlc:atomicType ;
              rdf:value "JSON" .
      OPTIONAL {
        ?input rdf:value ?pack_value
      }
      # If the function was sourced, then the morloc name may be an alias for the
      # real name. So here find the real name.
      OPTIONAL
      {
        ?source rdf:type mlc:source ;
                mlc:lang ?lang ;
                mlc:import ?import .
        ?import mlc:name  ?function_name ;
                mlc:alias ?mlc_name .
      }
    }
  |]

-- | Map the arguments in a call to their types
arg2type :: SparqlEndPoint -> IO [[Maybe DT.Text]]
arg2type e = fmap values (selectQuery' e sparql) where
  sparql = [r|
    PREFIX mlc: <http://www.morloc.io/ontology/000/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?fname ?element ?argvalue ?argtype ?argform ?argname
    WHERE {
        ?call rdf:type mlc:call ;
           mlc:value ?value .  
        ?value rdf:type ?value_type .
        ?value rdf:value ?fname .
        ?argvalue ?element ?call .
        ?typedec rdf:type mlc:typeDeclaration ;
                 mlc:lang "Morloc" ;
                 mlc:lhs ?fname ;
                 mlc:rhs ?ftype .
        ?ftype rdf:type mlc:functionType .
        ?argtype ?element ?ftype .
        OPTIONAL { ?argtype rdf:type ?argform . }
        OPTIONAL { ?argtype rdf:value ?argname . }
        FILTER(regex(str(?element), "_[0-9]+$", "i"))
    }
  |]
