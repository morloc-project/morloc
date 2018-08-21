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

forNexusCall :: SparqlEndPoint -> IO [(DT.Text, DT.Text, DT.Text, DT.Text)]
forNexusCall e = fmap (map four . values') (selectQuery' e sparql) where
  sparql = [r|
    PREFIX mlc: <http://www.morloc.io/ontology/000/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

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
