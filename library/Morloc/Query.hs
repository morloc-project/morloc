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

import Morloc.Database.HSparql.Connection

valueOf :: BindingValue -> [DT.Text]
valueOf (Bound (LNode (PlainL  s  ))) = [s]
valueOf (Bound (LNode (PlainLL s _))) = [s]
valueOf (Bound (LNode (TypedL  s _))) = [s]
valueOf _ = []

-- remove all the unsafe operation
asTextList :: Maybe [[BindingValue]] -> [DT.Text]
asTextList (Just []) = [] 
asTextList Nothing   = []
asTextList (Just xs) = concat $ map extractOne xs where
  extractOne :: [BindingValue] -> [DT.Text]
  extractOne [x] = valueOf x 
  extractOne _ = error "Expected just one"
asTextList _ = error "Bad RDF or SPARQL query: expected list of LNode"

exports :: SparqlEndPoint -> IO [DT.Text]
exports e = fmap asTextList (selectQuery' e sparql) where
  sparql = [r|
    PREFIX mlc: <http://www.morloc.io/ontology/000/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?s ?o
    WHERE {
      ?s rdf:type mlc:export ;
         rdf:value ?o
  |]

forNexusCall :: SparqlEndPoint -> IO [(DT.Text, DT.Text, DT.Text, Int)]
forNexusCall e = fmap parse' (selectQuery' e sparql) where
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

  parse' :: Maybe [[BindingValue]] -> [(DT.Text, DT.Text, DT.Text, Int)]
  parse' Nothing = []
  parse' (Just xs) = concat . map parse'' $ xs

  parse'' :: [BindingValue] -> [(DT.Text, DT.Text, DT.Text, Int)]
  parse'' [ Bound (LNode (PlainL fname))
          , Bound (LNode (PlainL lang))
          , Bound (UNode typedec)
          , Bound (LNode (TypedL nargs _))
          ] = [(fname, lang, typedec, (read (DT.unpack nargs) :: Int))]
  parse'' [] = []
  parse'' _ = error "Unexpected result"
