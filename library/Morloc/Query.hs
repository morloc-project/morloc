{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Query
Description : SPARQL queries for checking and building code
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Query (
  findallTypes
) where

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

import qualified Data.Text as DT

-- find the names of all types
findallTypes :: Query SelectQuery
findallTypes = do
  xsd <- prefix "xsd"    (iriRef "http://www.w3.org/2001/XMLSchema#")
  rdf <- prefix "rdf"    (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  mlc <- prefix "morloc" (iriRef "http://www.morloc.io/ontology/000")

  s        <- var
  typename <- var

  triple_ s (rdf .:. "type") (mlc .:. "typeDeclaration")
  triple_ s (mlc .:. "lang") (DT.pack "Morloc")
  triple_ s (mlc .:. "lhs") typename

  selectVars [typename]

{-

prefix xsd:    <http://www.w3.org/2001/XMLSchema#>
prefix rdf:    <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix morloc: <http://www.morloc.io/ontology/000>

SELECT typename
WHERE {
  s rdf:type mlc:typeDeclaration
  s mlc:lang "Morloc"
  s mlc:lhs typename
}

-}
