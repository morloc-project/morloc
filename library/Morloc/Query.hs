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
    exports
  , exportsQ
) where

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import Data.RDF hiding (Query)

import qualified Data.Text as DT

valueOf :: BindingValue -> [DT.Text]
valueOf (Bound (LNode (PlainL  s  ))) = [s]
valueOf (Bound (LNode (PlainLL s _))) = [s]
valueOf (Bound (LNode (TypedL  s _))) = [s]
valueOf _ = []

-- remove all the unsafe operation
extractListText :: Maybe [[BindingValue]] -> [DT.Text]
extractListText (Just xs) = concat $ map extractOne xs where
  extractOne :: [BindingValue] -> [DT.Text]
  extractOne [x] = valueOf x 
  extractOne _ = error "Expected just one"
extractListText (Just []) = [] 
extractListText Nothing   = []
extractListText _ = error "Bad RDF or SPARQL query: expected list of LNode"

exports :: EndPoint -> IO [DT.Text]
exports e = fmap extractListText (selectQuery e exportsQ)

exportsQ :: Query SelectQuery
exportsQ = do
  xsd <- prefix "xsd" (iriRef "http://www.w3.org/2001/XMLSchema#")
  rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  mlc <- prefix "mlc" (iriRef "http://www.morloc.io/ontology/000/")

  s <- var
  o <- var

  triple_ s (rdf .:. "type") (mlc .:. "export")
  triple_ s (rdf .:. "value") o

  selectVars [o]
