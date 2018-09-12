{-|
Module      : Morloc.Sparql
Description : Handles SPARQL queries and connections
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Sparql
  ( 
      module Database.HSparql.QueryGenerator
    , module Morloc.Database.HSparql.Connection
    , module Database.HSparql.Connection
  ) where

import Morloc.Types
import Morloc.RDF () -- just import instances

import Database.HSparql.QueryGenerator
import Database.HSparql.Connection
import Morloc.Database.HSparql.Connection

instance TermLike GraphPredicate where
  varOrTerm x = varOrTerm (asRdfNode x)

instance TermLike GraphObject where
  varOrTerm x = varOrTerm (asRdfNode x)

instance PredicateTermLike GraphPredicate
instance ObjectTermLike GraphObject
