{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

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
import qualified Morloc.Data.RDF as MR

import Database.HSparql.QueryGenerator
import Database.HSparql.Connection
import Morloc.Database.HSparql.Connection
import Morloc.Database.HSparql.Upload
import qualified Morloc.Data.Text as MT

instance TermLike GraphPredicate where
  varOrTerm x = varOrTerm (asRdfNode x)

instance TermLike GraphObject where
  varOrTerm x = varOrTerm (asRdfNode x)

instance PredicateTermLike GraphPredicate
instance ObjectTermLike GraphObject

instance SparqlSelectLike (Query SelectQuery) where
  writeSparql p x = writeFile (MT.unpack p) (createSelectQuery x) 
  showSparql = createSelectQuery

instance SparqlDatabaseLike SparqlEndPoint where
  -- sparqlUpload :: (RdfLike r) => a -> r -> IO a
  sparqlUpload ep r
    = fmap (const ep) (uploadTriples (endpoint ep) (asTriples r))

  -- sparqlSelect
  --   :: (SparqlSelectLike q)
  --   => q -> a -> IO (Either Text [[Maybe Text]])
  sparqlSelect q ep
    = fmap values (selectQuery' (endpoint ep) (showSparql q))

values :: Maybe [[BindingValue]] -> [[Maybe MT.Text]]
values Nothing = error "SPARQL command failed"
values (Just xss) = (fmap . fmap) maybeValue xss

maybeValue :: BindingValue -> Maybe MT.Text
maybeValue (Bound (MR.LNode (MR.PlainL  x  ))) = Just x
maybeValue (Bound (MR.LNode (MR.PlainLL x _))) = Just x
maybeValue (Bound (MR.LNode (MR.TypedL  x _))) = Just x
maybeValue (Bound (MR.UNode x))             = Just x
maybeValue _ = Nothing
