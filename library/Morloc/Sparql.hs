{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-|
Module      : Morloc.Sparql
Description : Handles SPARQL queries and connections
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

The SPARQL queries are the weakest spot in the codebase.
-}

module Morloc.Sparql ( 
    module Database.HSparql.QueryGenerator
  , module Database.HSparql.Connection
) where

import Morloc.Namespace
import qualified Morloc.Data.RDF as MR
import qualified Morloc.Data.Text as MT
import qualified Morloc.Data.Doc as G
import qualified Morloc.Monad as MM

import Database.HSparql.QueryGenerator
import Database.HSparql.Connection

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
  -- sparqlUpload :: (RdfLike r) => a -> r -> MorlocMonad a
  sparqlUpload ep r
    = MM.liftIO $ fmap (const ep) (uploadTriples (endpoint ep) (asTriples r))

  -- sparqlSelect
  --   :: (SparqlSelectLike q)
  --   => Text -> q -> a -> MorlocMonad (Either Text [[Maybe Text]])
  sparqlSelect _ q ep
    = (MM.liftIO $ selectQueryRaw (endpoint ep) (showSparql q)) >>= values

values :: Maybe [[BindingValue]] -> MorlocMonad [[Maybe MT.Text]]
values Nothing = MM.throwError . SparqlFail $ "SPARQL command failed: "
values (Just xss) = return $ (fmap . fmap) maybeValue xss

maybeValue :: BindingValue -> Maybe MT.Text
maybeValue (Bound (MR.LNode (MR.PlainL  x  ))) = Just x
maybeValue (Bound (MR.LNode (MR.PlainLL x _))) = Just x
maybeValue (Bound (MR.LNode (MR.TypedL  x _))) = Just x
maybeValue (Bound (MR.UNode x))             = Just x
maybeValue _ = Nothing

uploadTriples :: String -> [MR.Triple] -> IO Bool
uploadTriples ep xs = updateQueryRaw ep (G.render' . makeSparql $ xs) where

makeSparql :: [MR.Triple] -> MDoc
makeSparql xs =  "INSERT DATA" <> G.line
              <> G.braces (G.indent 4 $ G.pretty xs)
