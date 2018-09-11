{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Component.MData
Description : Build manifolds for code generation from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.MData (fromSparqlDb) where

import qualified Database.HSparql.Connection as Conn
import Database.HSparql.QueryGenerator
import qualified Data.RDF as DR

import Morloc.Types
import Morloc.Operators hiding ((.:.))
import Morloc.Quasi
import qualified Morloc.Triple as M3
import qualified Morloc.Component.Util as MCU

import Morloc.Builder hiding ((<$>), (<>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as DT

fromSparqlDb :: SparqlEndPoint -> IO (Map.Map Key MData)
fromSparqlDb = MCU.simpleGraph toMData getParentData id (MCU.sendQuery hsparql)

getParentData :: [Maybe DT.Text] -> (DT.Text, Maybe DT.Text) 
getParentData [Just t, v] = (t, v)
getParentData _ = error "Unexpected SPARQL result"

toMData :: Map.Map Key ((DT.Text, Maybe DT.Text), [Key]) -> Key -> MData
toMData h k = toMData' (Map.lookup k h) where
  toMData' :: (Maybe ((DT.Text, Maybe DT.Text), [Key])) -> MData
  -- primitive "leaf" data
  toMData' (Just ((mtype, Just x), _))
    | mtype == M3.mlcPre <> "number"  = Num' x
    | mtype == M3.mlcPre <> "string"  = Str' x
    | mtype == M3.mlcPre <> "boolean" = Log' (x == "true")
    | otherwise = error "Unexpected type ..."
  -- containers "node" data
  toMData' (Just ((mtype, _), xs))
    | mtype == M3.mlcPre <> "list"   = Lst' (map (toMData h) xs)
    | mtype == M3.mlcPre <> "tuple"  = Tup' (map (toMData h) xs)
    | mtype == M3.mlcPre <> "record" = error "Records not yet supported"
    | otherwise = error "Unexpected type ..."
  -- shit happens
  toMData' _ = error "Unexpected type"

instance MShow MData where
  mshow (Num' x  ) = text' x
  mshow (Str' x  ) = text' x
  mshow (Log' x  ) = text' $ DT.pack (show x)
  mshow (Lst' xs ) = list (map mshow xs)
  mshow (Tup' xs ) = tupled (map mshow xs)
  mshow (Rec' xs ) = braces $ (vsep . punctuate ", ")
                              (map (\(k, v) -> text' k <> "=" <> mshow v) xs)

hsparql :: Query SelectQuery
hsparql= do
  mlc <- prefix "mlc" (iriRef "http://www.morloc.io/ontology/000/")
  rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  mid <- prefix "mid" (iriRef "http://www.morloc.io/XXX/mid/")

  id_      <- var
  element_ <- var
  child_   <- var
  type_    <- var
  value_   <- var

  triple_ id_ (rdf .:. "type") (mlc .:. "data")
  triple_ id_ (rdf .:. "type") type_
  filterExpr (type_ .!=. (mlc .:. "data"))

  optional_ $ triple_ id_ (rdf .:. "value") value_
  
  optional_ $ do
      triple_ id_ element_ child_
      MCU.isElement_ element_

  selectVars [id_, element_, child_, type_, value_]
