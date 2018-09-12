{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Component.MType
Description : Build manifolds for code generation from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.MType (fromSparqlDb) where

import qualified Database.HSparql.Connection as Conn
import Database.HSparql.QueryGenerator
import qualified Data.RDF as DR

import Morloc.Types
import Morloc.Operators hiding ((.:.))
import qualified Morloc.Triple as M3
import qualified Morloc.Component.Util as MCU

import Morloc.Builder hiding ((<$>),(<>))
import Morloc.Database.HSparql.Connection
import qualified Data.Map.Strict as Map
import qualified Data.List.Extra as DLE
import qualified Data.Foldable as DF
import qualified Data.Text as DT

type ParentData =
  ( DT.Text       -- type (e.g. mlc:functionType or mlc:atomicGeneric)
  , Maybe DT.Text -- top-level name of the type (e.g. "List" or "Int")
  , Maybe Key     -- type id of the output if this is a function
  , Maybe Lang    -- type language ("Morloc" for a general type)
  , Maybe Name    -- typename from a typeDeclaration statement
  , [Name]        -- list of properties (e.g. "packs")
  )

instance MShow MType where
  mshow (MDataType _ n []) = text' n
  mshow (MDataType _ n ts) = parens $ hsep (text' n:(map mshow ts))
  mshow (MFuncType _ ts o) = parens $
    (hcat . punctuate ", ") (map mshow ts) <> " -> " <> mshow o

fromSparqlDb :: SparqlEndPoint -> IO (Map.Map Key MType)
fromSparqlDb = MCU.simpleGraph toMType getParentData id (MCU.sendQuery hsparql)

getParentData :: [Maybe DT.Text] -> ParentData 
getParentData [Just t, v, o, l, n, ps] = (t, v, o, l, n, properties) where
  properties = DF.concat . fmap (DT.splitOn ",") $ ps
getParentData x = error ("Unexpected SPARQL result: " ++ show x)

toMType :: Map.Map Key (ParentData, [Key]) -> Key -> MType
toMType h k = toMType' (Map.lookup k h) where
  toMType' (Just ((t, v, o, l, n, ps), xs)) = case makeMeta l n ps of
    meta -> toMType'' meta v o xs

  toMType'' meta (Just v) _ xs = MDataType meta v (map (toMType h) xs)
  toMType'' meta _ (Just o) xs = MFuncType meta (map (toMType h) xs) (toMType h o)

  makeMeta :: Maybe Lang -> Maybe Name -> [Name] -> MTypeMeta
  makeMeta l n ps = MTypeMeta {
        metaName = n
      , metaProp = ps
      , metaLang = l
    }

hsparql :: Query SelectQuery
hsparql = do
  mlc <- prefix "mlc" (iriRef "http://www.morloc.io/ontology/000/")
  rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  mid <- prefix "mid" (iriRef "http://www.morloc.io/XXX/mid/")

  id_ <- var
  element_ <- var
  child_ <- var
  type_ <- var
  value_ <- var
  output_ <- var
  lang_ <- var
  typename_ <- var
  property_ <- var
  properties_ <- var

  triple_ id_ (rdf .:. "type") (mlc .:. "type")
  triple_ id_ (rdf .:. "type") type_
  filterExpr (type_ .!=. (mlc .:. "type"))

  optional_ $ triple_ id_ (rdf .:. "value") value_
  
  optional_ $ do
    triple_ id_ element_ child_
    MCU.isElement_ element_

  optional_ $ triple_ id_ (mlc .:. "output") output_
  optional_ $ do
    typedec_ <- var 
    triple_ typedec_ (rdf .:. "type") (mlc .:. "typeDeclaration")
    triple_ typedec_ (mlc .:. "lang") lang_
    triple_ typedec_ (mlc .:. "lhs")  typename_
    triple_ typedec_ (mlc .:. "rhs")  id_

  groupBy id_
  groupBy element_
  groupBy child_
  groupBy type_
  groupBy value_
  groupBy output_
  groupBy lang_
  groupBy typename_

  orderNextAsc id_ 
  orderNextAsc element_ 

  select
    [ SelectVar id_
    , SelectVar element_
    , SelectVar child_
    , SelectVar type_
    , SelectVar value_
    , SelectVar output_
    , SelectVar lang_
    , SelectVar typename_
    , SelectExpr (groupConcat property_ ", ") properties_
    ]
