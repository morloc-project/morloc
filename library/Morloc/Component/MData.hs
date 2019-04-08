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

import Morloc.Sparql
import Morloc.Types
import Morloc.Operators
import qualified Morloc.Data.Text as MT
import qualified Morloc.Data.RDF as MR
import qualified Morloc.Component.Util as MCU
import qualified Morloc.Monad as MM

import Morloc.Data.Doc hiding ((<$>), (<>))
import qualified Data.Map.Strict as Map

fromSparqlDb
  :: SparqlDatabaseLike db
  => db -> MorlocMonad (Map.Map Key MData)
fromSparqlDb = MCU.simpleGraph toMData getParentData id (sparqlSelect hsparql)

getParentData :: [Maybe MT.Text] -> MorlocMonad (MT.Text, Maybe MT.Text) 
getParentData [Just t, v] = return $ (t, v)
getParentData _ = MM.throwError $ SparqlFail "Unexpected SPARQL result"

toMData :: Map.Map Key ((MT.Text, Maybe MT.Text), [Key]) -> Key -> MData
toMData h k = toMData' (Map.lookup k h) where
  toMData' :: (Maybe ((MT.Text, Maybe MT.Text), [Key])) -> MData
  -- primitive "leaf" data
  toMData' (Just ((mtype, Just x), _))
    | mtype == MR.mlcPre <> "number"  = Num' x
    | mtype == MR.mlcPre <> "string"  = Str' x
    | mtype == MR.mlcPre <> "boolean" = Log' (x == "true")
    | otherwise = error "Unexpected type ..."
  -- containers "node" data
  toMData' (Just ((mtype, _), xs))
    | mtype == MR.mlcPre <> "list"   = Lst' (map (toMData h) xs)
    | mtype == MR.mlcPre <> "tuple"  = Tup' (map (toMData h) xs)
    | mtype == MR.mlcPre <> "record" = error "Records not yet supported"
    | otherwise = error "Unexpected type ..."
  -- shit happens
  toMData' _ = error "Unexpected type"

instance MShow MData where
  mshow (Num' x  ) = text' x
  mshow (Str' x  ) = text' x
  mshow (Log' x  ) = text' $ MT.pack (show x)
  mshow (Lst' xs ) = list (map mshow xs)
  mshow (Tup' xs ) = tupled (map mshow xs)
  mshow (Rec' xs ) = braces $ (vsep . punctuate ", ")
                              (map (\(k, v) -> text' k <> "=" <> mshow v) xs)

instance MorlocTypeable MData where
  asType (Num' _) = MConcType emptyMeta "Number" []
  asType (Str' _) = MConcType emptyMeta "String" []
  asType (Log' _) = MConcType emptyMeta "Bool" []
  asType (Tup' xs) = MConcType emptyMeta "Tuple" (map asType xs)
  -- TODO: make `Record` type
  asType (Rec' xs) = MConcType emptyMeta "Tuple" (map record xs) where
    record (_, value) = MConcType emptyMeta "Tuple" [ MConcType emptyMeta "String" []
                                                      , asType value]
  asType (Lst' xs) = MConcType emptyMeta "List" [listType xs] where
    listType [] = MConcType emptyMeta "*" [] -- cannot determine type
    listType [x] = asType x
    listType (x:xs') =
      if
        all (\a -> asType a == asType x) xs'
      then
        asType x
      else
      error "Lists must be homogenous"

emptyMeta = MTypeMeta {
      metaName = Nothing
    , metaProp = []
    , metaLang = Nothing
  }

hsparql :: Query SelectQuery
hsparql= do
  id_      <- var
  element_ <- var
  child_   <- var
  type_    <- var
  value_   <- var

  triple_ id_ PType OData
  triple_ id_ PType type_
  filterExpr (type_ .!=. OData)

  optional_ $ triple_ id_ PValue value_
  
  optional_ $ do
      triple_ id_ element_ child_
      MCU.isElement_ element_

  selectVars [id_, element_, child_, type_, value_]
