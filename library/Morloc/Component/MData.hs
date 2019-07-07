{-|
Module      : Morloc.Component.MData
Description : Build MData objects for code generation from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.MData
(
    fromSparqlDb
  , hsparql
) where

import Morloc.Sparql
import Morloc.Global
import Morloc.Operators
import qualified Morloc.Data.Text as MT
import qualified Morloc.Data.RDF as MR
import qualified Morloc.Component.Util as MCU
import qualified Morloc.Monad as MM

import Morloc.Data.Doc hiding ((<$>), (<>))
import qualified Data.Map.Strict as Map

fromSparqlDb
  :: SparqlDatabaseLike db
  => db -> MorlocMonad (Map.Map URI MData)
fromSparqlDb db
  = MCU.simpleGraph toMData getParentData URI (sparqlSelect "mdata" hsparql) db
  >>= MM.logFileWith "mdata.txt" Map.assocs

getParentData :: [[Maybe MT.Text]] -> MorlocMonad (MT.Text, Maybe MT.Text) 
getParentData [[Just t, v]] = return (t, v)
getParentData _ = MM.throwError . SparqlFail $ "Unexpected SPARQL result"

toMData :: Map.Map URI ((MT.Text, Maybe MT.Text), [URI]) -> URI -> MorlocMonad MData
toMData h k = toMData' (Map.lookup k h) where
  toMData' :: (Maybe ((MT.Text, Maybe MT.Text), [URI])) -> MorlocMonad MData
  -- primitive "leaf" data
  toMData' (Just ((mtype, Just x), _))
    | mtype == MR.mlcPre <> "number"  = return $ Num' x
    | mtype == MR.mlcPre <> "string"  = return $ Str' x
    | mtype == MR.mlcPre <> "boolean" = return $ Log' (x == "true")
    | otherwise = MM.throwError . InvalidRDF $ "Unexpected type ..."
  -- containers "node" data
  toMData' (Just ((mtype, _), xs))
    | mtype == MR.mlcPre <> "list"   = Lst' <$> mapM (toMData h) xs
    | mtype == MR.mlcPre <> "tuple"  = Tup' <$> mapM (toMData h) xs
    | mtype == MR.mlcPre <> "record" = MM.throwError $ NotImplemented "Records not yet supported"
    | otherwise = MM.throwError . InvalidRDF $ "Unexpected type ..."
  -- shit happens
  toMData' _ = MM.throwError . InvalidRDF $ "Unexpected type"

instance MShow MData where
  mshow (Num' x  ) = text' x
  mshow (Str' x  ) = text' x
  mshow (Log' x  ) = text' $ MT.pack (show x)
  mshow (Lst' xs ) = list (map mshow xs)
  mshow (Tup' xs ) = tupled (map mshow xs)
  mshow (Rec' xs ) = braces $ (vsep . punctuate ", ")
                              (map (\(k, v) -> text' k <> "=" <> mshow v) xs)

instance MorlocTypeable MData where
  asType (Num' _) = return $ MConcType emptyMeta "Number" []
  asType (Str' _) = return $ MConcType emptyMeta "String" []
  asType (Log' _) = return $ MConcType emptyMeta "Bool" []
  asType (Tup' xs) = MConcType emptyMeta "Tuple" <$> mapM asType xs
  -- TODO: make `Record` type
  asType (Rec' xs) = MConcType emptyMeta "Tuple" <$> mapM record xs where
    record (_, value) = do
      value' <- asType value
      return $ MConcType emptyMeta "Tuple" [ MConcType emptyMeta "String" [], value' ]
  -- FIXME: How should empty lists be typed? Should this raise an error?
  asType (Lst' []) = return $ MConcType emptyMeta "*" [] -- cannot determine type
  asType (Lst' [x]) = MConcType emptyMeta "List" . return <$> asType x 
  asType (Lst' (x:xs)) = do
    x' <- asType x 
    xs' <- mapM asType xs
    if
      all ((==) x') xs'
    then
      return $ MConcType emptyMeta "List" [x']
    else
      MM.throwError . TypeError $ "Lists must be homogenous"

emptyMeta = MTypeMeta {
      metaName = Nothing
    , metaProp = []
    , metaLang = Nothing
  }

hsparql :: Query SelectQuery
hsparql= do
  id_          <- var
  m_           <- var
  child_order_ <- var
  child_       <- var
  type_        <- var
  value_       <- var

  triple_ id_ PType OData
  triple_ id_ PType type_
  filterExpr (type_ .!=. OData)

  optional_ $ triple_ id_ PValue value_

  optional_ $ do
      triple_ id_ PElem m_
      triple_ m_ PValue child_
      triple_ m_ PPosition child_order_

  orderNextAsc id_ 
  orderNextAsc child_order_ 

  selectVars [id_, child_order_, child_, type_, value_]

----- For example, with sample.loc ++ 'foo = [4,3,6]'
-- ----------------------------------------------------------------
-- | id              | order | child           | type     | value |
-- ================================================================
-- | <sample.loc_30> | 0     | <sample.loc_31> | <list>   |       |
-- | <sample.loc_30> | 1     | <sample.loc_32> | <list>   |       |
-- | <sample.loc_30> | 2     | <sample.loc_33> | <list>   |       |
-- | <sample.loc_17> |       |                 | <number> | 0.0   |
-- | <sample.loc_31> |       |                 | <number> | 4.0   |
-- | <sample.loc_32> |       |                 | <number> | 3.0   |
-- | <sample.loc_33> |       |                 | <number> | 6.0   |
-- | <sample.loc_50> |       |                 | <number> | 0.0   |
-- | <sample.loc_51> |       |                 | <number> | 1.0   |
-- ----------------------------------------------------------------
