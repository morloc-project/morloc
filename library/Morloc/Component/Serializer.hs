{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Component.Serializer
Description : Build JSON packers and unpackers from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.Serializer (
    fromSparqlDb
  , SerialMap(..)
) where

import Morloc.Types
import Morloc.Operators
import Morloc.Sparql
import qualified Morloc.Error as ME
import qualified Morloc.Util as MU
import qualified Morloc.Component.MType as MCM 
import qualified Morloc.Data.Text as MT

import qualified Data.Map.Strict as Map

type SerialData =
  ( Key  -- type_id - 
  , Name -- property - "packs" or "unpackes"
  , Bool -- is_generic - is this a generic packer/unpacker
  , Name -- name
  , Path -- path
  )

-- TODO: update this to limit results to one language
-- OR return a hash of hashes by language
fromSparqlDb
  :: SparqlDatabaseLike db
  => Lang -> db -> IO SerialMap
fromSparqlDb l db
  =   toSerialMap
  <$> MCM.fromSparqlDb db
  <*> (map tuplify <$> sparqlSelect (hsparql l) db)

  where

    tuplify :: [Maybe MT.Text] -> SerialData
    -- typename | property | is_generic | name | path
    tuplify [Just t, Just p, Just g, Just n, Just s] = (t,p,g == "true",n,s)
    tuplify e = ME.error' ("Unexpected SPARQL result: " <> MT.pretty e)

    toSerialMap
      :: Map.Map Key ConcreteType 
      -> [SerialData]
      -> SerialMap
    toSerialMap h xs = case
      ( Map.fromList [(getIn  (lookupOrDie t h), p) | (t, "packs"  , False, p, _) <- xs]
      , Map.fromList [(getOut (lookupOrDie t h), p) | (t, "unpacks", False, p, _) <- xs]
      , [p | (_, "packs"  , True, p, _) <- xs]
      , [p | (_, "unpacks", True, p, _) <- xs]
      , MU.unique [s | (_, _, _, _, s) <- xs]
      ) of
        (phash, uhash, [p], [u], srcs) -> SerialMap
          { serialLang = l
          , serialPacker = phash
          , serialUnpacker = uhash
          , serialGenericPacker = p
          , serialGenericUnpacker = u
          , serialSources = srcs
          }
        _ -> ME.error' ("Expected exactly one generic packer/unpacker: " <> MT.pretty xs)

    getOut :: MType -> MType
    getOut (MFuncType _ _ x) = x
    getOut t = ME.error' ("Expected packer to have signature: a -> JSON: " <> MT.pretty t)

    getIn :: MType -> MType
    getIn (MFuncType _ [x] _) = x
    getIn t = ME.error' ("Expected unpacker to have signature: JSON -> a: " <> MT.pretty t)

    lookupOrDie :: (Ord a, Show a) => a -> Map.Map a b -> b
    lookupOrDie k h = case Map.lookup k h of
      (Just x) -> x
      Nothing -> ME.error' (
          "Could not find SerialMap for key: " <> MT.pretty k <> " for " <> l
        )

hsparql :: Lang -> Query SelectQuery
hsparql lang' = do
  basetype_      <- var
  id_            <- var
  importId_      <- var
  isGeneric_     <- var
  name_          <- var
  output_        <- var
  packerInput_   <- var
  path_          <- var
  propertyId_    <- var
  property_      <- var
  rhs_           <- var
  sourceId_      <- var
  typeId_        <- var
  unpackerInput_ <- var

  -- Get serialization functions of type `a -> JSON`
  triple_ id_ PType  OTypeDeclaration
  triple_ id_ PLang  lang'
  triple_ id_ PLeft  name_
  triple_ id_ PRight rhs_

  triple_ rhs_ PType OFunctionType
  triple_ rhs_ PProperty propertyId_
  triple_ rhs_ POutput output_

  triple_ propertyId_ PType OName

  union_
    ( do
        filterExpr (property_ .==. ("packs" :: MT.Text))
        triple_ propertyId_ PValue property_
        triple_ output_ PType OAtomicType
        triple_ output_ PValue ("JSON" :: MT.Text)
        triple_ rhs_ (PElem 0) packerInput_
        triple_ packerInput_ PType basetype_
        filterExpr (basetype_ .!=. OType)
    )
    ( do
        filterExpr (property_ .==. ("unpacks" :: MT.Text))
        triple_ propertyId_ PValue property_
        triple_ rhs_ (PElem 0) unpackerInput_
        triple_ unpackerInput_ PType OAtomicType
        triple_ unpackerInput_ PValue ("JSON" :: MT.Text)
        triple_ output_ PType basetype_
        filterExpr (basetype_ .!=. OType)
    )

  bind (basetype_ .==. OAtomicGenericType) isGeneric_

  optional_
    ( do
        triple_ sourceId_ PType OSource
        triple_ sourceId_ PLang lang'
        triple_ sourceId_ PPath path_
        triple_ sourceId_ PImport importId_
        triple_ importId_ PAlias name_
    )

  orderNextAsc typeId_
  orderNextAsc property_

  selectVars [rhs_, property_, isGeneric_, name_, path_]
