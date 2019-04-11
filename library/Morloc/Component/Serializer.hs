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
import qualified Morloc.Monad as MM

import qualified Data.Map.Strict as Map

type SerialData =
  ( Key  -- type_id - 
  , Name -- property - "packs" or "unpackes"
  , Bool -- is_generic - is this a generic packer/unpacker
  , Name -- name
  , Path -- path
  , Path -- module path
  )

-- TODO: update this to limit results to one language
-- OR return a hash of hashes by language
fromSparqlDb
  :: SparqlDatabaseLike db
  => Lang -> db -> MorlocMonad SerialMap
fromSparqlDb l db = do
  typemap <- MCM.fromSparqlDb db
  serialData <- sparqlSelect "serializer" (hsparql l) db >>= mapM tuplify
  toSerialMap typemap serialData
  where
    tuplify :: [Maybe MT.Text] -> MorlocMonad SerialData
    tuplify [ Just t -- typename
            , Just p -- property
            , Just g -- is_generic
            , Just n -- name
            , Just s -- path (e.g., "rbase.R")
            , Just m -- module path (e.g., "$HOME/.morloc/lib/rbase/main.loc")
            ] = return (t,p,g == "true",n,s,m)
    tuplify e = MM.throwError . SparqlFail $ "Unexpected SPARQL result: " <> MT.pretty e

    toSerialMap
      :: Map.Map Key ConcreteType 
      -> [SerialData]
      -> MorlocMonad SerialMap
    toSerialMap h xs = do
      packers   <- sequence [lookupOrDie t h >>= getIn  p | (t, "packs"  , False, p, _, _) <- xs]
      unpackers <- sequence [lookupOrDie t h >>= getOut p | (t, "unpacks", False, p, _, _) <- xs]
      case ( Map.fromList packers
           , Map.fromList unpackers
           , [p | (_, "packs"  , True, p, _, _) <- xs]
           , [p | (_, "unpacks", True, p, _, _) <- xs]
           , MU.unique [makePath m s | (_, _, _, _, s, m) <- xs]
           ) of
        (phash, uhash, [p], [u], srcs) -> return $ SerialMap
          { serialLang = l
          , serialPacker = phash
          , serialUnpacker = uhash
          , serialGenericPacker = p
          , serialGenericUnpacker = u
          , serialSources = srcs
          }
        _ -> MM.throwError . TypeError $ "Expected exactly one generic packer/unpacker: " <> MT.pretty xs

    makePath
      :: MT.Text -- module path
      -> MT.Text -- basename of packer/unpacker source file
      -> MT.Text
    makePath m' p' = MT.intercalate "/" $ (init (MT.splitOn "/" m')) ++ [p']

    getOut :: a -> MType -> MorlocMonad (MType, a)
    getOut p (MFuncType _ _ x) = return (x, p)
    getOut _ t = MM.throwError . SerializationError $
      "Expected packer to have signature: a -> JSON: " <> MT.pretty t

    getIn :: a -> MType -> MorlocMonad (MType, a)
    getIn p (MFuncType _ [x] _) = return (x, p)
    getIn _ t = MM.throwError . SerializationError $
      "Expected unpacker to have signature: JSON -> a: " <> MT.pretty t

    lookupOrDie :: (Ord a, Show a) => a -> Map.Map a b -> MorlocMonad b
    lookupOrDie k h = case Map.lookup k h of
      (Just x) -> return x
      Nothing -> MM.throwError . SerializationError $
        "Could not find SerialMap for key: " <> MT.pretty k <> " for " <> l

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
  scriptId_      <- var
  element_       <- var
  modulePath_    <- var

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

        triple_ scriptId_ PType OScript
        triple_ scriptId_ element_ sourceId_ 
        triple_ scriptId_ PValue modulePath_
    )

  orderNextAsc typeId_
  orderNextAsc property_

  selectVars [rhs_, property_, isGeneric_, name_, path_, modulePath_]
