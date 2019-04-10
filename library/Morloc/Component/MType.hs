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

import Morloc.Sparql
import Morloc.Types
import Morloc.Operators
import Morloc.Data.Doc hiding ((<$>),(<>))
import qualified Morloc.Component.Util as MCU
import qualified Morloc.Data.Text as MT
import qualified Morloc.Data.RDF as MR
import qualified Morloc.Monad as MM

import qualified Data.Map.Strict as Map
import qualified Data.Foldable as DF

type ParentData =
  ( MT.Text       -- type (e.g. mlc:functionType or mlc:atomicGeneric)
  , Maybe MT.Text -- top-level name of the type (e.g. "List" or "Int")
  , Maybe Key     -- type id of the output if this is a function
  , Maybe Lang    -- type language ("Morloc" for a general type)
  , Maybe Name    -- typename from a typeDeclaration statement
  , [Name]        -- list of properties (e.g. "packs")
  )

instance MShow MType where
  mshow (MConcType _ n []) = text' n
  mshow (MConcType _ n ts) = parens $ hsep (text' n:(map mshow ts))
  mshow (MAbstType _ n []) = text' n
  mshow (MAbstType _ n ts) = parens $ hsep (text' n:(map mshow ts))
  mshow (MFuncType _ ts o) = parens $
    (hcat . punctuate ", ") (map mshow ts) <> " -> " <> mshow o

fromSparqlDb :: (SparqlDatabaseLike db) => db -> MorlocMonad (Map.Map Key MType)
fromSparqlDb = MCU.simpleGraph toMType getParentData id (sparqlSelect "mtype" hsparql)

getParentData :: [Maybe MT.Text] -> MorlocMonad ParentData 
getParentData [Just t, v, o, l, n, ps] = return $ (t, v, o, l, n, properties) where
  properties = DF.concat . fmap (MT.splitOn ",") $ ps
getParentData x = MM.throwError . SparqlFail $ "Unexpected SPARQL result: " <> MT.show' x

toMType :: Map.Map Key (ParentData, [Key]) -> Key -> MorlocMonad MType
toMType h k = toMType' (Map.lookup k h) where
  toMType' (Just ((t, v, o, l, n, ps), xs)) =
    case makeMeta l n ps of
      meta -> toMType'' meta t v o xs
  toMType' Nothing = MM.throwError TrulyWeird

  toMType'' meta t (Just v) _ xs
    | isGeneric t = MAbstType meta v <$> mapM (toMType h) xs
    | otherwise   = MConcType meta v <$> mapM (toMType h) xs
  toMType'' meta _ _ (Just o) xs = do
    inputTypes <- mapM (toMType h) xs
    outputType <- toMType h o
    return $ MFuncType meta inputTypes outputType
  toMType'' _ _ _ _ _ = MM.throwError TrulyWeird

  makeMeta :: Maybe Lang -> Maybe Name -> [Name] -> MTypeMeta
  makeMeta l n ps = MTypeMeta {
        metaName = n
      , metaProp = ps
      , metaLang = l
    }

isGeneric :: MT.Text -> Bool
isGeneric x = MR.UNode x == asRdfNode OAtomicGenericType ||
              MR.UNode x == asRdfNode OParameterizedGenericType

hsparql :: Query SelectQuery
hsparql = do
  id_         <- var
  element_    <- var
  child_      <- var
  type_       <- var
  value_      <- var
  output_     <- var
  lang_       <- var
  typename_   <- var
  property_   <- var
  properties_ <- var

  triple_ id_ PType OType
  triple_ id_ PType type_
  filterExpr (type_ .!=. OType)

  optional_ $ triple_ id_ PValue value_
  
  optional_ $ do
    triple_ id_ element_ child_
    MCU.isElement_ element_

  optional_ $ triple_ id_ POutput output_
  optional_ $ do
    typedec_ <- var 
    triple_ typedec_ PType OTypeDeclaration
    triple_ typedec_ PLang lang_
    triple_ typedec_ PLeft  typename_
    triple_ typedec_ PRight  id_

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
