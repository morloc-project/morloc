{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Component.MType
Description : Build MType objects for code generation from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.MType
(
    fromSparqlDb
  , hsparql
) where

import Morloc.Sparql
import Morloc.Global
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

-- | Extracts data needed to build types
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
  -- NOTE: these CANNOT be defined in the `optional_` do block
  typedec_    <- var 
  propertyId_ <- var

  -- For all types, the expression `?i rdf:type ?t` will return two objects.
  -- The first is `mlc:type`, which is common to all types. The second can be
  -- mlc:atomicType (A), mlc:parameterizedType (P), or mlc:functionType (F).
  triple_ id_ PType OType -- match the common mlc:type
  triple_ id_ PType type_       -- \ matches the specialized type
  filterExpr (type_ .!=. OType) -- /

  -- Get the type name, defined when type_ is OAtomicType or OParameterizedType
  optional_ $ triple_ id_ PValue value_

  -- This will run for P and F, since they have their parameters and inputs (respectively) as children
  optional_ $ do
    triple_ id_ element_ child_
    MCU.isElement_ element_

  -- For F
  optional_ $ triple_ id_ POutput output_

  -- For types that are declared, for instance:
  -- CountingNumber :: i:Integer where { i >= 0 }
  -- Here 'CountingNumber' is of rdf:type OTypeDeclaration and 'Integer' is OAtomicType
  optional_ $ do
    triple_ typedec_ PType OTypeDeclaration
    triple_ typedec_ PLang lang_
    triple_ typedec_ PLeft  typename_
    triple_ typedec_ PRight  id_

  -- Extract any properties associated with a function type
  optional_ $ do
    triple_ id_ PProperty propertyId_
    triple_ propertyId_ PValue property_

  groupBy id_
  groupBy element_
  groupBy child_
  groupBy type_
  groupBy value_
  groupBy output_
  groupBy lang_
  groupBy typename_
  groupBy property_

  orderNextAsc id_ 
  orderNextAsc element_ 

  select
    [ SelectVar id_        -- The UID of a OType object
    , SelectVar element_   -- The argument number for things that have children
    , SelectVar child_     -- The child ID (for F and P)
    , SelectVar type_      -- The specialized type (OAtomicType | OFunctionType | OParameterizedType)
    , SelectVar value_     -- The type name (e.g. "Int")
    , SelectVar output_    -- The UID of the output of a function (only defined if type_ == OFunctionTyp)
    , SelectVar lang_      -- Language, "Morloc" or something else
    , SelectVar typename_  -- The name associated with a type declaration, e.g., the "Foo" in `Foo :: i:Int {i > 5}`
    , SelectExpr (groupConcat property_ ", ") properties_  -- a comma delimited list of properties (TODO: make this NOT a comma delimited list)
    ]

{- -- for sample.loc
---------------------------------------------------------------------------------------------------------------------------------------------------------------
| id_                  | el_     | child_               | type_               | value_      | output_              | lang_    | typename_         | property_ |
===============================================================================================================================================================
| <rbase__main.loc_20> | <rdf_0> | <rbase__main.loc_21> | <functionType>      |             | <rbase__main.loc_22> | "R"      | "packCharacter"   | "packs"   |
| <rbase__main.loc_21> |         |                      | <atomicType>        | "Character" |                      |          |                   |           |
| <rbase__main.loc_22> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_25> | <rdf_0> | <rbase__main.loc_26> | <functionType>      |             | <rbase__main.loc_27> | "R"      | "packDataFrame"   | "packs"   |
| <rbase__main.loc_26> |         |                      | <atomicType>        | "DataFrame" |                      |          |                   |           |
| <rbase__main.loc_27> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_30> | <rdf_0> | <rbase__main.loc_31> | <functionType>      |             | <rbase__main.loc_32> | "R"      | "packDataTable"   | "packs"   |
| <rbase__main.loc_31> |         |                      | <atomicType>        | "DataTable" |                      |          |                   |           |
| <rbase__main.loc_32> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_35> | <rdf_0> | <rbase__main.loc_36> | <functionType>      |             | <rbase__main.loc_37> | "R"      | "packList"        | "packs"   |
| <rbase__main.loc_36> |         |                      | <atomicType>        | "List"      |                      |          |                   |           |
| <rbase__main.loc_37> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_40> | <rdf_0> | <rbase__main.loc_41> | <functionType>      |             | <rbase__main.loc_42> | "R"      | "packLogical"     | "packs"   |
| <rbase__main.loc_41> |         |                      | <atomicType>        | "Logical"   |                      |          |                   |           |
| <rbase__main.loc_42> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_45> | <rdf_0> | <rbase__main.loc_46> | <functionType>      |             | <rbase__main.loc_47> | "R"      | "packMatrix"      | "packs"   |
| <rbase__main.loc_46> |         |                      | <atomicType>        | "Matrix"    |                      |          |                   |           |
| <rbase__main.loc_47> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_50> | <rdf_0> | <rbase__main.loc_51> | <functionType>      |             | <rbase__main.loc_52> | "R"      | "packNumeric"     | "packs"   |
| <rbase__main.loc_51> |         |                      | <atomicType>        | "Numeric"   |                      |          |                   |           |
| <rbase__main.loc_52> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_55> | <rdf_0> | <rbase__main.loc_56> | <functionType>      |             | <rbase__main.loc_57> | "R"      | "packGeneric"     | "packs"   |
| <rbase__main.loc_56> |         |                      | <atomicGeneric>     | "a"         |                      |          |                   |           |
| <rbase__main.loc_57> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_60> | <rdf_0> | <rbase__main.loc_61> | <functionType>      |             | <rbase__main.loc_62> | "R"      | "unpackGeneric"   | "unpacks" |
| <rbase__main.loc_61> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_62> |         |                      | <atomicGeneric>     | "a"         |                      |          |                   |           |
| <rbase__main.loc_65> | <rdf_0> | <rbase__main.loc_66> | <functionType>      |             | <rbase__main.loc_67> | "R"      | "unpackCharacter" | "unpacks" |
| <rbase__main.loc_66> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_67> |         |                      | <atomicType>        | "Character" |                      |          |                   |           |
| <rbase__main.loc_70> | <rdf_0> | <rbase__main.loc_71> | <functionType>      |             | <rbase__main.loc_72> | "R"      | "unpackDataFrame" | "unpacks" |
| <rbase__main.loc_71> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_72> |         |                      | <atomicType>        | "DataFrame" |                      |          |                   |           |
| <rbase__main.loc_75> | <rdf_0> | <rbase__main.loc_76> | <functionType>      |             | <rbase__main.loc_77> | "R"      | "unpackDataTable" | "unpacks" |
| <rbase__main.loc_76> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_77> |         |                      | <atomicType>        | "DataTable" |                      |          |                   |           |
| <rbase__main.loc_80> | <rdf_0> | <rbase__main.loc_81> | <functionType>      |             | <rbase__main.loc_82> | "R"      | "unpackList"      | "unpacks" |
| <rbase__main.loc_81> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_82> |         |                      | <atomicType>        | "List"      |                      |          |                   |           |
| <rbase__main.loc_85> | <rdf_0> | <rbase__main.loc_86> | <functionType>      |             | <rbase__main.loc_87> | "R"      | "unpackLogical"   | "unpacks" |
| <rbase__main.loc_86> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_87> |         |                      | <atomicType>        | "Logical"   |                      |          |                   |           |
| <rbase__main.loc_90> | <rdf_0> | <rbase__main.loc_91> | <functionType>      |             | <rbase__main.loc_92> | "R"      | "unpackMatrix"    | "unpacks" |
| <rbase__main.loc_91> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_92> |         |                      | <atomicType>        | "Matrix"    |                      |          |                   |           |
| <rbase__main.loc_95> | <rdf_0> | <rbase__main.loc_96> | <functionType>      |             | <rbase__main.loc_97> | "R"      | "unpackNumeric"   | "unpacks" |
| <rbase__main.loc_96> |         |                      | <atomicType>        | "JSON"      |                      |          |                   |           |
| <rbase__main.loc_97> |         |                      | <atomicType>        | "Numeric"   |                      |          |                   |           |
| <sample.loc_10>      |         |                      | <atomicType>        | "Int"       |                      |          |                   |           |
| <sample.loc_11>      |         |                      | <atomicType>        | "Num"       |                      |          |                   |           |
| <sample.loc_12>      |         |                      | <atomicType>        | "Num"       |                      |          |                   |           |
| <sample.loc_13>      | <rdf_0> | <sample.loc_14>      | <parameterizedType> | "List"      |                      |          |                   |           |
| <sample.loc_14>      |         |                      | <atomicType>        | "Num"       |                      |          |                   |           |
| <sample.loc_30>      | <rdf_0> | <sample.loc_31>      | <functionType>      |             | <sample.loc_33>      | "Morloc" | "ceiling"         |           |
| <sample.loc_31>      | <rdf_0> | <sample.loc_32>      | <parameterizedType> | "List"      |                      |          |                   |           |
| <sample.loc_32>      |         |                      | <atomicType>        | "Num"       |                      |          |                   |           |
| <sample.loc_33>      | <rdf_0> | <sample.loc_34>      | <parameterizedType> | "List"      |                      |          |                   |           |
| <sample.loc_34>      |         |                      | <atomicType>        | "Int"       |                      |          |                   |           |
| <sample.loc_36>      | <rdf_0> | <sample.loc_37>      | <functionType>      |             | <sample.loc_38>      | "Morloc" | "rand"            |           |
| <sample.loc_37>      |         |                      | <atomicType>        | "Int"       |                      |          |                   |           |
| <sample.loc_38>      | <rdf_0> | <sample.loc_39>      | <parameterizedType> | "List"      |                      |          |                   |           |
| <sample.loc_39>      |         |                      | <atomicType>        | "Num"       |                      |          |                   |           |
| <sample.loc_9>       | <rdf_0> | <sample.loc_10>      | <functionType>      |             | <sample.loc_13>      | "Morloc" | "rand_uniform"    |           |
| <sample.loc_9>       | <rdf_1> | <sample.loc_11>      | <functionType>      |             | <sample.loc_13>      | "Morloc" | "rand_uniform"    |           |
| <sample.loc_9>       | <rdf_2> | <sample.loc_12>      | <functionType>      |             | <sample.loc_13>      | "Morloc" | "rand_uniform"    |           |
---------------------------------------------------------------------------------------------------------------------------------------------------------------
-}
