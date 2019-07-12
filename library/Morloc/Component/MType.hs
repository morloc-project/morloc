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
  , makeAbstractFunctionMap
) where

import Morloc.Sparql
import Morloc.Global
import Morloc.Operators
import Morloc.Data.Doc hiding ((<>))
import Morloc.Util as U
import qualified Morloc.Language as ML
import qualified Morloc.Component.Util as MCU
import qualified Morloc.Data.Text as MT
import qualified Morloc.Data.RDF as MR
import qualified Morloc.Monad as MM
import qualified Morloc.TypeHandler as MTH

import qualified Data.Map.Strict as Map
import qualified Data.List.Extra as DLE

type ParentData =
  ( MT.Text       -- type (e.g. mlc:functionType or mlc:atomicGeneric)
  , Maybe MT.Text -- top-level name of the type (e.g. "List" or "Int")
  , Maybe URI     -- type id of the output if this is a function
  , Maybe MT.Text -- type language ("Morloc" for a general type)
  , Maybe Name    -- typename from a typeDeclaration statement
  , [[Name]] -- But this is transformed into a property list
  )

instance Pretty MType where
  pretty (MConcType _ n []) = pretty n
  pretty (MConcType _ n ts) = parens $ hsep (pretty n:(map pretty ts))
  pretty (MAbstType _ n []) = pretty n
  pretty (MAbstType _ n ts) = parens $ hsep (pretty n:(map pretty ts))
  pretty (MFuncType _ ts o) = parens $
    (hcat . punctuate ", ") (map pretty ts) <> " -> " <> pretty o

fromSparqlDb :: (SparqlDatabaseLike db) => db -> MorlocMonad (Map.Map URI MType)
fromSparqlDb db
  = MCU.simpleGraph
      toMType
      getParentData
      URI
      (sparqlSelect "mtype" hsparql)
      db
  >>= MM.logFileWith "mtype.txt" Map.assocs

getParentData :: [[Maybe MT.Text]] -> MorlocMonad ParentData
getParentData xss
  = mapM tuplify xss -- [((t,a,o,l,n), Just (g,i,v))]
  >>= U.groupSortWith groupProperties -- [((t,a,o,l,n), [[Name]])]
  |>> map (\((t,a,o,l,n),ps) -> (t,a,o,l,n,ps)) 
  |>> head
  where
    tuplify [Just t, a, o, l, n, (Just g), (Just i), (Just v)] = return ((t,a, fmap URI o,l,n), Just (URI g,i,v))
    tuplify [Just t, a, o, l, n, _, _, _] = return ((t,a,fmap URI o,l,n), Nothing)
    tuplify x = MM.throwError . SparqlFail $ "Unexpected SPARQL result: " <> MT.show' x

    groupProperties xs = case sequence xs of
      Nothing -> return []
      (Just xs)
        -> fmap (map snd)
        . U.groupSortWith (return . map snd . DLE.sort)
        . map (\(g,i,v)->(g,(i,v)))
        $ xs

toMType :: Map.Map URI (ParentData, [URI]) -> URI -> MorlocMonad MType
toMType h k = toMType' (Map.lookup k h) where
  toMType' (Just ((t, v, o, l, n, ps), xs)) =
    makeMeta l n ps >>= toMType'' t v o xs
  toMType' Nothing = MM.throwError $ CallTheMonkeys "MType.hs::toMType'"

  toMType'' t (Just v) _ xs meta 
    | isGeneric t = MAbstType meta v <$> mapM (toMType h) xs
    | otherwise   = MConcType meta v <$> mapM (toMType h) xs
  toMType'' _ _ (Just o) xs meta = do
    inputTypes <- mapM (toMType h) xs
    outputType <- toMType h o
    return $ MFuncType meta inputTypes outputType
  toMType'' _ _ _ _ _ = MM.throwError $ CallTheMonkeys "MType.hs::toMType'"

  makeMeta :: Maybe MT.Text -> Maybe Name -> [[Name]] -> MorlocMonad MTypeMeta
  makeMeta (Just langStr) n ps = case ML.readLangName langStr of
    (Just lang) ->
      return $ MTypeMeta {
          metaName = n
        , metaProp = ps
        , metaLang = (Just lang)
      }
    Nothing -> MM.throwError $ UnknownLanguage langStr
  makeMeta Nothing n ps = return $ MTypeMeta {
          metaName = n
        , metaProp = ps
        , metaLang = Nothing
      }

isGeneric :: MT.Text -> Bool
isGeneric x = MR.UNode x == asRdfNode OAtomicGenericType ||
              MR.UNode x == asRdfNode OParameterizedGenericType

makeAbstractFunctionMap
  :: Map.Map URI MType
  -> Map.Map Name MType
makeAbstractFunctionMap
  = Map.mapMaybe MTH.chooseAbstraction -- Map Name MType
  . Map.fromList -- Map Name [MType]
  . DLE.groupSort -- [(Name, [MType])]
  . Map.elems -- [(Name, MType)]
  . Map.mapMaybe toNameKeyList -- Map Name (Name, MType)
  where
    toNameKeyList :: MType -> Maybe (Name, MType)
    toNameKeyList t = case t of
      (MFuncType (MTypeMeta (Just name) _ (Just MorlocLang)) _ _) -> Just (name, t) 
      _ -> Nothing

-- | Extracts data needed to build types
hsparql :: Query SelectQuery
hsparql = do
  id_             <- var
  m_              <- var
  child_position_ <- var
  child_          <- var
  type_           <- var
  value_          <- var
  output_         <- var
  lang_           <- var
  typename_       <- var
  typedec_        <- var
  -- variables for getting properties
  propertyId_         <- var
  propertyElementIdx_ <- var
  elementId_          <- var
  elementName_        <- var
  e_                  <- var

  -- For all types, the expression `?i rdf:type ?t` will return two objects.
  -- The first is `mlc:type`, which is common to all types. The second can be
  -- mlc:atomicType (A), mlc:parameterizedType (P), or mlc:functionType (F).
  triple_ id_ PType OType -- match the common mlc:type
  triple_ id_ PType type_       -- \ matches the specialized type
  filterExpr (type_ .!=. OType) -- /

  -- Get the type name, defined when type_ is OAtomicType or OParameterizedType
  optional_ $ do
    triple_ id_ PValue value_

  -- This will run for P and F, since they have their parameters and inputs (respectively) as children
  optional_ $ do
    triple_ id_ PElem m_
    triple_ m_ PValue child_
    triple_ m_ PPosition child_position_

  -- For F
  optional_ $ do
    triple_ id_ POutput output_

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
    filterExpr (type_ .==. OFunctionType)
    triple_ id_ PProperty propertyId_
    triple_ propertyId_ PType OProperty
    triple_ propertyId_ PElem e_
    triple_ e_ PPosition propertyElementIdx_
    triple_ e_ PValue elementId_
    triple_ elementId_ PType OName
    triple_ elementId_ PValue elementName_

  orderNextAsc id_ 
  orderNextAsc child_position_ 

  selectVars
    [ id_                 -- The UID of a OType object
    , child_position_     -- The argument number for things that have children
    , child_              -- The child ID (for F and P)
    , type_               -- The specialized type (OAtomicType | OFunctionType | OParameterizedType)
    , value_              -- The type name (e.g. "Int")
    , output_             -- The UID of the output of a function (only defined if type_ == OFunctionTyp)
    , lang_               -- Language, "Morloc" or something else
    , typename_           -- The name of type declaration ("Foo" in `Foo :: i:Int {i > 5}`)
    , propertyId_         -- member index for a property
    , propertyElementIdx_ -- element index of a name within a property
    , elementName_        -- value of an element name
    ]
