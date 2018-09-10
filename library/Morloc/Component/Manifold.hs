{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Component.Manifold
Description : Build manifolds for code generation from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.Manifold (fromSparqlDb) where

import Morloc.Types
import Morloc.Operators
import Morloc.Quasi
import qualified Morloc.System as MS
import qualified Morloc.Util as MU
import qualified Morloc.Triple as M3
import qualified Morloc.Component.MType as MCT 
import qualified Morloc.Component.MData as MCD 

import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))
import Morloc.Database.HSparql.Connection
import qualified Data.Map.Strict as Map
import qualified Data.List.Extra as DLE
import qualified Data.Text as DT
import qualified Data.Maybe as DM
import qualified Safe as Safe

-- | Collect most of the info needed to build all manifolds
fromSparqlDb :: SparqlEndPoint -> IO [Manifold]
fromSparqlDb ep = do
  typemap <- MCT.fromSparqlDb ep
  datamap <- MCD.fromSparqlDb ep
  mandata <- sparqlQuery ep
  return $ ( unroll
           . setCalls
           . setLangs
           . DLE.groupSort
           . propagateBoundVariables
           . map (asTuple typemap datamap) 
           ) mandata

asTuple
  :: Map.Map Key MType
  -> Map.Map Key MData
  -> [Maybe DT.Text]
  -> (Manifold, Either Key Argument)
asTuple typemap datamap [ Just callId'
                        , abstractTypeId'
                        , element'
                        , Just morlocName'
                        , sourceName'
                        , composition'
                        , bvars'
                        , sourceLang'
                        , sourcePath'
                        , Just called'
                        , Just sourced'
                        , Just exported'
                        , concreteTypeId'
                        , argname'
                        , argcall_id'
                        , argdata_id'
                        ] =
  (
    Manifold
      { mCallId       = callId'
      , mAbstractType = abstractTypeId' >>= (flip Map.lookup) typemap
      , mConcreteType = concreteTypeId' >>= (flip Map.lookup) typemap
      , mMorlocName   = morlocName'
      , mCallName     = maybe morlocName' id sourceName'
      , mSourceName   = sourceName'
      , mComposition  = composition'
      , mCalled       = called'   == "true"
      , mExported     = exported' == "true"
      , mSourced      = sourced'  == "true"
      , mSourcePath   = sourcePath'
      , mBoundVars    = maybe [] (DT.splitOn ",") bvars'
      , mLang         = sourceLang'
      , mArgs         = [] -- this will be set in the next step
      }
  , makeArgument (
      argname'
    , argcall_id'
    , argdata_id' >>= (flip Map.lookup) datamap
    , element'
    )
  )
asTuple _ _ x = error ("Unexpected SPARQL row:\n" ++ show x)

setCalls :: [(Manifold, [Either Key Argument])] -> [Manifold]
setCalls xs = map (\(m, args) -> m { mArgs = map (set hash) args }) xs
  where
    set :: Map.Map Key Manifold -> Either Key Argument -> Argument
    set h (Left key) = case Map.lookup key h of
      (Just m) -> ArgCall m
      Nothing -> error "Call to non-existing manifold"
    set _ (Right a) = a

    hash :: Map.Map Key Manifold
    hash = Map.fromList $ zip (map (mCallId . fst) xs) (map fst xs)

setLangs :: [(Manifold, a)] -> [(Manifold, a)]
setLangs ms = map (setLang hash) ms
  where
    setLang :: (Map.Map Name Lang) -> (Manifold, a) -> (Manifold, a)
    setLang h (m, a) = (m { mLang = Map.lookup (mMorlocName m) h }, a)

    hash :: Map.Map Name Lang
    hash = MU.spreadAttr (map (\(m,_) -> ( mMorlocName m
                                         , mComposition m
                                         , mLang m)) ms)

makeArgument
  :: ( Maybe Name    -- argument name (if it is a bound argument)
     , Maybe Key     -- argument callId (if it is a function call)
     , Maybe MData   -- argument data (if this is data)
     , Maybe DT.Text -- the element (rdf:_<num>)
     )
  -> Either Key Argument
makeArgument (Just x  , _       , _       , _ ) = Right $ ArgName x
makeArgument (_       , Just x  , _       , _ ) = Left x
makeArgument (_       , _       , Just x  , _ ) = Right $ ArgData x
makeArgument (Nothing , Nothing , Nothing , Just e) =
  case (DT.stripPrefix (M3.rdfPre <> "_") e) >>= (Safe.readMay . DT.unpack) of
    Just i -> Right $ ArgPosi i
    _ -> error ("Unexpected value for element: " ++ show e)

propagateBoundVariables :: [(Manifold, Either Key Argument)] -> [(Manifold, Either Key Argument)]
propagateBoundVariables ms = map setBoundVars ms 
  where
    setBoundVars :: (Manifold, Either Key Argument) -> (Manifold, Either Key Argument)
    setBoundVars (m, a) = (m { mBoundVars = maybe [] id (Map.lookup (mCallId m) hash) }, a)

    -- map from mcallId to mBoundVars
    hash :: Map.Map Key [Name]
    hash = MU.spreadAttr (map toTriple ms)

    toTriple :: (Manifold, Either Key Argument) -> (Key, Maybe Key, Maybe [Name])
    toTriple (m, Left k) = (mCallId m, Just k, toMaybe (mBoundVars m))
    toTriple (m, _) = (mCallId m, Nothing, toMaybe (mBoundVars m))

    toMaybe :: [a] -> Maybe [a]
    toMaybe [] = Nothing
    toMaybe xs = Just xs

-- | This function creates a tree of new manifolds to represent the call tree
-- of a called Morloc composition.
unroll :: [Manifold] -> [Manifold]
unroll ms = concat $ map unroll' ms
  where
    unroll' :: Manifold -> [Manifold]
    unroll' m
      | not ((mCalled m) && (not $ mSourced m)) = [m]
      | otherwise = case filter (declaringManifold m) ms of
         [r] -> unrollPair m r
         _ -> error "Expected exactly one declaration"

    unrollPair :: Manifold -> Manifold -> [Manifold]
    unrollPair m r = [m'] ++ unroll' r' where
      signedKey = signKey (mCallId m) (mCallId r)
      r' = r { mCallId = signedKey, mExported = False }
      m' = m { mCallName = MS.makeManifoldName signedKey }

    signKey :: Key -> Key -> Key
    signKey m r =
      case
        (DT.stripPrefix M3.midPre m, DT.stripPrefix M3.midPre r)
      of
        (Just mKey, Just rKey) -> M3.midPre <> mKey <> "_" <> rKey
        _ -> error ("callId of invalid form: " ++ show (m, r))

    declaringManifold :: Manifold -> Manifold -> Bool
    declaringManifold m n = (Just (mMorlocName m) == mComposition n)



sparqlQuery :: SparqlEndPoint -> IO [[Maybe DT.Text]]
sparqlQuery = [sparql|
PREFIX mlc: <http://www.morloc.io/ontology/000/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX mid: <http://www.morloc.io/XXX/mid/>
SELECT ?call_id ?abstract_type_id ?element ?morloc_name ?source_name ?composition
       (group_concat(?bnd; separator=",") as ?bvars)
       ?source_lang ?source_path ?called ?sourced ?exported ?concrete_type_id
       ?argname ?argcall_id ?argdata_id
WHERE {
    SELECT *
    WHERE {
        {
            ?call_id rdf:type mlc:call ;
                    rdf:value ?fid ;
                    ?element ?arg .
            FILTER(regex(str(?element), "_[0-9]+$", "i"))
            ?fid rdf:type mlc:name ;
                 rdf:value ?morloc_name .
        } UNION {
            # Find exported values
            ?call_id rdf:type mlc:export ;
                     rdf:value ?morloc_name .
            # This is exported from the global environment
            ?script_id rdf:type mlc:script ;
                       rdf:value "<stdin>" ;
                       ?script_element ?call_id .
            FILTER(regex(str(?script_element), "_[0-9]+$", "i"))
            ?typedec rdf:type mlc:typeDeclaration ;
                     mlc:lang "Morloc" ;
                     mlc:lhs ?morloc_name ;
                     mlc:rhs ?typeid .
            ?typeid ?element ?arg;
            FILTER(regex(str(?element), "_[0-9]+$", "i"))
            # Keep only the values that are NOT calls (to avoid duplication)
            MINUS {
                ?call_id rdf:type mlc:call .
            }
        }
        BIND(bound(?fid) AS ?called)
        # Determine whether this is exported
        OPTIONAL {
            ?e rdf:type mlc:export ;
               rdf:value ?morloc_name .
        }
        BIND(bound(?e) AS ?exported)
        # Find the bound variables
        OPTIONAL {
            ?datadec rdf:type mlc:dataDeclaration ;
                     mlc:lhs ?composition ;
                     mlc:rhs ?call_id ;
                     ?bnd_element ?bnd_id .
            FILTER(regex(str(?bnd_element), "_[0-9]+$", "i"))
            ?bnd_id rdf:type mlc:name ;
                   rdf:value ?bnd . # bound variables
        }
        # Find the source language
        OPTIONAL {
            ?source_id rdf:type mlc:source ;
                       mlc:lang ?source_lang ;
                       mlc:import ?import_id .
            ?import_id mlc:name ?source_name ;
                       mlc:alias ?morloc_name .
            OPTIONAL {
                ?source_id mlc:path ?source_path .
            }
        }
        BIND(bound(?source_id) AS ?sourced)
        # Find language-specific type signature, packer, and unpacker
        OPTIONAL {
           ?lang_typedec rdf:type mlc:typeDeclaration ;
                         mlc:lang ?source_lang ;
                         mlc:lhs ?morloc_name ;
                         mlc:rhs ?concrete_type_id.
           FILTER(!regex(str(?source_lang), "Morloc", "i"))
        }
        # Find the type delcaration ID
        OPTIONAL {
            ?dectype_id rdf:type mlc:typeDeclaration ;
                     mlc:lang "Morloc" ;
                     mlc:lhs ?morloc_name ;
                     mlc:rhs ?abstract_type_id .
        }
        # A argument must be one of the following:
        #  1. raw data
        OPTIONAL {
            ?arg rdf:type mlc:data.
            BIND(?arg AS ?argdata_id)
        }
        #  2. a function call
        OPTIONAL {
            ?arg rdf:type mlc:call .
            BIND(?arg AS ?argcall_id)
        }
        #  3. a name - the name can only come from one of the bound variables
        OPTIONAL {
            FILTER(bound(?arg))
            ?arg rdf:type mlc:name ;
                 rdf:value ?argname .
        }
    }
    ORDER BY ?bnd_element 
}
GROUP BY ?call_id ?element ?abstract_type_id ?concrete_type_id ?morloc_name ?source_name ?composition
         ?source_lang ?source_path ?called ?sourced ?exported 
         ?argname ?argcall_id ?argdata_id
ORDER BY ?call_id ?element 
|]
