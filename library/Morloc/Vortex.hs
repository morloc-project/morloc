{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Vortex
Description : Build the data structures needed for code generation from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Vortex (
    buildManifolds
  , buildPackHash
  , findSources
  , Argument(..)
  , Manifold(..)
  , MData(..)
  , PackHash(..)
) where

import Morloc.Types
import Morloc.Operators
import Morloc.Quasi
import qualified Morloc.System as MS
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))
import qualified Morloc.Util as MU
import qualified Morloc.Triple as M3
import Morloc.Database.HSparql.Connection
import qualified Data.HashMap.Strict as Map
import qualified Data.List.Extra as DLE
import qualified Data.Text as DT
import qualified Data.Maybe as DM
import qualified Data.Scientific as DS
import qualified System.IO as IO
import qualified Safe as Safe

type Key  = DT.Text
type Type = DT.Text
type ReturnType = DT.Text -- TODO: extend support to non-atomics

data Argument
  = ArgName Name (Maybe Type)
  -- ^ Morloc variables that are defined in scope
  | ArgCall Key (Maybe ReturnType) (Maybe Lang)
  -- ^ A call to some function
  | ArgData MData (Maybe Type)
  -- ^ Raw data defined in one of the Morloc internal types
  | ArgPosi Int (Maybe Type)
  -- ^ A manifold positional argument (passed into a function assignment)
  deriving(Show, Eq, Ord)

data Manifold = Manifold {
      mCallId      :: Key
    , mTypeId      :: Maybe Key
    , mExported    :: Bool
    , mCalled      :: Bool
    , mSourced     :: Bool
    , mMorlocName  :: Name
    , mCallName    :: Name
    , mSourcePath  :: Maybe Path
    , mSourceName  :: Maybe Name
    , mComposition :: Maybe Name
    , mBoundVars   :: [Name]
    , mLang        :: Maybe Lang
    , mArgs        :: [Argument]
  }
  deriving(Show, Eq, Ord)

-- The values are left unparsed, since they will be used as text
data MData
  = Num' DT.Text
  | Str' DT.Text
  | Log' Bool -- booleans are parsed, since representation depend on language
  | Lst' [MData]
  | Rec' [(Name, MData)]
  | Tup' [MData]
  deriving(Show, Eq, Ord)

data PackHash = PackHash {
      packer   :: Map.HashMap Type Name
    , unpacker :: Map.HashMap Type Name
    , genericPacker   :: Name
    , genericUnpacker :: Name
    , sources :: [Path] -- Later, I might want to link the source files to
                        -- each function, but for now that isn't needed.
  }
  deriving(Show, Eq, Ord)

-- | Collect most of the info needed to build all manifolds
buildManifolds :: SparqlEndPoint -> IO [Manifold]
buildManifolds e = fmap ( unroll
                        . setLangs
                        . map setArgs
                        . DLE.groupSort
                        . propadateBoundVariables
                        . map asTuple
                        ) (manifoldQ e)

asTuple :: [Maybe DT.Text] ->  (Manifold, Argument)
asTuple [ Just callId'
        , typeId'
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
        , langType'
        , argname'
        , argcall_id'
        , datatype'
        , dataval'
        ] =
  (
    Manifold
      { mCallId      = callId'
      , mTypeId      = typeId'
      , mMorlocName  = morlocName'
      , mCallName    = maybe morlocName' id sourceName'
      , mSourceName  = sourceName'
      , mComposition = composition'
      , mCalled      = called'   == "true"
      , mExported    = exported' == "true"
      , mSourced     = sourced'  == "true"
      , mSourcePath  = sourcePath'
      , mBoundVars   = maybe [] (DT.splitOn ",") bvars'
      , mLang        = sourceLang'
      , mArgs        = [] -- this will be set in the next step
      }
  , makeArgument (
      Nothing -- to set the language, I need to look up the argname
    , langType'
    , argname'
    , argcall_id'
    , makeData <$> datatype' <*> dataval'
    , element'
    )
  )
asTuple x = error ("Unexpected SPARQL row:\n" ++ show x)

makeArgument
  :: ( Maybe Lang -- lang
     , Maybe Name -- language-specific type
     , Maybe Name -- argument name (if it is a bound argument)
     , Maybe Key  -- argument callId (if it is a function call)
     , Maybe MData -- argument data (if this is data)
     , Maybe DT.Text -- the element (rdf:_<num>)
     )
  -> Argument
makeArgument (_ , t, Just x  , _       , _       , _ ) = ArgName x t
makeArgument (l , t, _       , Just x  , _       , _ ) = ArgCall x t l
makeArgument (_ , t, _       , _       , Just x  , _ ) = ArgData x t
makeArgument (_ , t, Nothing , Nothing , Nothing , Just e) =
  case (DT.stripPrefix (M3.rdfPre <> "_") e) >>= (Safe.readMay . DT.unpack) of
    Just i -> ArgPosi i t
    _ -> error ("Unexpected value for element: " ++ show e)

makeData :: Name -> DT.Text -> MData
makeData t n = case DT.stripPrefix M3.mlcPre t of
  Nothing -> error ("Exected Morloc data type (mlc prefix), got: " ++ show t)
  Just t -> makeData' t n where
    makeData' "number"  x = Num' x
    makeData' "string"  x = Str' x
    makeData' "boolean" x = Log' (x == "true")
    makeData' "list"    x = error "lists are not yet supported"
    makeData' "tuple"   x = error "typles are not yet supported"
    makeData' "record"  x = error "records are not yet supported"
    makeData' typename _ = error ("Data type " ++ (DT.unpack typename) ++ " not supported")

setArgs
  :: (Manifold, [Argument])
  -> Manifold
setArgs (m, xs) = m { mArgs = xs }

setLangs :: [Manifold] -> [Manifold]
setLangs ms = map setArgs ms' where

  ms' = map (setLang hash) ms

  setLang :: (Map.HashMap Name Lang) -> Manifold -> Manifold
  setLang h m = m { mLang = Map.lookup (mMorlocName m) h }

  setArgs m = m { mArgs = map setArgLang (mArgs m) } 

  setArgLang :: Argument -> Argument
  setArgLang arg = case arg of
    (ArgCall k t _) -> ArgCall k t (findLang k ms')
    _ -> arg

  findLang :: Key -> [Manifold] -> Maybe Lang
  findLang k ms = case filter (\m -> mCallId m == k) ms of
    [m] -> mLang m
    _ -> error "Could not find language of foreign call" 

  hash :: Map.HashMap Name Lang
  hash = MU.spreadAttr (map (\m -> (mMorlocName m, mComposition m, mLang m)) ms)

propadateBoundVariables :: [(Manifold, Argument)] -> [(Manifold, Argument)]
propadateBoundVariables ms = map setBoundVars ms 
  where
    setBoundVars :: (Manifold, Argument) -> (Manifold, Argument)
    setBoundVars (m, a) = (m { mBoundVars = maybe [] id (Map.lookup (mCallId m) hash) }, a)

    -- map from mcallId to mBoundVars
    hash :: Map.HashMap Key [Name]
    hash = MU.spreadAttr (map toTriple ms)

    toTriple :: (Manifold, Argument) -> (Key, Maybe Key, Maybe [Name])
    toTriple (m, ArgCall k _ _) = (mCallId m, Just k, toMaybe (mBoundVars m))
    toTriple (m, _) = (mCallId m, Nothing, toMaybe (mBoundVars m))

    toMaybe :: [a] -> Maybe [a]
    toMaybe [] = Nothing
    toMaybe xs = Just xs

-- TODO: update this to limit results to one language
-- OR return a hash of hashes by language
buildPackHash :: Lang -> SparqlEndPoint -> IO PackHash
buildPackHash lang se = toPackHash <$> (map tuplify <$> serializationQ lang' se)
  where
    tuplify :: [Maybe DT.Text] -> (Type, Name, Bool, Name, Path)
    -- typename | property | is_generic | name | path
    tuplify [Just t, Just p, Just g, Just n, Just s] = (t,p,g == "true",n,s)
    tuplify e = error ("Unexpected SPARQL result: " ++ show e)

    lang' = dquotes (text' lang)

    toPackHash :: [(Type, Name, Bool, Name, Path)] -> PackHash
    toPackHash xs = case
      ( Map.fromList [(t,p) | (t, "packs"  , False, p, _) <- xs]
      , Map.fromList [(t,p) | (t, "unpacks", False, p, _) <- xs]
      , [p | (_, "packs"  , True, p, _) <- xs]
      , [p | (_, "unpacks", True, p, _) <- xs]
      , MU.unique [s | (_, _, _, _, s) <- xs]
      ) of
        (phash, uhash, [p], [u], srcs) -> PackHash
          { packer = phash
          , unpacker = uhash
          , genericPacker = p
          , genericUnpacker = u
          , sources = srcs
          }
        e -> error ("Expected exactly one generic packer/unpacker: " ++ show xs)

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

manifoldQ :: SparqlEndPoint -> IO [[Maybe DT.Text]]
manifoldQ = [sparql|
PREFIX mlc: <http://www.morloc.io/ontology/000/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX mid: <http://www.morloc.io/XXX/mid/>
SELECT ?call_id ?type_id ?element ?morloc_name ?source_name ?composition
       (group_concat(?bnd; separator=",") as ?bvars)
       ?source_lang ?source_path ?called ?sourced ?exported ?lang_type
       ?argname ?argcall_id ?datatype ?dataval
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
                 ?delement ?bnd_id .
        FILTER(regex(str(?delement), "_[0-9]+$", "i"))
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
                     mlc:rhs ?lang_typeid .
       FILTER(!regex(str(?source_lang), "Morloc", "i"))
       ?lang_typeid ?element ?lang_inid .
       ?lang_inid rdf:type mlc:atomicType ; # for now only support atomic
                  rdf:value ?lang_type .
    }
    # Find the type delcaration ID
    OPTIONAL {
        ?type_id rdf:type mlc:typeDeclaration ;
                 mlc:lang "Morloc" ;
                 mlc:lhs ?morloc_name .
    }
    # A argument must be one of the following:
    #  1. raw data
    OPTIONAL {
        FILTER(bound(?arg))
        ?arg rdf:type mlc:data;
             rdf:type ?datatype ;
             rdf:value ?dataval .
        FILTER(?datatype != mlc:data)
    }
    #  2. a function call
    OPTIONAL {
        FILTER(bound(?arg))
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
GROUP BY ?call_id ?type_id ?called ?composition ?source_lang ?source_path ?sourced ?exported ?lang_type ?morloc_name ?element ?argname ?argcall_id ?datatype ?dataval ?source_name
ORDER BY ?call_id ?element
|]

serializationQ :: Doc -> SparqlEndPoint -> IO [[Maybe DT.Text]]
serializationQ lang = [sparql|
PREFIX mlc: <http://www.morloc.io/ontology/000/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX mid: <http://www.morloc.io/XXX/mid/>
SELECT DISTINCT ?typename ?property ?is_generic ?name ?path
WHERE {
        # Get serialization functions of type `a -> JSON`
        ?id rdf:type mlc:typeDeclaration ;
              mlc:lang ${lang} ;
              mlc:lhs ?name ;
              mlc:rhs ?rhs .
        ?rhs rdf:type mlc:functionType ;
                  mlc:property ?property_id ;
                  mlc:output ?output .
        ?property_id rdf:type mlc:name .
        {
            ?property_id rdf:value ?property .
            ?output rdf:type mlc:atomicType ;
                    rdf:value "JSON" .
            ?rhs rdf:_0 ?packer_input .
            ?packer_input rdf:value ?typename .
            BIND(exists{?packer_input rdf:type mlc:atomicGeneric} AS ?is_generic)
            FILTER(?property = "packs")
        } UNION {
            ?property_id rdf:value ?property .
            ?rhs rdf:_0 ?unpacker_input .
            ?unpacker_input rdf:type mlc:atomicType ;
                            rdf:value "JSON" .
            ?output rdf:value ?typename .
            BIND(exists{?output rdf:type mlc:atomicGeneric} AS ?is_generic)
            FILTER(?property = "unpacks")
        }
        OPTIONAL{
           ?source_id rdf:type mlc:source ;
                      mlc:lang ${lang} ;
                      mlc:path ?path ;
                      mlc:import ?import_id .
           ?import_id mlc:alias ?name .
        }
}
ORDER BY ?property ?typename
|]

findSources :: SparqlEndPoint -> IO [(DT.Text,Lang)]
findSources e = fmap (map tuplify) (findSourcesQ e) where
  tuplify :: [Maybe DT.Text] -> (DT.Text, Lang)
  tuplify [Just x, Just y] = (x,y)
  tuplify x = error ("Unexpected SPARQL result: " ++ show x)

findSourcesQ :: SparqlEndPoint -> IO [[Maybe DT.Text]]
findSourcesQ = [sparql|
PREFIX mlc: <http://www.morloc.io/ontology/000/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX mid: <http://www.morloc.io/XXX/mid/>
SELECT ?path ?lang
WHERE {
  ?s rdf:type mlc:source ;
     mlc:lang ?lang ;
     mlc:path ?path .
}
|]
