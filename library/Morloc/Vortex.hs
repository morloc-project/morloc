{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Vortex
Description : Build the data structures needed for code generation from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Vortex () where

import Morloc.Types
import Morloc.Quasi
import Morloc.Database.HSparql.Connection
import qualified Data.List.Extra as DLE
import qualified Data.Text as DT
import qualified Data.Scientific as DS

type Key  = DT.Text
type Type = DT.Text
type ReturnType = DT.Text -- TODO: extend support to non-atomics

data Argument
  = ArgName Name (Maybe Type)
  | ArgCall Name (Maybe ReturnType) (Maybe Lang)
  | ArgData MData (Maybe Type)
  deriving(Show, Eq, Ord)
    
data Manifold = Manifold {
      callId     :: Key
    , typeId     :: Key
    , morlocName :: Maybe Name
    , sourceName :: Maybe Name
    , boundVars  :: [Name]
    , lang       :: Maybe Lang
    , arguments  :: [Argument] 
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

-- | Collect most of the info needed to build all manifolds
call :: SparqlEndPoint -> IO [Manifold]
call e = fmap (map setArgs . DLE.groupSort . map asTuple) (callQ e)
  where

    asTuple :: [Maybe DT.Text] ->  (Manifold, Argument)
    asTuple [ Just callId'
            , Just typeId'
            , element'
            , morlocName'
            , sourceName'
            , composition'
            , bvars'
            , lang'
            , langType'
            , argname'
            , argcallname'
            , datatype'
            , dataval'
            ] =
      (
        Manifold
          { callId     = callId'
          , typeId     = typeId'
          , morlocName = morlocName' 
          , sourceName = sourceName'
          , boundVars  = maybe [] (DT.splitOn ",") bvars'
          , lang       = lang'
          , arguments  = [] -- this will be set in the next step
          }
      , makeArgument (
          lang'
        , langType'
        , argname'
        , argcallname'
        , makeData <$> datatype' <*> dataval'
        )
      )
    asTuple x = error ("Unexpected SPARQL row:\n" ++ show x)

    makeArgument
      :: ( Maybe Lang -- lang
         , Maybe Name -- language-specific type
         , Maybe Name -- argument name (if it is a bound argument)
         , Maybe Name -- argument call name (if it is a function call)
         , Maybe MData -- argument data (if this is data)
         )
      -> Argument
    makeArgument (_ , t, Just x , _      , _      ) = ArgName x t
    makeArgument (l , t, _      , Just x , _      ) = ArgCall x t l
    makeArgument (_ , t, _      , _      , Just x ) = ArgData x t  
    makeArgument _ = error "Argument must be a name, call or data"

    makeData :: Name -> DT.Text -> MData
    makeData "number"  x = Num' x
    makeData "string"  x = Str' x
    makeData "boolean" x = Log' (x == "true")
    makeData "list"    x = error "lists are not yet supported"
    makeData "tuple"   x = error "typles are not yet supported"
    makeData "record"  x = error "records are not yet supported"
    makeData typename _ = error ("Data type " ++ (DT.unpack typename) ++ " not supported")

    setArgs
      :: (Manifold, [Argument])
      -> Manifold
    setArgs (m, xs) = m { arguments = xs }


callQ :: SparqlEndPoint -> IO [[Maybe DT.Text]]
callQ = [sparql|
PREFIX mlc: <http://www.morloc.io/ontology/000/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX mid: <http://www.morloc.io/XXX/mid/>
SELECT ?call_id ?type_id ?element ?morloc_name ?source_name ?composition
       (group_concat(?bnd; separator=",") as ?bvars)
       ?lang ?lang_type
       ?argname ?argcallname ?datatype ?dataval
WHERE {
    ?call_id rdf:type mlc:call ; 
            rdf:value ?fid .
    ?fid rdf:type mlc:name ;
         rdf:value ?morloc_name .
    ?arg ?element ?call_id .
    FILTER(regex(str(?element), "_[0-9]+$", "i"))
    # Find the bound variables
    OPTIONAL {
        ?datadec rdf:type mlc:dataDeclaration ;
                 mlc:lhs ?composition ;
                 mlc:rhs ?call_id .
        ?bndid ?delement ?datadec ;
               rdf:type mlc:name ;
               rdf:value ?bnd . # bound variables
        FILTER(regex(str(?delement), "_[0-9]+$", "i"))
    }
    # Find the source language
    OPTIONAL {
        ?source_id rdf:type mlc:source ;
                   mlc:lang ?lang ;
                   mlc:import ?import_id .
        ?import_id mlc:name ?source_name ;
                   mlc:alias ?morloc_name .
    }
    # Find language-specific type signature, packer, and unpacker
    OPTIONAL {
       ?lang_typedec rdf:type mlc:typeDeclaration ; 
                     mlc:lang ?lang ;
                     mlc:lhs ?morloc_name ;
                     mlc:rhs ?lang_typeid .
       FILTER(!regex(str(?lang), "Morloc", "i"))
       ?lang_inid ?element ?lang_typeid ;
                  rdf:type mlc:atomicType ; # for now only support atomic
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
        ?arg rdf:type mlc:data;
             rdf:type ?datatype ;
             rdf:value ?dataval .
        FILTER(?datatype != mlc:data)
    }
    #  2. a function call
    OPTIONAL {
        ?arg rdf:type mlc:name ; 
             rdf:value ?argname .
    }
    #  3. a name - the name can only come from one of the bound variables
    OPTIONAL {
        ?arg rdf:type mlc:call ;
             rdf:value ?argcall_id .
        ?argcall_id rdf:type mlc:name ;
                   rdf:value ?argcallname .
    }
}
GROUP BY ?call_id ?type_id ?composition ?lang ?lang_type ?morloc_name ?element ?argname ?argcallname ?datatype ?dataval ?source_name
ORDER BY ?call_id ?element
|]
