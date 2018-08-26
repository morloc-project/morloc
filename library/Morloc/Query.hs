{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Query
Description : SPARQL queries for checking and building code
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Query (
    exportsQ
  , forNexusCallQ
  , sourcesQ
  , sourcesByLangQ
  , languagesQ
  , packersQ
  , arg2typeQ
) where

import qualified Data.Text as DT
import qualified Text.PrettyPrint.Leijen.Text as Gen

import Morloc.Quasi
import Morloc.Database.HSparql.Connection
import Morloc.Types (SparqlEndPoint)

exportsQ :: SparqlEndPoint -> IO [[Maybe DT.Text]]
exportsQ = [sparql|
  PREFIX mlc: <http://www.morloc.io/ontology/000/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

  SELECT ?o
  WHERE {
    ?s rdf:type mlc:export ;
       rdf:value ?o
|]

sourcesQ :: SparqlEndPoint -> IO [[Maybe DT.Text]]
sourcesQ = [sparql|
  PREFIX mlc: <http://www.morloc.io/ontology/000/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

  SELECT ?lang ?fname ?alias ?path 
  WHERE {
    ?s rdf:type mlc:source ;
       mlc:lang ?lang ;
       mlc:import ?i .
    ?i mlc:name ?fname ;
       mlc:alias ?alias .
    OPTIONAL { ?s mlc:path ?path }
  }
|]

sourcesByLangQ :: Gen.Doc -> SparqlEndPoint -> IO [[Maybe DT.Text]]
sourcesByLangQ lang = [sparql|
  PREFIX mlc: <http://www.morloc.io/ontology/000/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

  SELECT DISTINCT ?path
  WHERE {
    ?s rdf:type mlc:source ;
       mlc:lang ${lang} ;
       mlc:path ?path .
  }
|]


-- | This query returns the following:
--   1. name - name of an exported Morloc function
--   2. lang - the language the function is exported from
--   3. mid - id of the specific morloc manifold (id of the type declaration)
--   4. nargs - number of arguments the function takes
forNexusCallQ :: SparqlEndPoint -> IO [[Maybe DT.Text]]
forNexusCallQ = [sparql|
  PREFIX mlc: <http://www.morloc.io/ontology/000/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

  SELECT ?fname ?lang ?typedec (count (distinct ?element) as ?nargs)
  WHERE {
    ?typedec rdf:type mlc:typeDeclaration ;
       mlc:lang "Morloc" ;
       mlc:lhs ?fname ;
       mlc:rhs ?type .
    ?type rdf:type mlc:functionType .
    ?arg ?element ?type .
    FILTER(regex(str(?element), "_[0-9]+$", "i"))
    ?src rdf:type mlc:source ;
         mlc:lang ?lang ;
         mlc:import ?i .
    ?i mlc:alias ?fname .
    ?e rdf:type mlc:export ;
       rdf:value ?fname .
  }
  GROUP BY ?typedec ?fname ?lang
|]

-- | Find all packers
-- @
--   <function> <lang> :: packs => <from> -> JSON
-- @
packersQ :: SparqlEndPoint -> IO [[Maybe DT.Text]]
packersQ = [sparql|
  PREFIX mlc: <http://www.morloc.io/ontology/000/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

  SELECT ?lang ?mlc_name ?function_name ?pack_type ?pack_value
  WHERE {
    # find language-specific type declarations
    ?type rdf:type mlc:typeDeclaration ;
          mlc:lang ?lang ;
          mlc:lhs ?mlc_name ;
          mlc:rhs ?type_rhs .
    # that have the property "packs"
    ?type_rhs rdf:type mlc:functionType ;
              mlc:property ?property .
    # properties may by compound (e.g. when used to specify type constraints)
    ?property rdf:type mlc:name ;
              rdf:value "packs" .
    ?input rdf:_0 ?type_rhs;
           rdf:type ?pack_type .
    ?output rdf:type mlc:atomicType ;
            rdf:value "JSON" .
    OPTIONAL {
      ?input rdf:value ?pack_value
    }
    # If the function was sourced, then the morloc name may be an alias for the
    # real name. So here find the real name.
    OPTIONAL
    {
      ?source rdf:type mlc:source ;
              mlc:lang ?lang ;
              mlc:import ?import .
      ?import mlc:name  ?function_name ;
              mlc:alias ?mlc_name .
    }
  }
|]

languagesQ :: SparqlEndPoint -> IO [[Maybe DT.Text]]
languagesQ = [sparql|
  PREFIX mlc: <http://www.morloc.io/ontology/000/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

  SELECT DISTINCT ?lang
  WHERE {
    ?i rdf:type mlc:source ;
       mlc:lang ?lang .
  }
|]

-- | Map the arguments in a call to their types
arg2typeQ :: SparqlEndPoint -> IO [[Maybe DT.Text]]
arg2typeQ = [sparql|
  PREFIX mlc: <http://www.morloc.io/ontology/000/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

  SELECT ?fname ?element ?argvalue ?argtype ?argform ?argname
  WHERE {
      ?call rdf:type mlc:call ;
         mlc:value ?value .  
      ?value rdf:type ?value_type .
      ?value rdf:value ?fname .
      ?argvalue ?element ?call .
      ?typedec rdf:type mlc:typeDeclaration ;
               mlc:lang "Morloc" ;
               mlc:lhs ?fname ;
               mlc:rhs ?ftype .
      ?ftype rdf:type mlc:functionType .
      ?argtype ?element ?ftype .
      OPTIONAL { ?argtype rdf:type ?argform . }
      OPTIONAL { ?argtype rdf:value ?argname . }
      FILTER(regex(str(?element), "_[0-9]+$", "i"))
  }
|]
