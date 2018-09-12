{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Pools.Pools
Description : Generate language-specific code
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pools.Pools (generate) where

import Morloc.Types
import Morloc.Operators
import Morloc.Quasi
import qualified Morloc.Data.Text as MT

import qualified Morloc.Pools.Template.R as RLang
import qualified Morloc.Pools.Template.Python3 as Py3

generate :: SparqlEndPoint -> IO [Script]
generate ep = languagesQ ep >>= foo' where 
  foo' :: [[Maybe MT.Text]] -> IO [Script]
  foo' xss = sequence (map (generateLang ep) xss)

generateLang :: SparqlEndPoint -> [Maybe MT.Text] -> IO Script
generateLang e lang = case lang of
  [Just "R"] -> RLang.generate e
  [Just "py"] -> Py3.generate e
  [Just x] -> error ("The language " ++ show x ++ " is not supported")
  x -> error ("Bad SPARQL query:" ++ show x)

languagesQ :: SparqlEndPoint -> IO [[Maybe MT.Text]]
languagesQ = [sparql|
  PREFIX mlc: <http://www.morloc.io/ontology/000/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

  SELECT DISTINCT ?lang
  WHERE {
    ?i rdf:type mlc:source ;
       mlc:lang ?lang .
  }
|]
