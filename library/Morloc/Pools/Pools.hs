{-# LANGUAGE OverloadedStrings #-}

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
import Morloc.Sparql
import Morloc.Config (Config)
import qualified Morloc.Data.Text as MT

import qualified Morloc.Pools.Template.R as RLang
import qualified Morloc.Pools.Template.Python3 as Py3

generate :: SparqlDatabaseLike db => Config -> db -> IO [Script]
generate config db = sparqlSelect hsparql db >>= foo' where 
  foo' :: [[Maybe MT.Text]] -> IO [Script]
  foo' xss = sequence (map (generateLang config db) xss)

generateLang :: SparqlDatabaseLike db => Config -> db -> [Maybe MT.Text] -> IO Script
generateLang config db lang' = case lang' of
  [Just "R"] -> RLang.generate config db
  [Just "py"] -> Py3.generate config db
  [Just x] -> error ("The language " ++ show x ++ " is not supported")
  x -> error ("Bad SPARQL query:" ++ show x)

hsparql :: Query SelectQuery
hsparql = do
  i_    <- var
  lang_ <- var
  triple_ i_ PType OSource
  triple_ i_ PLang lang_
  distinct_
  selectVars [lang_]
