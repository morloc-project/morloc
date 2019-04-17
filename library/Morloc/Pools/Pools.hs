{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Pools.Pools
Description : Generate language-specific code
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pools.Pools
(
    generate
  , hsparql
) where

import Morloc.Types
import Morloc.Operators
import Morloc.Sparql
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified Morloc.Pools.Template.R as RLang
import qualified Morloc.Pools.Template.Python3 as Py3

import qualified Control.Monad as CM

generate :: SparqlDatabaseLike db => db -> MorlocMonad [Script]
generate db = (sparqlSelect "pools" hsparql db) >>= CM.mapM (generateLang db)

generateLang :: SparqlDatabaseLike db => db -> [Maybe MT.Text] -> MorlocMonad Script
generateLang db lang' = case lang' of
  [Just "R"] -> RLang.generate db
  [Just "py"] -> Py3.generate db
  [Just x] -> MM.throwError . GeneratorError $ "The language " <> x <> " is not supported"
  x -> MM.throwError . SparqlFail $ "Bad SPARQL query:" <> MT.show' x

-- | Find all languages that are used in the Morloc script
hsparql :: Query SelectQuery
hsparql = do
  i_    <- var
  lang_ <- var
  triple_ i_ PType OSource
  triple_ i_ PLang lang_
  distinct_
  selectVars [lang_]
