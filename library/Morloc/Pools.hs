{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Pools
Description : Generate language-specific code
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pools (generate) where

import Morloc.Types
import Morloc.Operators
import Morloc.Query as Q
import qualified Data.Text as DT

import qualified Morloc.Language.R as RLang

generate :: SparqlEndPoint -> IO [Script]
generate ep = Q.languagesQ ep >>= foo' where 
  foo' :: [[Maybe DT.Text]] -> IO [Script]
  foo' xss = sequence (map (generateLang ep) xss)

generateLang :: SparqlEndPoint -> [Maybe DT.Text] -> IO Script
generateLang e lang = case lang of
  [Just "R"] -> RLang.generate e
  [Just x] -> error ("The language '" ++ DT.unpack x ++ "' is not supported")
  x -> error ("Bad SPARQL query:" ++ show x)
