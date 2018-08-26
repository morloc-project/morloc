{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.R
Description : R language generation
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Language.R (generate) where

import Morloc.Quasi
import Morloc.Types
import qualified Morloc.Query as Q

import qualified Data.Text as DT 
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

generate :: SparqlEndPoint -> IO Script
generate e
  =   Script
  <$> pure "pool"
  <*> pure "R"
  <*> generateCode e

generateCode :: SparqlEndPoint -> IO DT.Text
generateCode e = fmap render (main <$> srcs' <*> names')
  where
    srcs' :: IO [Doc]
    -- srcs' = fmap (map srcParse) (Q.sourcesQ e)
    srcs' = return ["sourceX"]

    names' :: IO [Doc]
    names' = return ["foo"]


main :: [Doc] -> [Doc] -> Doc
main srcs names = [idoc|#!/usr/bin/env Rscript

${vsep (map sourceT srcs)}

${vsep (map functionT names)}

args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  stop("Expected 1 or more arguments")
} else if(exists(args[[1]])){
  x <- get(args[[1]])
  result <- if(class(x) == "function"){
    do.call(get(args[[1]]), as.list(args[-1, drop=FALSE]))
  } else {
    x
  }
  cat(packGeneric(result))
} else {
  stop("Could not find function '", args[[1]], "'")
}
|]

sourceT s = [idoc|source("${s}")|]

functionT s = [idoc|
# comment
mXX <- function(...){
  ${s}(...)
}
|]
