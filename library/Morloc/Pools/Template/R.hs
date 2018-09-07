{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Pools.Template.R
Description : R language generation
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pools.Template.R (generate) where

import Morloc.Types
import Morloc.Quasi
import Morloc.Pools.Common

import qualified Data.Text as DT 
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

generate = makeGenerator g (defaultCodeGenerator g text' main)

g = Grammar {
      gLang     = "R"
    , gCall     = call'
    , gFunction = function'
    , gComment  = comment'
    , gReturn   = return'
    , gQuote    = dquotes
    , gSource   = gSource'
    , gTrue     = "TRUE"
    , gFalse    = "FALSE"
    , gList     = gList'
    , gTuple    = gTuple'
    , gRecord   = gRecord'
    , gTrans    = transManifoldT
  } where
    call' :: Doc -> [Doc] -> Doc
    call' n args = n <> tupled args

    function' :: Doc -> [Doc] -> Doc -> Doc
    function' name args body
      = name <> " <- function" <> tupled args <> braces (line <> indent 2 body <> line) <> line

    comment' :: Doc -> Doc
    comment' d = "# " <> d

    return' :: Doc -> Doc
    return' = id

    gList' :: [Doc] -> Doc
    gList' xs = "c" <> tupled xs

    gTuple' :: [Doc] -> Doc
    gTuple' xs = "list" <> tupled xs

    gRecord' :: [(Doc,Doc)] -> Doc
    gRecord' xs = "list" <> tupled (map (\(k,v) -> k <> "=" <> v) xs)

    gSource' :: Doc -> Doc
    gSource' s = call' "source" [dquotes s]

transManifoldT :: TransManifoldDoc -> Doc
transManifoldT t = [idoc| XXX |]

main
  :: [Doc] -> [Manifold] -> SerialMap -> Doc
main srcs manifolds hash = [idoc|#!/usr/bin/env Rscript

${line <> vsep (map (gSource g) srcs) <> line}

${vsep (map (defaultManifold g hash) manifolds)}

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
  cat(packGeneric(result), "\n")
} else {
  stop("Could not find function '", args[[1]], "'")
}
|]
