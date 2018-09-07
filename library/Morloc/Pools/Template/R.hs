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
    , gImport   = gImport'
    , gTrue     = "TRUE"
    , gFalse    = "FALSE"
    , gList     = gList'
    , gTuple    = gTuple'
    , gRecord   = gRecord'
    , gTrans    = transManifoldT
    , gCis      = cisManifoldT
    , gSource   = sourceManifoldT
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

    gImport' :: Doc -> Doc
    gImport' s = call' "source" [dquotes s]

transManifoldT :: TransManifoldDoc -> Doc
transManifoldT t = [idoc| XXX |]

sourceManifoldT :: SourceManifoldDoc -> Doc
sourceManifoldT s = [idoc|
# R: ${srcName s} source manifold

${srcCallId s} <- function(${commaSep (srcBndArgs s)}){
  ${run}$result
}
|] where
  run = "catchyEval" <> (
      tupled $
           [srcName s]
        ++ srcFunArgs s
        ++ [ ".pool=" <> dquotes (srcPool s)
           , ".name=" <> dquotes (srcCallId s)]
    )

cisManifoldT :: CisManifoldDoc -> Doc
cisManifoldT c = [idoc|
# R: ${cisName c} cis manifold

${cisCallId c} <- function(${commaSep (cisBndArgs c)}){
  ${run}$result
}
|] where
  run = "catchyEval" <> (
      tupled $
           [cisName c]
        ++ cisFunArgs c
        ++ [ ".pool=" <> dquotes (cisPool c)
           , ".name=" <> dquotes (cisCallId c)]
    )

main
  :: [Doc] -> [Manifold] -> SerialMap -> Doc
main srcs manifolds hash = [idoc|#!/usr/bin/env Rscript

${line <> vsep (map (gImport g) srcs) <> line}

catchyEval <- function(f, ..., .pool="_", .name="_"){
  fails <- ""
  isOK <- TRUE
  warns <- list()
  notes <- capture.output(
    {
      value <- withCallingHandlers(
        tryCatch(
          f(...),
          error = function(e) {
            fails <<- e$message;
            isOK <<- FALSE
          }
        ),
        warning = function(w){
          warns <<- append(warns, w$message)
          invokeRestart("muffleWarning")
        }
      )
    },
    type="message"
  )
  if(! isOK){
    msg <- sprintf("Error in %s::%s:\n %s", .pool, .name, x$errmsg)
    stop(msg) 
  }
  return(list(
    result = value,
    warns  = warns,
    notes  = noes
  ))
}

${makeSourceManifolds g hash manifolds}

${makeTransManifolds g hash manifolds}

${makeCisManifolds g hash manifolds}

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
