{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.R
Description : R translator
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Grammars.Translator.R
  ( 
    translate
  ) where

import Morloc.Namespace
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Data.Doc
import Morloc.Quasi


translate :: [Source] -> [Manifold] -> MorlocMonad MDoc
translate srcs ms = do 
  let includes = unique . catMaybes . map srcPath $ srcs

  includeDocs <- mapM translateSource includes
  mDocs <- mapM translateManifold ms
  dispatch <- makeDispatch ms
  return $ makeMain includeDocs mDocs dispatch

translateSource :: Path -> MorlocMonad MDoc
translateSource p = return $ "source(" <> dquotes (pretty p) <> ")"

translateManifold :: Manifold -> MorlocMonad MDoc
translateManifold (Manifold v args es) = return $ line <> block 4 head body where
  head = returnName v <+> "<- function" <> tupled (map makeArgument args)
  body = "BODY"

makeArgument :: Argument -> MDoc
makeArgument (PackedArgument v c) = pretty v
makeArgument (UnpackedArgument v c) = pretty v
makeArgument (PassThroughArgument v) = pretty v

returnName :: ReturnValue -> MDoc
returnName (PackedReturn v _) = pretty v
returnName (UnpackedReturn v _) = pretty v
returnName (PassThroughReturn v) = pretty v


makeDispatch :: [Manifold] -> MorlocMonad MDoc
makeDispatch _ = return "DISPATCH"

makeMain :: [MDoc] -> [MDoc] -> MDoc -> MDoc
makeMain sources manifolds dispatch = [idoc|#!/usr/bin/env Rscript

#{vsep sources}

.morloc_run <- function(f, args){
  fails <- ""
  isOK <- TRUE
  warns <- list()
  notes <- capture.output(
    {
      value <- withCallingHandlers(
        tryCatch(
          do.call(f, args),
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
  list(
    value = value,
    isOK  = isOK,
    fails = fails,
    warns = warns,
    notes = notes
  )
}

# dies on error, ignores warnings and messages
.morloc_try <- function(f, args, .log=stderr(), .pool="_", .name="_"){
  x <- .morloc_run(f=f, args=args)
  location <- sprintf("%s::%s", .pool, .name)
  if(! x$isOK){
    cat("** R errors in ", location, "\n", file=stderr())
    cat(x$fails, "\n", file=stderr())
    stop(1)
  }
  if(! is.null(.log)){
    lines = c()
    if(length(x$warns) > 0){
      cat("** R warnings in ", location, "\n", file=stderr())
      cat(paste(unlist(x$warns), sep="\n"), file=stderr())
    }
    if(length(x$notes) > 0){
      cat("** R messages in ", location, "\n", file=stderr())
      cat(paste(unlist(x$notes), sep="\n"), file=stderr())
    }
  }
  x$value
}

.morloc_unpack <- function(unpacker, x, .pool, .name){
  x <- .morloc_try(f=unpacker, args=list(as.character(x)), .pool=.pool, .name=.name)
  return(x)
}

.morloc_foreign_call <- function(cmd, args, .pool, .name){
  .morloc_try(f=system2, args=list(cmd, args=args, stdout=TRUE), .pool=.pool, .name=.name)
}

#{vsep manifolds}

args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  stop("Expected 1 or more arguments")
} else {
  cmdID <- args[[1]]
  #{dispatch}
  cat(result, "\n")
}
|]
