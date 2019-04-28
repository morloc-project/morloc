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

import Morloc.Global
import Morloc.Quasi
import Morloc.Pools.Common
import Morloc.Data.Doc hiding ((<$>))
import qualified Morloc.Config as MC
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT

generate :: SparqlDatabaseLike db => db -> MorlocMonad Script
generate db = makeGenerator g (defaultCodeGenerator g asImport main) db

asImport :: MT.Text -> MorlocMonad Doc
asImport s = return . text' $ s

g = Grammar {
      gLang        = "R"
    , gAssign      = assign'
    , gCall        = call'
    , gFunction    = function'
    , gComment     = comment'
    , gReturn      = return'
    , gQuote       = dquotes
    , gImport      = import'
    , gTrue        = "TRUE"
    , gFalse       = "FALSE"
    , gList        = list'
    , gTuple       = tuple'
    , gRecord      = record'
    , gIndent      = indent'
    , gTry         = try'
    , gUnpacker    = unpacker'
    , gForeignCall = foreignCall'
  } where

    assign' l r = l <> " <- " <> r 

    indent' = indent 4

    call' :: Doc -> [Doc] -> Doc
    call' n args = n <> tupled args

    function' :: Doc -> [Doc] -> Doc -> Doc
    function' name args body
      = name <> " <- function" <> tupled args <> braces (line <> indent' body <> line) <> line

    comment' :: Doc -> Doc
    comment' d = "# " <> d

    return' :: Doc -> Doc
    return' = id

    list' :: [Doc] -> Doc
    list' xs = "c" <> tupled xs

    tuple' :: [Doc] -> Doc
    tuple' xs = "list" <> tupled xs

    record' :: [(Doc,Doc)] -> Doc
    record' xs = "list" <> tupled (map (\(k,v) -> k <> "=" <> v) xs)

    -- FIXME: make portable (replace "/" with the appropriate separator)
    import' :: Doc -> Doc -> Doc
    import' _ srcpath = call' "source" [dquotes srcpath]

    try' :: TryDoc -> Doc
    try' t = call' ".morloc_try"
      $  ["f=" <> tryCmd t]
      ++ [("args=" <> tuple' (tryArgs t))]
      ++ [ ".name=" <> dquotes (tryMid t)
         , ".file=" <> dquotes (tryFile t)]

    unpacker' :: UnpackerDoc -> Doc
    unpacker' u = call' ".morloc_unpack"
      [ udUnpacker u
      , udValue u
      , ".name=" <> dquotes (udMid u)
      , ".pool=" <> dquotes (udFile u)
      ]

    foreignCall' :: ForeignCallDoc -> Doc
    foreignCall' f = call' ".morloc_foreign_call"
      [ dquotes (fcdForeignProg f)
      , dquotes (fcdForeignPool f)
      , dquotes (fcdMid f)
      , list' (fcdArgs f)
      , ".pool=" <> dquotes (fcdFile f)
      , ".name=" <> dquotes (fcdMid f)
      ]

toDict :: (a -> Doc) -> (a -> Doc) -> [a] -> Doc
toDict l r xs = "list" <> tupled (map (\x -> l x <> "=" <> r x) xs)

main
  :: [Doc] -> [Manifold] -> SerialMap -> MorlocMonad Doc
main srcs manifolds hash = do
  lib <- fmap text' $ MM.asks MC.configLibrary
  let sources = line <> vsep (map ((gImport g) lib) srcs)
  sourceManifolds <- makeSourceManifolds g hash manifolds
  cisManifolds <- makeCisManifolds g hash manifolds
  mids <- MM.mapM callIdToName manifolds
  let dispatchSerializerDict = toDict fst (getPacker hash . snd) (zip mids manifolds)
  return $ [idoc|#!/usr/bin/env Rscript

#{sources}

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
.morloc_try <- function(..., .log=stderr(), .pool="_", .name="_"){
  x <- .morloc_run(...)
  location <- sprintf("%s::%s", .pool, .name)
  if(! x$isOK){
    cat(sprintf("** R errors in %s\n", location), file=stderr())
    cat(x$fails, "\n", file=stderr())
    stop(1)
  }
  if(! is.null(.log)){
    lines = c()
    if(length(x$warns) > 0){
      cat(sprintf("** R warnings in %s\n", location), file=stderr())
      cat(paste(unlist(x$warns), sep="\n"), file=stderr())
    }
    if(length(x$notes) > 0){
      cat(sprintf("** R messages in %s\n", location), file=stderr())
      cat(paste(unlist(x$notes), sep="\n"), file=stderr())
    }
  }
  x$value
}

.morloc_unpack <- function(unpacker, x, ...){
  x <- .morloc_try(f=unpacker, args=list(as.character(x)), ...)  
  return(x)
}

.morloc_foreign_call <- function(cmd, args, .pool, .name){
  .morloc_try(f=system2, args=list(cmd, args=args, stdout=TRUE), .pool=.pool, .pool=.pool)
}

#{sourceManifolds}

#{cisManifolds}

dispatchSerializer <- #{dispatchSerializerDict}

args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  stop("Expected 1 or more arguments")
} else if(exists(args[[1]])){
  cmdstr <- args[[1]]
  cmd <- get(cmdstr)
  args <- as.list(args[-1, drop=FALSE])
  result <- if(class(cmd) == "function"){
    do.call(cmd, args)
  } else {
    cmd
  }
  serializer <- dispatchSerializer[[cmdstr]]
  cat(serializer(result), "\n")
} else {
  stop("Could not find function '", cmdstr, "'")
}
|]
