{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Pools.Template.R
Description : R language generation
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pools.Template.R (generate) where

import Morloc.Namespace
import Morloc.Quasi
import Morloc.Pools.Common
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT

generate :: [Manifold] -> SerialMap -> MorlocMonad Script
generate = defaultCodeGenerator g asImport

asImport :: MT.Text -> MorlocMonad MDoc
asImport s = return . pretty $ s

g = Grammar {
      gLang        = gLang'
    , gSerialType  = gSerialType'
    , gAssign      = gAssign'
    , gCall        = gCall'
    , gFunction    = gFunction'
    , gId2Function = gId2Function'
    , gComment     = gComment'
    , gReturn      = gReturn'
    , gQuote       = gQuote'
    , gImport      = gImport'
    , gTrue        = gTrue'
    , gFalse       = gFalse'
    , gList        = gList'
    , gTuple       = gTuple'
    , gRecord      = gRecord'
    , gIndent      = gIndent'
    , gTry         = gTry'
    , gUnpacker    = gUnpacker'
    , gForeignCall = gForeignCall'
    , gSignature   = gSignature'
    , gSwitch      = gSwitch'
    , gCmdArgs     = gCmdArgs'
    , gShowType    = gShowType'
    , gMain        = gMain'
  }

gLang' :: Lang
gLang' = RLang

gSerialType' :: MType
gSerialType' = MConcType (MTypeMeta Nothing [] Nothing) "character" []

gAssign' :: GeneralAssignment -> MDoc
gAssign' ga = case gaType ga of
  (Just t) -> gaName ga <> " <- " <> gaValue ga <+> gComment' ("::" <+> t) 
  Nothing  -> gaName ga <> " <- " <> gaValue ga 

gCall' :: MDoc -> [MDoc] -> MDoc
gCall' n args = n <> tupled args

gFunction' :: GeneralFunction -> MDoc
gFunction' gf
  =  gComment' (gfComments gf)
  <> gfName gf <> " <- function"
  <> tupled (map snd (gfArgs gf))
  <> braces (line <> gIndent' (gfBody gf) <> line)

gId2Function' :: Integer -> MDoc
gId2Function' i = "m" <> (pretty (MT.show' i))

gComment' :: MDoc -> MDoc
gComment' d = "# " <> d

gReturn' :: MDoc -> MDoc
gReturn' = id

gQuote' :: MDoc -> MDoc
gQuote' = dquotes

gTrue' = "TRUE"
gFalse' = "FALSE"

-- FIXME: make portable (replace "/" with the appropriate separator)
gImport' :: MDoc -> MDoc -> MDoc
gImport' _ srcpath = gCall' "source" [gQuote' srcpath]

gList' :: [MDoc] -> MDoc
gList' xs = "c" <> tupled xs

gTuple' :: [MDoc] -> MDoc
gTuple' xs = "list" <> tupled xs

gRecord' :: [(MDoc, MDoc)] -> MDoc
gRecord' xs = "list" <> tupled (map (\(k,v) -> k <> "=" <> v) xs)

gIndent' :: MDoc -> MDoc
gIndent' = indent 4

gTry' :: TryDoc -> MDoc
gTry' t = gCall' ".morloc_try"
  $  ["f=" <> tryCmd t]
  ++ [("args=" <> gTuple' (tryArgs t))]
  ++ [ ".name=" <> dquotes (tryMid t)
     , ".file=" <> dquotes (tryFile t)]

gUnpacker' :: UnpackerDoc -> MDoc
gUnpacker' u = gCall' ".morloc_unpack"
  [ udUnpacker u
  , udValue u
  , ".name=" <> dquotes (udMid u)
  , ".pool=" <> dquotes (udFile u)
  ]

gSignature' :: GeneralFunction -> MDoc
gSignature' gf
  =   maybe "?" id (gfReturnType gf)
  <+> gfName gf
  <>  tupledNoFold (map (\(t,x) -> maybe "?" id t <+> x) (gfArgs gf))

gSwitch' :: (a -> MDoc) -> (a -> MDoc) -> [a] -> MDoc -> MDoc -> MDoc
gSwitch' l r xs x var
  =   var <+> "<-"
  <+> "switch"
  <> tupled ([x] ++ map (\x -> "`" <> l x <> "`" <> "=" <> r x) xs)

gCmdArgs' :: [MDoc]
gCmdArgs' = map (\i -> "args[[" <> int i <> "]]") [2..]

gShowType' :: MType -> MDoc
gShowType' = pretty

gForeignCall' :: ForeignCallDoc -> MDoc
gForeignCall' f = gCall' ".morloc_foreign_call" $
  [ "cmd=" <> hsep (take 1 (fcdCall f))
  , "args=" <> gList' ((drop 1 (fcdCall f)) ++ fcdArgs f)
  , ".pool=" <> dquotes (fcdFile f)
  , ".name=" <> dquotes (fcdMid f)
  ]

gMain' :: PoolMain -> MorlocMonad MDoc
gMain' pm = return [idoc|#!/usr/bin/env Rscript
  
#{line <> vsep (pmSources pm)}

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

#{vsep $ map (gFunction g) (pmPoolManifolds pm)}

args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  stop("Expected 1 or more arguments")
} else {
  cmdID <- args[[1]]
  #{(pmDispatchManifold pm) "cmdID" "result"}
  cat(result, "\n")
}
|]
