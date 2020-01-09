{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Template.R
Description : R language generation
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Grammars.Template.R (grammar) where

import Morloc.Data.Doc
import Morloc.Namespace
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Quasi
import Morloc.Pretty (prettyType)
import qualified Morloc.Data.Text as MT
import qualified Morloc.TypeChecker.Macro as MTM

grammar = Grammar {
      gLang        = gLang'
    , gSerialType  = gSerialType'
    , gAssign      = gAssign'
    , gCall        = gCall'
    , gFunction    = gFunction'
    , gId2Function = gId2Function'
    , gCurry       = gCurry'
    , gComment     = gComment'
    , gReturn      = gReturn'
    , gQuote       = gQuote'
    , gImport      = gImport'
    , gPrepImport  = gPrepImport'
    , gNull        = gNull'
    , gBool        = gBool'
    , gReal        = viaShow
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

gSerialType' :: Type
gSerialType' = VarT (TV (Just RLang) "character")

gAssign' :: GeneralAssignment -> MDoc
gAssign' ga = case gaType ga of
  (Just t) -> gaName ga <> " <- " <> gaValue ga <+> gComment' ["::" <+> t]
  Nothing  -> gaName ga <> " <- " <> gaValue ga

gCall' :: MDoc -> [MDoc] -> MDoc
gCall' n args = n <> tupled args

gFunction' :: GeneralFunction -> MDoc
gFunction' gf
  =  gComment' (gfComments gf)
  <> block 2
     (gfName gf <> " <- function" <> tupled (map snd (gfArgs gf)))
     (gIndent' (gfBody gf))

gId2Function' :: Integer -> MDoc
gId2Function' i = "m" <> (pretty (MT.show' i))

gCurry' :: MDoc -> [MDoc] -> Int -> MDoc
gCurry' f args i
  = "function" <> tupled rems <> braces (gCall' f (args ++ rems)) where
    rems = if i == 1
      then ["x"]
      else map (\i' -> pretty i') (take i ([1..] :: [Int]))

gComment' :: [MDoc] -> MDoc
gComment' ds = vsep (map (\d -> "#" <+> d) ds) <> line

gReturn' :: MDoc -> MDoc
gReturn' = id

gQuote' :: MDoc -> MDoc
gQuote' = dquotes

gNull' :: MDoc
gNull' = "NULL"

gBool' :: Bool -> MDoc
gBool' x = if x then "TRUE" else "FALSE" 

-- FIXME: make portable (replace "/" with the appropriate separator)
gImport' :: MDoc -> MDoc -> MDoc
gImport' _ srcpath = gCall' "source" [gQuote' srcpath]

gPrepImport' :: MT.Text -> MorlocMonad MDoc
gPrepImport' = return . pretty

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
  <> tupled ([x] ++ map (\v -> "`" <> l v <> "`" <> "=" <> r v) xs)

gCmdArgs' :: [MDoc]
gCmdArgs' = map (\i -> "args[[" <> int i <> "]]") [2..]

gShowType' :: Type -> MDoc
gShowType' = MTM.buildConcreteType mkfun mkrec where
  mkfun :: MDoc -> [MDoc] -> MDoc 
  mkfun _ _ = "FUNCTION!!!" -- FIXME: stub

  mkrec :: [(MDoc, MDoc)] -> MDoc
  mkrec _ = "RECORD!!!" -- FIXME: stub

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

#{vsep (pmPoolManifolds pm)}

args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  stop("Expected 1 or more arguments")
} else {
  cmdID <- args[[1]]
  #{(pmDispatchManifold pm) "cmdID" "result"}
  cat(result, "\n")
}
|]
