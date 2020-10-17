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
  , preprocess
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Data.Doc
import Morloc.Quasi
import Morloc.Pretty (prettyType)
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT

-- tree rewrites
preprocess :: ExprM Many -> MorlocMonad (ExprM Many)
preprocess = invertExprM

translate :: [Source] -> [ExprM One] -> MorlocMonad MDoc
translate srcs es = do
  -- translate sources
  includeDocs <- mapM
    translateSource
    (unique . catMaybes . map srcPath $ srcs)

  -- diagnostics
  liftIO . putDoc $ (vsep $ map prettyExprM es)

  -- translate each manifold tree, rooted on a call from nexus or another pool
  mDocs <- mapM translateManifold es

  return $ makePool includeDocs mDocs

letNamer :: Int -> MDoc 
letNamer i = "a" <> viaShow i

bndNamer :: Int -> MDoc
bndNamer i = "x" <> viaShow i

manNamer :: Int -> MDoc
manNamer i = "m" <> viaShow i

serialType :: CType
serialType = CType (VarT (TV (Just RLang) "character"))

-- For R, the type schema is the JSON representation of the type
typeSchema :: CType -> MorlocMonad MDoc
typeSchema c = do
  json <- jsontype2json <$> type2jsontype (unCType c)
  -- FIXME: Need to support single quotes inside strings
  return $ "'" <> json <> "'"

translateSource :: Path -> MorlocMonad MDoc
translateSource p = return $ "source(" <> dquotes (pretty p) <> ")"

-- break a call tree into manifolds
translateManifold :: ExprM One -> MorlocMonad MDoc
translateManifold m@(ManifoldM _ args _) = (vsep . punctuate line . fst) <$> f args m where
  f :: [Argument] -> ExprM One -> MorlocMonad ([MDoc], MDoc)
  f pargs m@(ManifoldM (metaId->i) args e) = do
    (ms', body) <- f args e
    let head = manNamer i <+> "<- function" <> tupled (map makeArgument args)
        mdoc = block 4 head body
        mname = manNamer i
    -- TODO: handle partials BEFORE translation
    call <- return $ case (splitArgs args pargs, nargsTypeM (typeOfExprM m)) of
      ((rs, []), _) -> mname <> tupled (map makeArgument rs) -- covers #1, #2 and #4
      (([], vs), _) -> mname
      ((rs, vs), _) -> makeLambda vs (mname <> tupled (map makeArgument (rs ++ vs))) -- covers #5
    return (mdoc : ms', call)

  f _ (PoolCallM t _ cmds args) = do
    let quotedCmds = map dquotes cmds
        callArgs = "list(" <> hsep (punctuate "," (drop 1 quotedCmds ++ map makeArgument args)) <> ")"
        call = ".morloc_foreign_call" <> tupled([head quotedCmds, callArgs, dquotes "_", dquotes "_"])
    return ([], call)

  f args (ForeignInterfaceM _ _) = MM.throwError . CallTheMonkeys $
    "Foreign interfaces should have been resolved before passed to the translators"
  f args (LetM i e1 e2) = do
    (ms1', e1') <- (f args) e1
    (ms2', e2') <- (f args) e2
    return (ms1' ++ ms2', letNamer i <+> "<-" <+> e1' <> line <> e2')
  f args (AppM (SrcM _ src) xs) = do
    (mss', xs') <- mapM (f args) xs |>> unzip
    return (concat mss', pretty (srcName src) <> tupled xs')
  f _ (SrcM t src) = return ([], pretty (srcName src))
  f args (LamM labmdaArgs e) = do
    (ms', e') <- f args e
    let vs = map (bndNamer . argId) labmdaArgs
    return (ms', "function" <> tupled vs <> "{" <+> e' <> "}")
  f _ (BndVarM _ i) = return ([], bndNamer i)
  f _ (LetVarM _ i) = return ([], letNamer i)
  f args (ListM t es) = do
    (mss', es') <- mapM (f args) es |>> unzip
    x' <- return $ case t of
      (Native (CType (ArrT _ [VarT et]))) -> case et of
        (TV _ "numeric") -> "c" <> tupled es'
        (TV _ "logical") -> "c" <> tupled es'
        (TV _ "character") -> "c" <> tupled es'
        _ -> "list" <> tupled es'
      _ -> "list" <> tupled es'
    return (concat mss', x')
  f args (TupleM _ es) = do
    (mss', es') <- mapM (f args) es |>> unzip
    return (concat mss', "list" <> tupled es')
  f args (RecordM c entries) = do
    (mss', es') <- mapM (f args . snd) entries |>> unzip
    let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
    return (concat mss', "list" <> tupled entries')
  f _ (LogM _ x) = return ([], if x then "TRUE" else "FALSE")
  f _ (NumM _ x) = return ([], viaShow x)
  f _ (StrM _ x) = return ([], dquotes $ pretty x)
  f _ (NullM _) = return ([], "NULL")
  f args (SerializeM _ e) = do
    (ms, e') <- f args e
    let (Native t) = typeOfExprM e
    schema <- typeSchema t
    return (ms, "rmorlocinternals::mlc_serialize" <> tupled [e', schema])
  f args (DeserializeM _ e) = do
    (ms, e') <- f args e
    let (Serial t) = typeOfExprM e
    schema <- typeSchema t
    return (ms, "rmorlocinternals::mlc_deserialize" <> tupled [e', schema])
  f args (ReturnM e) = do
    (ms, e') <- f args e
    return (ms, e')

makeLambda :: [Argument] -> MDoc -> MDoc
makeLambda args body = "function" <+> tupled (map makeArgument args) <> "{" <> body <> "}"

-- divide a list of arguments based on wheither they are in a second list
splitArgs :: [Argument] -> [Argument] -> ([Argument], [Argument])
splitArgs args1 args2 = partitionEithers $ map split args1 where
  split :: Argument -> Either Argument Argument
  split r = if elem r args2
            then Left r
            else Right r

makeArgument :: Argument -> MDoc
makeArgument (SerialArgument v c) = bndNamer v
makeArgument (NativeArgument v c) = bndNamer v
makeArgument (PassThroughArgument v) = bndNamer v

makePool :: [MDoc] -> [MDoc] -> MDoc
makePool sources manifolds = [idoc|#!/usr/bin/env Rscript

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

args <- as.list(commandArgs(trailingOnly=TRUE))
if(length(args) == 0){
  stop("Expected 1 or more arguments")
} else {
  cmdID <- args[[1]]
  f_str <- paste0("m", cmdID)
  if(exists(f_str)){
    f <- eval(parse(text=paste0("m", cmdID)))
    result <- do.call(f, args[-1])
    cat(result, "\n")
  } else {
    cat("Could not find manifold '", cmdID, "'\n", file=stderr())
  }
}
|]
