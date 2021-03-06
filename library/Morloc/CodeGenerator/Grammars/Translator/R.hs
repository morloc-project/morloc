{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.R
Description : R translator
Copyright   : (c) Zebulun Arendsee, 2021
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
import Morloc.CodeGenerator.Serial (isSerializable, prettySerialOne, serialAstToType)
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Data.Doc
import Morloc.Quasi
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

translateSource :: Path -> MorlocMonad MDoc
translateSource (Path p) = do
  let p' = MT.stripPrefixIfPresent "./" p
  return $ "source(" <> dquotes (pretty p') <> ")"

tupleKey :: Int -> MDoc -> MDoc
tupleKey i v = [idoc|#{v}[[#{pretty i}]]|]

recordAccess :: MDoc -> MDoc -> MDoc
recordAccess record field = record <> "$" <> field

serialize :: MDoc -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
serialize v0 s0 = do
  (ms, v1) <- serialize' v0 s0
  t <- serialAstToType s0
  schema <- typeSchema t
  let v2 = "rmorlocinternals::mlc_serialize" <> tupled [v1, schema]
  return (v2, ms)
  where
    serialize' :: MDoc -> SerialAST One -> MorlocMonad ([MDoc], MDoc)
    serialize' v s
      | isSerializable s = return ([], v)
      | otherwise = construct v s

    construct :: MDoc -> SerialAST One -> MorlocMonad ([MDoc], MDoc)
    construct v (SerialPack _ (One (p, s))) = do
      unpacker <- case typePackerReverse p of
        [] -> MM.throwError . SerializationError $ "No unpacker found"
        (src:_) -> return . pretty . srcName $ src
      serialize' [idoc|#{unpacker}(#{v})|] s

    construct v (SerialList s) = do
      idx <- fmap pretty $ MM.getCounter
      let v' = "s" <> idx
      (before, x) <- serialize' [idoc|i#{idx}|] s
      let lst = block 4 [idoc|#{v'} <- lapply(#{v}, function(i#{idx})|] (vsep (before ++ [x])) <> ")"
      return ([lst], v')

    construct v (SerialTuple ss) = do
      (befores, ss') <- fmap unzip $ zipWithM (\i s -> construct (tupleKey i v) s) [1..] ss
      idx <- fmap pretty $ MM.getCounter
      let v' = "s" <> idx
          x = [idoc|#{v'} <- list#{tupled ss'}|]
      return (concat befores ++ [x], v');

    construct v (SerialObject _ _ _ rs) = do
      (befores, ss') <- fmap unzip $ mapM (\(PV _ _ k,s) -> serialize' (recordAccess v (pretty k)) s) rs
      idx <- fmap pretty $ MM.getCounter
      let v' = "s" <> idx
          entries = zipWith (\(PV _ _ key) val -> pretty key <> "=" <> val) (map fst rs) ss'
          decl = [idoc|#{v'} <- list#{tupled entries};|]
      return (concat befores ++ [decl], v');

    construct _ s = MM.throwError . SerializationError . render
      $ "construct: " <> prettySerialOne s


deserialize :: MDoc -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
deserialize v0 s0
  | isSerializable s0 = do
      t <- serialAstToType s0
      schema <- typeSchema t
      let deserializing = [idoc|rmorlocinternals::mlc_deserialize(#{v0}, #{schema});|]
      return (deserializing, [])
  | otherwise = do
      idx <- fmap pretty $ MM.getCounter
      t <- serialAstToType s0
      schema <- typeSchema t
      let rawvar = "s" <> idx
          deserializing = [idoc|#{rawvar} <- rmorlocinternals::mlc_deserialize(#{v0}, #{schema});|]
      (x, befores) <- check rawvar s0
      return (x, deserializing:befores)
  where
    check :: MDoc -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
    check v s
      | isSerializable s = return (v, [])
      | otherwise = construct v s

    construct :: MDoc -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
    construct v (SerialPack _ (One (p, s'))) = do
      packer <- case typePackerForward p of
        [] -> MM.throwError . SerializationError $ "No packer found"
        (x:_) -> return . pretty . srcName $ x
      (x, before) <- check v s'
      let deserialized = [idoc|#{packer}(#{x})|]
      return (deserialized, before)

    construct v (SerialList s) = do
      idx <- fmap pretty $ MM.getCounter
      let v' = "s" <> idx
      (x, before) <- check [idoc|i#{idx}|] s
      let lst = block 4 [idoc|#{v'} <- lapply(#{v}, function(i#{idx})|] (vsep (before ++ [x])) <> ")"
      return (v', [lst])

    construct v (SerialTuple ss) = do
      (ss', befores) <- fmap unzip $ zipWithM (\i s -> check (tupleKey i v) s) [1..] ss
      idx <- fmap pretty $ MM.getCounter
      let v' = "s" <> idx
          x = [idoc|#{v'} <- list#{tupled ss'};|]
      return (v', concat befores ++ [x]);

    construct v (SerialObject _ (PV _ _ constructor) _ rs) = do
      idx <- fmap pretty $ MM.getCounter
      (ss', befores) <- fmap unzip $ mapM (\(PV _ _ k,s) -> check (recordAccess v (pretty k)) s) rs
      let v' = "s" <> idx
          entries = zipWith (\(PV _ _ key) val -> pretty key <> "=" <> val) (map fst rs) ss'
          decl = [idoc|#{v'} <- #{pretty constructor}#{tupled entries};|]
      return (v', concat befores ++ [decl]);

    construct _ s = MM.throwError . SerializationError . render
      $ "deserializeDescend: " <> prettySerialOne s



-- break a call tree into manifolds
translateManifold :: ExprM One -> MorlocMonad MDoc
translateManifold m0@(ManifoldM _ args0 _) = do
  MM.startCounter
  (vsep . punctuate line . (\(x,_,_)->x)) <$> f args0 m0
  where


  f :: [Argument] -> ExprM One -> MorlocMonad ([MDoc], MDoc, [MDoc])
  f pargs m@(ManifoldM (metaId->i) args e) = do
    (ms', body, rs') <- f args e
    let decl = manNamer i <+> "<- function" <> tupled (map makeArgument args)
        mdoc = block 4 decl (vsep $ rs' ++ [body])
        mname = manNamer i
    -- TODO: handle partials BEFORE translation
    call <- return $ case (splitArgs args pargs, nargsTypeM (typeOfExprM m)) of
      ((rs, []), _) -> mname <> tupled (map makeArgument rs) -- covers #1, #2 and #4
      (([], _ ), _) -> mname
      ((rs, vs), _) -> makeLambda vs (mname <> tupled (map makeArgument (rs ++ vs))) -- covers #5
    return (mdoc : ms', call, [])

  f _ (PoolCallM _ _ cmds args) = do
    let quotedCmds = map dquotes cmds
        callArgs = "list(" <> hsep (punctuate "," (drop 1 quotedCmds ++ map makeArgument args)) <> ")"
        call = ".morloc_foreign_call" <> tupled([head quotedCmds, callArgs, dquotes "_", dquotes "_"])
    return ([], call, [])

  f _ (ForeignInterfaceM _ _) = MM.throwError . CallTheMonkeys $
    "Foreign interfaces should have been resolved before passed to the translators"

  f args (LetM i e1 e2) = do
    (ms1', e1', rs1) <- (f args) e1
    (ms2', e2', rs2) <- (f args) e2
    let rs = rs1 ++ [ letNamer i <+> "<-" <+> e1' ] ++ rs2
    return (ms1' ++ ms2', e2', rs)

  f args (AppM (SrcM _ src) xs) = do
    (mss', xs', rss') <- mapM (f args) xs |>> unzip3
    return (concat mss', pretty (srcName src) <> tupled xs', concat rss')

  f _ (AppM _ _) = error "Can only apply functions"

  f _ (SrcM _ src) = return ([], pretty (srcName src), [])

  f args (LamM labmdaArgs e) = do
    (ms', e', rs) <- f args e
    let vs = map (bndNamer . argId) labmdaArgs
    return (ms', "function" <> tupled vs <> "{" <+> e' <> "}", rs)

  f _ (BndVarM _ i) = return ([], bndNamer i, [])

  f _ (LetVarM _ i) = return ([], letNamer i, [])

  f args (AccM e k) = do
    (ms, e', ps) <- f args e
    return (ms, e' <> "$" <> pretty k, ps)

  f args (ListM t es) = do
    (mss', es', rss) <- mapM (f args) es |>> unzip3
    x' <- return $ case t of
      (Native (ArrP _ [VarP et])) -> case et of
        (PV _ _ "numeric") -> "c" <> tupled es'
        (PV _ _ "logical") -> "c" <> tupled es'
        (PV _ _ "character") -> "c" <> tupled es'
        _ -> "list" <> tupled es'
      _ -> "list" <> tupled es'
    return (concat mss', x', concat rss)

  f args (TupleM _ es) = do
    (mss', es', rss) <- mapM (f args) es |>> unzip3
    return (concat mss', "list" <> tupled es', concat rss)

  f args (RecordM _ entries) = do
    (mss', es', rss) <- mapM (f args . snd) entries |>> unzip3
    let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
    return (concat mss', "list" <> tupled entries', concat rss)

  f _ (LogM _ x) = return ([], if x then "TRUE" else "FALSE", [])

  f _ (NumM _ x) = return ([], viaShow x, [])

  f _ (StrM _ x) = return ([], dquotes $ pretty x, [])

  f _ (NullM _) = return ([], "NULL", [])

  f args (SerializeM s e) = do
    (ms, e', rs1) <- f args e
    (serialized, rs2) <- serialize e' s
    return (ms, serialized, rs1 ++ rs2)

  f args (DeserializeM s e) = do
    (ms, e', rs1) <- f args e
    (deserialized, rs2) <- deserialize e' s
    return (ms, deserialized, rs1 ++ rs2)

  f args (ReturnM e) = do
    (ms, e', rs) <- f args e
    return (ms, e', rs)
translateManifold _ = error "Every ExprM object must start with a Manifold term"

makeLambda :: [Argument] -> MDoc -> MDoc
makeLambda args body = "function" <+> tupled (map makeArgument args) <> "{" <> body <> "}"

makeArgument :: Argument -> MDoc
makeArgument (SerialArgument v _) = bndNamer v
makeArgument (NativeArgument v _) = bndNamer v
makeArgument (PassThroughArgument v) = bndNamer v

-- For R, the type schema is the JSON representation of the type
typeSchema :: TypeP -> MorlocMonad MDoc
typeSchema t = do
  json <- jsontype2rjson <$> type2jsontype t
  -- FIXME: Need to support single quotes inside strings
  return $ "'" <> json <> "'"

jsontype2rjson :: JsonType -> MDoc
jsontype2rjson (VarJ v) = dquotes (pretty v)
jsontype2rjson (ArrJ v ts) = "{" <> key <> ":" <> val <> "}" where
  key = dquotes (pretty v)
  val = encloseSep "[" "]" "," (map jsontype2rjson ts)
jsontype2rjson (NamJ objType rs) =
  case objType of
    "data.frame" -> "{" <> dquotes "data.frame" <> ":" <> encloseSep "{" "}" "," rs' <> "}"
    "record" -> "{" <> dquotes "record" <> ":" <> encloseSep "{" "}" "," rs' <> "}"
    _ -> encloseSep "{" "}" "," rs'
  where
  keys = map (dquotes . pretty) (map fst rs) 
  vals = map jsontype2rjson (map snd rs)
  rs' = zipWith (\key val -> key <> ":" <> val) keys vals

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
