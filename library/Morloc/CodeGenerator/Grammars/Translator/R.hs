{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ViewPatterns #-}

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
import Morloc.Frontend.Lang.DefaultTypes as Def
import Morloc.Data.Doc
import Morloc.Quasi
import Morloc.Monad (asks, gets, Index, newIndex, runIndex)
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import Morloc.CodeGenerator.Grammars.Translator.PseudoCode (pseudocodeSerialManifold)

-- tree rewrites
preprocess :: SerialManifold -> MorlocMonad SerialManifold
preprocess = return . invertSerialManifold

translate :: [Source] -> [SerialManifold] -> MorlocMonad Script
translate srcs es = do
  -- translate sources
  includeDocs <- mapM
    translateSource
    (unique . mapMaybe srcPath $ srcs)

  -- diagnostics
  debugLog (vsep (map pseudocodeSerialManifold es) <> "\n")

  -- translate each manifold tree, rooted on a call from nexus or another pool
  let mDocs = map translateSegment es

  let code = makePool includeDocs mDocs
  let outfile = ML.makeExecutableName RLang "pool"

  return $ Script
    { scriptBase = "pool"
    , scriptLang = RLang
    , scriptCode = "." :/ File "pool.R" (Code . render $ code)
    , scriptMake = [SysExe outfile]
    }

debugLog :: Doc ann -> MorlocMonad ()
debugLog d = do
  verbosity <- gets stateVerbosity
  when (verbosity > 0) $ (liftIO . putDoc) d

svarNamer :: Int -> MDoc
svarNamer i = "s" <> viaShow i

nvarNamer :: Int -> MDoc
nvarNamer i = "n" <> viaShow i

argName :: Arg TypeM -> MDoc
argName (Arg i (Native _)) = nvarNamer i
argName (Arg i _) = svarNamer i

-- create a name for a manifold based on a unique id
manNamer :: Int -> MDoc
manNamer i = "m" <> viaShow i

translateSource :: Path -> MorlocMonad MDoc
translateSource p = do
  let p' = MT.stripPrefixIfPresent "./" (MT.pack p)
  return $ "source(" <> dquotes (pretty p') <> ")"

tupleKey :: Int -> MDoc -> MDoc
tupleKey i v = [idoc|#{v}[[#{pretty i}]]|]

recordAccess :: MDoc -> MDoc -> MDoc
recordAccess record field = record <> "$" <> field

serialize :: MDoc -> SerialAST -> Index (MDoc, [MDoc])
serialize v0 s0 = do
  (ms, v1) <- serialize' v0 s0
  let schema = typeSchema s0
  let v2 = "rmorlocinternals::mlc_serialize" <> tupled [v1, schema]
  return (v2, ms)
  where
    serialize' :: MDoc -> SerialAST -> Index ([MDoc], MDoc)
    serialize' v s
      | isSerializable s = return ([], v)
      | otherwise = construct v s

    construct :: MDoc -> SerialAST -> Index ([MDoc], MDoc)
    construct v (SerialPack _ (p, s)) = do
      let unpacker = pretty . srcName $ typePackerReverse p
      serialize' [idoc|#{unpacker}(#{v})|] s

    construct v (SerialList _ s) = do
      idx <- fmap pretty newIndex 
      let v' = "s" <> idx
      (before, x) <- serialize' [idoc|i#{idx}|] s
      let lst = block 4 [idoc|#{v'} <- lapply(#{v}, function(i#{idx})|] (vsep (before ++ [x])) <> ")"
      return ([lst], v')

    construct v (SerialTuple _ ss) = do
      (befores, ss') <- unzip <$> zipWithM (\i s -> construct (tupleKey i v) s) [1..] ss
      idx <- fmap pretty newIndex
      let v' = "s" <> idx
          x = [idoc|#{v'} <- list#{tupled ss'}|]
      return (concat befores ++ [x], v')

    construct v (SerialObject _ _ _ rs) = do
      (befores, ss') <- mapAndUnzipM (\(FV _ k, s) -> serialize' (recordAccess v (pretty k)) s) rs
      idx <- fmap pretty newIndex
      let v' = "s" <> idx
          entries = zipWith (\(FV _ key) val -> pretty key <> "=" <> val) (map fst rs) ss'
          decl = [idoc|#{v'} <- list#{tupled entries}|]
      return (concat befores ++ [decl], v')
    construct _ _ = undefined


deserialize :: MDoc -> SerialAST -> Index (MDoc, [MDoc])
deserialize v0 s0
  | isSerializable s0 =
      let schema = typeSchema s0
          deserializing = [idoc|rmorlocinternals::mlc_deserialize(#{v0}, #{schema})|]
      in return (deserializing, [])
  | otherwise = do
      idx <- fmap pretty newIndex 
      let schema = typeSchema s0
          rawvar = "s" <> idx
          deserializing = [idoc|#{rawvar} <- rmorlocinternals::mlc_deserialize(#{v0}, #{schema})|]
      (x, befores) <- check rawvar s0
      return (x, deserializing:befores)
  where
    check :: MDoc -> SerialAST -> Index (MDoc, [MDoc])
    check v s
      | isSerializable s = return (v, [])
      | otherwise = construct v s

    construct :: MDoc -> SerialAST -> Index (MDoc, [MDoc])
    construct v (SerialPack _ (p, s')) = do
      let packer = pretty . srcName $ typePackerForward p
      (x, before) <- check v s'
      let deserialized = [idoc|#{packer}(#{x})|]
      return (deserialized, before)

    construct v (SerialList _ s) = do
      idx <- fmap pretty newIndex
      let v' = "s" <> idx
      (x, before) <- check [idoc|i#{idx}|] s
      let lst = block 4 [idoc|#{v'} <- lapply(#{v}, function(i#{idx})|] (vsep (before ++ [x])) <> ")"
      return (v', [lst])

    construct v (SerialTuple _ ss) = do
      (ss', befores) <- unzip <$> zipWithM (\i s -> check (tupleKey i v) s) [1..] ss
      idx <- fmap pretty newIndex
      let v' = "s" <> idx
          x = [idoc|#{v'} <- list#{tupled ss'}|]
      return (v', concat befores ++ [x])

    construct v (SerialObject _ (FV _ constructor) _ rs) = do
      idx <- fmap pretty newIndex

      (ss', befores) <- mapAndUnzipM (\(FV _ k,s) -> check (recordAccess v (pretty k)) s) rs
      let v' = "s" <> idx
          entries = zipWith (\(FV _ key) val -> pretty key <> "=" <> val) (map fst rs) ss'
          decl = [idoc|#{v'} <- #{pretty constructor}#{tupled entries}|]
      return (v', concat befores ++ [decl])

    construct _ _ = undefined

translateSegment :: SerialManifold -> MDoc
translateSegment m0 =
  let e = runIndex 0 (foldSerialManifoldM fm m0)
  in vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e
  where
    fm = FoldManifoldM
      { opSerialManifoldM = makeSerialManifold
      , opNativeManifoldM = makeNativeManifold
      , opSerialExprM = makeSerialExpr
      , opNativeExprM = makeNativeExpr
      , opSerialArgM = makeSerialArg
      , opNativeArgM = makeNativeArg
      }

    makeSerialManifold :: SerialManifold_ PoolDocs -> Index PoolDocs
    makeSerialManifold (SerialManifold_ m _ form x) = translateManifold m form x

    makeNativeManifold :: NativeManifold_ PoolDocs -> Index PoolDocs
    makeNativeManifold (NativeManifold_ m _ form (_, x)) = translateManifold m form x

    makeSerialExpr :: SerialExpr_ PoolDocs PoolDocs PoolDocs PoolDocs PoolDocs -> Index PoolDocs
    makeSerialExpr (AppManS_ f _) = return f
    makeSerialExpr (AppPoolS_ (PoolCall _ cmds _) args) = return $ mergePoolDocs makePoolCall args
        where
          makePoolCall xs' =
            let quotedCmds = map dquotes cmds
                callArgs = "list(" <> hsep (punctuate "," (drop 1 quotedCmds ++ xs')) <> ")"
            in ".morloc_foreign_call" <> tupled [head quotedCmds, callArgs, dquotes "_", dquotes "_"]

    makeSerialExpr (ReturnS_ x) = return $ x {poolExpr = "return(" <> poolExpr x <> ")"}
    makeSerialExpr (SerialLetS_ i e1 e2) = return $ makeLet svarNamer i e1 e2
    makeSerialExpr (NativeLetS_ i (_, e1) e2) = return $ makeLet nvarNamer i e1 e2
    makeSerialExpr (LetVarS_ i) = return $ PoolDocs [] (svarNamer i) [] []
    makeSerialExpr (BndVarS_ i) = return $ PoolDocs [] (svarNamer i) [] []
    makeSerialExpr (SerializeS_ s e) = do
      (serialized, assignments) <- serialize (poolExpr e) s
      return $ e {poolExpr = serialized, poolPriorLines = poolPriorLines e <> assignments}

    makeNativeExpr :: NativeExpr_ PoolDocs PoolDocs PoolDocs PoolDocs PoolDocs -> Index PoolDocs
    makeNativeExpr (AppSrcN_      _ (pretty . srcName -> functionName) xs) =
        return $ mergePoolDocs ((<>) functionName . tupled) xs
    makeNativeExpr (AppManN_      _ call _) = return call
    makeNativeExpr (ReturnN_      _ x) =
        return $ x { poolExpr = "return(" <> poolExpr x <> ")" }
    makeNativeExpr (SerialLetN_     i x1 (_, x2)) = return $ makeLet svarNamer i x1 x2
    makeNativeExpr (NativeLetN_     i (_, x1) (_, x2)) = return $ makeLet nvarNamer i x1 x2
    makeNativeExpr (LetVarN_      _ i) = return $ PoolDocs [] (nvarNamer i) [] []
    makeNativeExpr (BndVarN_      _ i) = return $ PoolDocs [] (nvarNamer i) [] []
    makeNativeExpr (DeserializeN_ _ s x) = do
        (deserialized, assignments) <- deserialize (poolExpr x) s
        return $ x
          { poolExpr = deserialized
          , poolPriorLines = poolPriorLines x <> assignments
          }
    makeNativeExpr (AccN_         _ _ _ x k) =
        return $ x {poolExpr = recordAccess (poolExpr x) (pretty k)}
    makeNativeExpr (SrcN_         _ src) = return $ PoolDocs [] (pretty (srcName src)) [] []
    makeNativeExpr (ListN_        v _ xs) = return $ mergePoolDocs rlist xs where
       rlist es' = case v of
         (FV _ "numeric") -> "c" <> tupled es'
         (FV _ "double") -> "c" <> tupled es'
         (FV _ "logical") -> "c" <> tupled es'
         (FV _ "character") -> "c" <> tupled es'
         _ -> "list" <> tupled es'

    makeNativeExpr (TupleN_       _ xs) = return $ mergePoolDocs ((<>) "list" . tupled) (map snd xs)
    makeNativeExpr (RecordN_      _ _ _ rs)
        = return $ mergePoolDocs rlist (map (\(_, (_, x)) -> x) rs)
        where
            rlist es' =
                let entries' = zipWith (\(FV _ k) v -> pretty k <> "=" <> v) (map fst rs) es'
                in "list" <> tupled entries'

    makeNativeExpr (LogN_         _ v) = return $ PoolDocs [] (if v then "TRUE" else "FALSE") [] []
    makeNativeExpr (RealN_        _ v) = return $ PoolDocs [] (viaShow v) [] []
    makeNativeExpr (IntN_         _ v) = return $ PoolDocs [] (viaShow v) [] []
    makeNativeExpr (StrN_         _ v) = return $ PoolDocs [] (dquotes $ pretty v) [] []
    makeNativeExpr (NullN_        _)   = return $ PoolDocs [] "NULL" [] []

    makeSerialArg :: SerialArg_ PoolDocs PoolDocs -> Index PoolDocs
    makeSerialArg (SerialArgManifold_ x) = return x
    makeSerialArg (SerialArgExpr_ x) = return x

    makeNativeArg :: NativeArg_ PoolDocs PoolDocs -> Index PoolDocs
    makeNativeArg (NativeArgManifold_ x) = return x
    makeNativeArg (NativeArgExpr_ x) = return x

    translateManifold :: Int -> ManifoldForm TypeM -> PoolDocs -> Index PoolDocs
    translateManifold m form (PoolDocs completeManifolds body priorLines priorExprs) = do
      let args = manifoldArgs form
      let mname = manNamer m
          def = mname <+> "<-" <+> "function" <> tupled (map argName args)
          newManifold = block 4 def (vsep $ priorLines <> [body])
          call = case form of
            (ManifoldFull rs) -> mname <> tupled (map argName rs) -- covers #1, #2 and #4
            (ManifoldPass _) -> mname
            (ManifoldPart rs vs) -> makeLambda vs (mname <> tupled (map argName (rs ++ vs))) -- covers #5
      return $ PoolDocs
          { poolCompleteManifolds = newManifold : completeManifolds
          , poolExpr = call
          , poolPriorLines = []
          , poolPriorExprs = priorExprs
          }

    makeLet :: (Int -> MDoc) -> Int -> PoolDocs -> PoolDocs -> PoolDocs
    makeLet namer i (PoolDocs ms1' e1' rs1 pes1) (PoolDocs ms2' e2' rs2 pes2) =
      let rs = rs1 ++ [ namer i <+> "<-" <+> e1' ] ++ rs2
      in PoolDocs (ms1' <> ms2') e2' rs (pes1 <> pes2)


-- -- break a call tree into manifolds
-- translateManifold :: SerialManifold -> MorlocMonad MDoc
-- translateManifold = undefined
-- -- translateManifold m0@(ManifoldM _ form0 _) = do
-- --   MM.startCounter
-- --   e <- f (manifoldArgs form0) m0
-- --   return . vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e
-- --   where
-- --
-- --   f :: [Arg TypeM]
-- --     -> ExprM One
-- --     -> MorlocMonad PoolDocs
-- --   f = undefined
-- --   -- f _ (ManifoldM i form e) = do
-- --   --   let args = manifoldArgs form
-- --   --   (PoolDocs completeManifolds body priorLines priorExprs) <- f args e
-- --   --   let decl = manNamer i <+> "<- function" <> tupled (map argName args)
-- --   --       newManifold = block 4 decl (vsep $ priorLines ++ [body])
-- --   --       mname = manNamer i
-- --   --       call = case form of
-- --   --         (ManifoldFull rs) -> mname <> tupled (map argName rs) -- covers #1, #2 and #4
-- --   --         (ManifoldPass _) -> mname
-- --   --         (ManifoldPart rs vs) -> makeLambda vs (mname <> tupled (map argName (rs ++ vs))) -- covers #5
-- --   --   return $ PoolDocs
-- --   --       { poolCompleteManifolds = newManifold : completeManifolds
-- --   --       , poolExpr = call
-- --   --       , poolPriorLines = []
-- --   --       , poolPriorExprs = priorExprs
-- --   --       }
-- --   --
-- --   -- f _ (PoolCallM _ _ cmds args) = do
-- --   --   let quotedCmds = map dquotes cmds
-- --   --       callArgs = "list(" <> hsep (punctuate "," (drop 1 quotedCmds ++ map argName args)) <> ")"
-- --   --       call = ".morloc_foreign_call" <> tupled [head quotedCmds, callArgs, dquotes "_", dquotes "_"]
-- --   --   return $ PoolDocs [] call [] []
-- --   --
-- --   -- f _ (ForeignInterfaceM _ _ _) = MM.throwError . CallTheMonkeys $
-- --   --   "Foreign interfaces should have been resolved before passed to the translators"
-- --   --
-- --   -- f args (LetM i e1 e2) = do
-- --   --   (PoolDocs ms1' e1' rs1 pes1) <- f args e1
-- --   --   (PoolDocs ms2' e2' rs2 pes2) <- f args e2
-- --   --   let rs = rs1 ++ [ letNamer i <+> "<-" <+> e1' ] ++ rs2
-- --   --   return $ PoolDocs (ms1' <> ms2') e2' rs (pes1 <> pes2)
-- --   --
-- --   --
-- --   -- f args (AppM (SrcM _ src) xs)
-- --   --   = mergePoolDocs (\xs' -> pretty (srcName src) <> tupled xs')
-- --   --   <$> mapM (f args) xs
-- --   --
-- --   -- f args (AppM (PoolCallM _ _ cmds _) xs) = mapM (f args) xs |>> mergePoolDocs makePoolCall where
-- --   --   makePoolCall xs' =
-- --   --       let quotedCmds = map dquotes cmds
-- --   --           callArgs = "list(" <> hsep (punctuate "," (drop 1 quotedCmds ++ xs')) <> ")"
-- --   --       in ".morloc_foreign_call" <> tupled [head quotedCmds, callArgs, dquotes "_", dquotes "_"]
-- --   --
-- --   -- f _ (AppM f _) = error . MT.unpack . render $ "Can only apply functions, found:" <+> pretty f
-- --   --
-- --   -- f _ (SrcM _ src) = return (PoolDocs [] (pretty (srcName src)) [] [])
-- --   --
-- --   -- f _ (LamM manifoldArgs boundArgs body) = do
-- --   --   p <- f (manifoldArgs <> boundArgs) body
-- --   --   return $ p { poolExpr = makeLambda boundArgs (poolExpr p) }
-- --   -- -- f args (LamM lambdaArgs body) = do
-- --   -- --   p <- f args body
-- --   -- --   i <- MM.getCounter
-- --   -- --   let vs = map (bndNamer . argId) lambdaArgs
-- --   -- --       lambdaName = "mlc_lam_" <> pretty i
-- --   -- --       decl = lambdaName <+> "<- function" <> tupled vs
-- --   -- --       lambdaDef = block 4 decl (vsep $ poolPriorLines p <> [poolExpr p])
-- --   -- --       call = lambdaName
-- --   -- --   return $ p
-- --   -- --       { poolExpr = call
-- --   -- --       , poolPriorLines = []
-- --   -- --       , poolPriorExprs = [lambdaDef]
-- --   -- --       }
-- --   --
-- --   -- f _ (BndVarM _ i) = return $ PoolDocs [] (bndNamer i) [] []
-- --   --
-- --   -- f _ (LetVarM _ i) = return $ PoolDocs [] (letNamer i) [] []
-- --   --
-- --   -- f args (AccM e k) = do
-- --   --   p <- f args e
-- --   --   return $ p {poolExpr = poolExpr p <> "$" <> pretty k}
-- --   --
-- --   -- f args (ListM t es) = mapM (f args) es |>> mergePoolDocs rList
-- --   --   where
-- --   --       rList es' = case t of
-- --   --         (Native (AppP _ [VarP et])) -> case et of
-- --   --           (PV _ _ "numeric") -> "c" <> tupled es'
-- --   --           (PV _ _ "logical") -> "c" <> tupled es'
-- --   --           (PV _ _ "character") -> "c" <> tupled es'
-- --   --           _ -> "list" <> tupled es'
-- --   --         _ -> "list" <> tupled es'
-- --   --
-- --   -- f args (TupleM _ es) = mapM (f args) es |>> mergePoolDocs (\es' -> "list" <> tupled es')
-- --   --
-- --   -- f args (RecordM _ entries) = mapM (f args . snd) entries |>> mergePoolDocs rNamedList
-- --   --   where
-- --   --       rNamedList es' =
-- --   --           let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
-- --   --           in "list" <> tupled entries'
-- --   --
-- --   -- f _ (LogM _ x) = return $ PoolDocs [] (if x then "TRUE" else "FALSE") [] []
-- --   --
-- --   -- f _ (RealM _ x) = return $ PoolDocs [] (viaShow x) [] []
-- --   --
-- --   -- f _ (IntM _ x) = return $ PoolDocs [] (viaShow x) [] []
-- --   --
-- --   -- f _ (StrM _ x) = return $ PoolDocs [] (dquotes $ pretty x) [] []
-- --   --
-- --   -- f _ (NullM _) = return $ PoolDocs [] "NULL" [] []
-- --   --
-- --   -- f args (SerializeM s e) = do
-- --   --   p <- f args e
-- --   --   (serialized, assignments) <- serialize (poolExpr p) s
-- --   --   return $ p {poolExpr = serialized, poolPriorLines = poolPriorLines p <> assignments}
-- --   --
-- --   -- f args (DeserializeM s e) = do
-- --   --   p <- f args e
-- --   --   (deserialized, assignments) <- deserialize (poolExpr p) s
-- --   --   return $ p {poolExpr = deserialized, poolPriorLines = poolPriorLines p <> assignments}
-- --   --
-- --   -- f args (ReturnM e) = f args e
-- -- translateManifold _ = error "Every ExprM object must start with a Manifold term"


makeLambda :: [Arg TypeM] -> MDoc -> MDoc
makeLambda args body = "function" <+> tupled (map argName args) <> "{" <> body <> "}"

-- For R, the type schema is the JSON representation of the type
typeSchema :: SerialAST -> MDoc
typeSchema s0 = squotes $ jsontype2rjson (serialAstToJsonType s0) where
  serialAstToJsonType :: SerialAST -> JsonType
  serialAstToJsonType (SerialPack _ (_, s)) = serialAstToJsonType s
  serialAstToJsonType (SerialList _ s) = ArrJ "list" [serialAstToJsonType s]
  serialAstToJsonType (SerialTuple _ ss) = ArrJ "tuple" (map serialAstToJsonType ss)
  serialAstToJsonType (SerialObject _ (FV _ n) _ rs) = NamJ n (map (bimap (\(FV _ x) -> x) serialAstToJsonType) rs)
  serialAstToJsonType (SerialReal    (FV _ v)) = VarJ v
  serialAstToJsonType (SerialInt     (FV _ v)) = VarJ v
  serialAstToJsonType (SerialBool    (FV _ v)) = VarJ v
  serialAstToJsonType (SerialString  (FV _ v)) = VarJ v
  serialAstToJsonType (SerialNull    (FV _ v)) = VarJ v
  serialAstToJsonType (SerialUnknown (FV _ v)) = VarJ v -- the unknown type is the serialization type

  -- type2jsontype :: TypeP -> MorlocMonad JsonType
  -- type2jsontype (VarP (PV _ _ v)) = return $ VarJ v
  -- type2jsontype (AppP (VarP (PV _ (Just gt) v)) ts)
  --   -- see rmorlocinternal package where tuples, lists, and records are all
  --   -- represented by the R list type
  --   | Def.isTuple gt (length ts) = ArrJ "tuple" <$> mapM type2jsontype ts
  --   | gt == Def.listG = ArrJ "list" <$> mapM type2jsontype ts
  --   | otherwise = ArrJ v <$> mapM type2jsontype ts
  -- type2jsontype (AppP (VarP (PV _ _ v)) ts) = ArrJ v <$> mapM type2jsontype ts
  -- type2jsontype (AppP _ _) = MM.throwError . SerializationError $ "Invalid JSON type: complex AppP"
  -- type2jsontype (NamP _ (PV _ _ v) _ rs) = do
  --   ts <- mapM (type2jsontype . snd) rs
  --   return $ NamJ v (zip [k | (PV _ _ k, _) <- rs] ts)
  -- type2jsontype (UnkP _) = MM.throwError . SerializationError $ "Invalid JSON type: UnkT"
  -- type2jsontype (FunP _ _) = MM.throwError . SerializationError $ "Invalid JSON type: cannot serialize function"

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
  keys = map (dquotes . pretty . fst) rs
  vals = map (jsontype2rjson . snd) rs
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
            fails <<- e$message
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
  mlc_pool_cmdID <- args[[1]]
  mlc_pool_function_name <- paste0("m", mlc_pool_cmdID)
  if(exists(mlc_pool_function_name)){
    mlc_pool_function <- eval(parse(text=paste0("m", mlc_pool_cmdID)))
    result <- do.call(mlc_pool_function, args[-1])
    if(result != "null"){
        cat(result, "\n")
    }
  } else {
    cat("Could not find manifold '", mlc_pool_cmdID, "'\n", file=stderr())
  }
}
|]
