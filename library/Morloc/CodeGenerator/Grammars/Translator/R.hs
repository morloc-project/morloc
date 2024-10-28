{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.R
Description : R translator
Copyright   : (c) Zebulun Arendsee, 2016-2024
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
import Morloc.CodeGenerator.Serial (isSerializable)
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Data.Doc
import Morloc.DataFiles as DF
import Morloc.Quasi
import Morloc.Monad (gets, Index, newIndex, runIndex)
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
  let v2 = ".put_value(" <> "rmorlocinternals::mlc_serialize" <> tupled [v1, schema] <> ")"
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
      idx <- newIndex
      let v' = helperNamer idx
          idxStr = pretty idx
      (before, x) <- serialize' [idoc|i#{idxStr}|] s
      let lst = block 4 [idoc|#{v'} <- lapply(#{v}, function(i#{idxStr})|] (vsep (before ++ [x])) <> ")"
      return ([lst], v')

    construct v (SerialTuple _ ss) = do
      (befores, ss') <- unzip <$> zipWithM (\i s -> serialize' (tupleKey i v) s) [1..] ss
      v' <- helperNamer <$> newIndex
      let x = [idoc|#{v'} <- list#{tupled ss'}|]
      return (concat befores ++ [x], v')

    construct v (SerialObject _ _ _ rs) = do
      (befores, ss') <- mapAndUnzipM (\(key, s) -> serialize' (recordAccess v (pretty key)) s) rs
      v' <- helperNamer <$> newIndex
      let entries = zipWith (\key value -> pretty key <> "=" <> value) (map fst rs) ss'
          decl = [idoc|#{v'} <- list#{tupled entries}|]
      return (concat befores ++ [decl], v')

    construct _ _ = error "Unreachable"


deserialize :: MDoc -> SerialAST -> Index (MDoc, [MDoc])
deserialize v0 s0
  | isSerializable s0 =
      let schema = typeSchema s0
          deserializing = [idoc|rmorlocinternals::mlc_deserialize(.get_value(#{v0}), #{schema})|]
      in return (deserializing, [])
  | otherwise = do
      rawvar <- helperNamer <$> newIndex
      let schema = typeSchema s0
          deserializing = [idoc|#{rawvar} <- rmorlocinternals::mlc_deserialize(.get_value(#{v0}), #{schema})|]
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
      idx <- newIndex
      let v' = helperNamer idx
          idxStr = pretty idx
      (x, before) <- check [idoc|i#{idxStr}|] s
      let lst = block 4 [idoc|#{v'} <- lapply(#{v}, function(i#{idxStr})|] (vsep (before ++ [x])) <> ")"
      return (v', [lst])

    construct v (SerialTuple _ ss) = do
      (ss', befores) <- unzip <$> zipWithM (\i s -> check (tupleKey i v) s) [1..] ss
      v' <- helperNamer <$> newIndex
      let x = [idoc|#{v'} <- list#{tupled ss'}|]
      return (v', concat befores ++ [x])

    construct v (SerialObject _ (FV _ constructor) _ rs) = do
      (ss', befores) <- mapAndUnzipM (\(k, s) -> check (recordAccess v (pretty k)) s) rs
      v' <- helperNamer <$> newIndex
      let entries = zipWith (\key value -> pretty key <> "=" <> value) (map fst rs) ss'
          decl = [idoc|#{v'} <- #{pretty constructor}#{tupled entries}|]
      return (v', concat befores ++ [decl])

    construct _ _ = undefined

translateSegment :: SerialManifold -> MDoc
translateSegment m0 =
  let e = runIndex 0 (foldWithSerialManifoldM fm m0)
  in vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e
  where
    fm = FoldWithManifoldM
      { opFoldWithSerialManifoldM = makeSerialManifold
      , opFoldWithNativeManifoldM = makeNativeManifold
      , opFoldWithSerialExprM = makeSerialExpr
      , opFoldWithNativeExprM = makeNativeExpr
      , opFoldWithSerialArgM = makeSerialArg
      , opFoldWithNativeArgM = makeNativeArg
      }

    makeSerialManifold :: SerialManifold -> SerialManifold_ PoolDocs -> Index PoolDocs
    makeSerialManifold _ (SerialManifold_ m _ form x)
      = return $ translateManifold makeFunction makeLambda m form x

    makeNativeManifold :: NativeManifold -> NativeManifold_ PoolDocs -> Index PoolDocs
    makeNativeManifold _ (NativeManifold_ m _ form x)
      = return $ translateManifold makeFunction makeLambda m form x

    makeSerialExpr :: SerialExpr -> SerialExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) -> Index PoolDocs
    makeSerialExpr _ (ManS_ f) = return f
    makeSerialExpr _ (AppPoolS_ _ (PoolCall mid (Socket _ _ socketFile) args) _) = do
      let call = [idoc|.morloc_foreign_call(#{dquotes socketFile}, #{pretty mid}, list#{tupled (map argNamer args)})|]
      return $ PoolDocs
        { poolCompleteManifolds = []
        , poolExpr = call
        , poolPriorLines = []
        , poolPriorExprs = []
        }
    makeSerialExpr _ (ReturnS_ x) = return $ x {poolExpr = "return(" <> poolExpr x <> ")"}
    makeSerialExpr _ (SerialLetS_ i e1 e2) = return $ makeLet svarNamer i e1 e2
    makeSerialExpr _ (NativeLetS_ i e1 e2) = return $ makeLet nvarNamer i e1 e2
    makeSerialExpr _ (LetVarS_ _ i) = return $ PoolDocs [] (svarNamer i) [] []
    makeSerialExpr _ (BndVarS_ _ i) = return $ PoolDocs [] (svarNamer i) [] []
    makeSerialExpr _ (SerializeS_ s e) = do
      (serialized, assignments) <- serialize (poolExpr e) s
      return $ e {poolExpr = serialized, poolPriorLines = poolPriorLines e <> assignments}

    makeNativeExpr :: NativeExpr -> NativeExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) -> Index PoolDocs
    makeNativeExpr _ (AppSrcN_ _ (pretty . srcName -> functionName) _ xs) =
        return $ mergePoolDocs ((<>) functionName . tupled) (map snd xs)
    makeNativeExpr _ (ManN_ call) = return call
    makeNativeExpr _ (ReturnN_ x) =
        return $ x { poolExpr = "return(" <> poolExpr x <> ")" }
    makeNativeExpr _ (SerialLetN_ i x1 x2) = return $ makeLet svarNamer i x1 x2
    makeNativeExpr _ (NativeLetN_ i x1 x2) = return $ makeLet nvarNamer i x1 x2
    makeNativeExpr _ (LetVarN_ _ i) = return $ PoolDocs [] (nvarNamer i) [] []
    makeNativeExpr _ (BndVarN_ _ i) = return $ PoolDocs [] (nvarNamer i) [] []
    makeNativeExpr _ (DeserializeN_ _ s x) = do
        (deserialized, assignments) <- deserialize (poolExpr x) s
        return $ x
          { poolExpr = deserialized
          , poolPriorLines = poolPriorLines x <> assignments
          }
    makeNativeExpr _ (AccN_ _ _ x k) =
        return $ x {poolExpr = recordAccess (poolExpr x) (pretty k)}
    makeNativeExpr _ (SrcN_ _ src) = return $ PoolDocs [] (pretty (srcName src)) [] []
    makeNativeExpr _ (ListN_ v _ xs) = return $ mergePoolDocs rlist xs where
       rlist es' = case v of
         (FV _ (CV "numeric")) -> "c" <> tupled es'
         (FV _ (CV "double")) -> "c" <> tupled es'
         (FV _ (CV "logical")) -> "c" <> tupled es'
         (FV _ (CV "character")) -> "c" <> tupled es'
         _ -> "list" <> tupled es'

    makeNativeExpr _ (TupleN_ _ xs) = return $ mergePoolDocs ((<>) "list" . tupled) xs
    makeNativeExpr _ (RecordN_ _ _ _ rs)
        = return $ mergePoolDocs rlist (map snd rs)
        where
            rlist es' =
                let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst rs) es'
                in "list" <> tupled entries'

    makeNativeExpr _ (LogN_ _ v) = return $ PoolDocs [] (if v then "TRUE" else "FALSE") [] []
    makeNativeExpr _ (RealN_ _ v) = return $ PoolDocs [] (viaShow v) [] []
    makeNativeExpr _ (IntN_ _ v) = return $ PoolDocs [] (viaShow v) [] []
    makeNativeExpr _ (StrN_ _ v) = return $ PoolDocs [] (dquotes $ pretty v) [] []
    makeNativeExpr _ (NullN_ _) = return $ PoolDocs [] "NULL" [] []

    makeSerialArg :: SerialArg -> SerialArg_ PoolDocs PoolDocs -> Index (TypeS, PoolDocs)
    makeSerialArg sr (SerialArgManifold_ x) = return (typeSof sr, x)
    makeSerialArg sr (SerialArgExpr_ x) = return (typeSof sr, x)

    makeNativeArg :: NativeArg -> NativeArg_ PoolDocs PoolDocs -> Index (TypeM, PoolDocs)
    makeNativeArg nr (NativeArgManifold_ x) = return (typeMof nr, x)
    makeNativeArg nr (NativeArgExpr_ x) = return (typeMof nr, x)

    makeFunction :: MDoc -> [Arg TypeM] -> [MDoc] -> MDoc -> MDoc
    makeFunction mname args priorLines body =
      let def = mname <+> "<-" <+> "function" <> tupled (map argNamer args)
      in block 4 def (vsep $ priorLines <> [body])

    makeLambda :: [Arg TypeM] -> MDoc -> MDoc
    makeLambda args body = "function" <+> tupled (map argNamer args) <> "{" <> body <> "}"

    makeLet :: (Int -> MDoc) -> Int -> PoolDocs -> PoolDocs -> PoolDocs
    makeLet namer i (PoolDocs ms1' e1' rs1 pes1) (PoolDocs ms2' e2' rs2 pes2) =
      let rs = rs1 ++ [ namer i <+> "<-" <+> e1' ] ++ rs2
      in PoolDocs (ms1' <> ms2') e2' rs (pes1 <> pes2)

-- For R, the type schema is the JSON representation of the type
typeSchema :: SerialAST -> MDoc
typeSchema s0 = squotes $ jsontype2rjson (serialAstToJsonType s0) where
  serialAstToJsonType :: SerialAST -> JsonType
  serialAstToJsonType (SerialPack _ (_, s)) = serialAstToJsonType s
  serialAstToJsonType (SerialList _ s) = ArrJ (CV "list") [serialAstToJsonType s]
  serialAstToJsonType (SerialTuple _ ss) = ArrJ (CV "tuple") (map serialAstToJsonType ss)
  serialAstToJsonType (SerialObject _ (FV _ n) _ rs) = NamJ n (map (second serialAstToJsonType) rs)
  serialAstToJsonType (SerialReal    (FV _ v)) = VarJ v
  serialAstToJsonType (SerialInt     (FV _ v)) = VarJ v
  serialAstToJsonType (SerialBool    (FV _ v)) = VarJ v
  serialAstToJsonType (SerialString  (FV _ v)) = VarJ v
  serialAstToJsonType (SerialNull    (FV _ v)) = VarJ v
  serialAstToJsonType (SerialUnknown (FV _ v)) = VarJ v -- the unknown type is the serialization type

jsontype2rjson :: JsonType -> MDoc
jsontype2rjson (VarJ v) = dquotes (pretty v)
jsontype2rjson (ArrJ v ts) = "{" <> key <> ":" <> value <> "}" where
  key = dquotes (pretty v)
  value = encloseSep "[" "]" "," (map jsontype2rjson ts)
jsontype2rjson (NamJ objType rs) =
  case objType of
    (CV "data.frame") -> "{" <> dquotes "data.frame" <> ":" <> encloseSep "{" "}" "," rs' <> "}"
    (CV "record") -> "{" <> dquotes "record" <> ":" <> encloseSep "{" "}" "," rs' <> "}"
    _ -> encloseSep "{" "}" "," rs'
  where
  keys = map (dquotes . pretty . fst) rs
  values = map (jsontype2rjson . snd) rs
  rs' = zipWith (\key value -> key <> ":" <> value) keys values

makePool :: [MDoc] -> [MDoc] -> MDoc
makePool sources manifolds = format (DF.poolTemplate RLang) "# <<<BREAK>>>" [vsep sources, vsep manifolds]
