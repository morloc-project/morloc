{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Translator.R
Description : R translator
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Grammars.Translator.R
  ( translate
  , preprocess
  ) where

import Data.Text (Text)
import Morloc.CodeGenerator.Grammars.Common
import Morloc.CodeGenerator.Grammars.Translator.Imperative (LowerConfig(..), expandSerialize, expandDeserialize, lowerSerialExpr, lowerNativeExpr)
import qualified Morloc.CodeGenerator.Grammars.Translator.Printer.R as RP
import Morloc.CodeGenerator.Grammars.Translator.PseudoCode (pseudocodeSerialManifold)
import Morloc.CodeGenerator.Grammars.Translator.Syntax (IndexM, genericMakeSerialArg, genericMakeNativeArg)
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import Morloc.DataFiles as DF
import qualified Morloc.Language as ML
import Morloc.Monad (asks, gets, newIndex, runIndex)
import Morloc.Quasi

-- tree rewrites
preprocess :: SerialManifold -> MorlocMonad SerialManifold
preprocess = return . invertSerialManifold

translate :: [Source] -> [SerialManifold] -> MorlocMonad Script
translate srcs es = do
  -- translate sources
  includeDocs <-
    mapM
      translateSource
      (unique . mapMaybe srcPath $ srcs)

  homeDir <- asks configHome
  let dynlibDocs = [[idoc|dyn.load("#{pretty $ homeDir </> "lib" </> "librmorloc.so"}")|]]

  -- diagnostics
  debugLog (vsep (map pseudocodeSerialManifold es) <> "\n")

  -- translate each manifold tree, rooted on a call from nexus or another pool
  let mDocs = map translateSegment es

  let code = makePool includeDocs dynlibDocs mDocs
      exefile = ML.makeExecutablePoolName RLang

  return $
    Script
      { scriptBase = "pool"
      , scriptLang = RLang
      , scriptCode = "." :/ File exefile (Code . render $ code)
      , scriptMake = []
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

rLowerConfig :: LowerConfig IndexM
rLowerConfig = LowerConfig
  { lcSrcName = \src -> pretty (srcName src)
  , lcTypeOf = \_ -> return Nothing
  , lcSerialAstType = \_ -> return Nothing
  , lcDeserialAstType = \_ -> return Nothing
  , lcRawDeserialAstType = \_ -> return Nothing
  , lcTemplateArgs = \_ -> return Nothing
  , lcTypeMOf = \_ -> return Nothing
  , lcPackerName = \src -> pretty (srcName src)
  , lcUnpackerName = \src -> pretty (srcName src)
  , lcRecordAccessor = \_ _ record field -> record <> "$" <> field
  , lcDeserialRecordAccessor = \_ k v -> v <> "$" <> pretty k
  , lcTupleAccessor = \i v -> [idoc|#{v}[[#{pretty (i + 1)}]]|]  -- R uses 1-indexed
  , lcNewIndex = newIndex
  , lcPrintExpr = RP.printExpr
  , lcPrintStmt = RP.printStmt
  , lcEvalPattern = \t p xs -> return $ evaluatePattern t p xs
  , lcListConstructor = \v _ es -> case v of
      (FV _ (CV "integer"))   -> "c" <> tupled es
      (FV _ (CV "numeric"))   -> "c" <> tupled es
      (FV _ (CV "double"))    -> "c" <> tupled es
      (FV _ (CV "logical"))   -> "c" <> tupled es
      (FV _ (CV "character")) -> "c" <> tupled es
      _ -> "list" <> tupled es
  , lcTupleConstructor = \_ -> ((<>) "list" . tupled)
  , lcRecordConstructor = \_ _ _ _ rs -> return $ defaultValue
      { poolExpr = "list" <> tupled [pretty k <> "=" <> v | (k, v) <- rs] }
  , lcForeignCall = \socketFile mid args ->
      [idoc|morloc_foreign_call(#{makeSocketPath socketFile}, #{pretty mid}L, list#{tupled args})|]
  , lcRemoteCall = \socketFile mid res args -> do
      let resMem = pretty $ remoteResourcesMemory res
          resTime = pretty $ remoteResourcesTime res
          resCPU = pretty $ remoteResourcesThreads res
          resGPU = pretty $ remoteResourcesGpus res
          resources = [idoc|list(mem=#{resMem}L, time=#{resTime}L, cpus=#{resCPU}L, gpus=#{resGPU}L)|]
          call =
            "morloc_remote_call"
              <> tupled [pretty mid, dquotes socketFile, dquotes ".morloc-cache", resources, list args]
      return $ defaultValue {poolExpr = call}
  , lcMakeLet = \namer i _ e1 e2 -> return $ makeLet namer i e1 e2
  , lcReturn = \e -> "return(" <> e <> ")"
  , lcSerialize = \v s -> do
      (serialized, assignments) <- serialize v s
      return $ defaultValue {poolExpr = serialized, poolPriorLines = assignments}
  , lcDeserialize = \_ v s -> deserialize v s
  }
  where
    makeLet :: (Int -> MDoc) -> Int -> PoolDocs -> PoolDocs -> PoolDocs
    makeLet namer i (PoolDocs ms1' e1' rs1 pes1) (PoolDocs ms2' e2' rs2 pes2) =
      let rs = rs1 ++ [namer i <+> "<-" <+> e1'] ++ rs2
       in PoolDocs (ms1' <> ms2') e2' rs (pes1 <> pes2)

serialize :: MDoc -> SerialAST -> IndexM (MDoc, [MDoc])
serialize v s = do
  (expr, stmts) <- expandSerialize rLowerConfig v s
  return (RP.printExpr expr, map RP.printStmt stmts)

deserialize :: MDoc -> SerialAST -> IndexM (MDoc, [MDoc])
deserialize v s = do
  (expr, stmts) <- expandDeserialize rLowerConfig v s
  return (RP.printExpr expr, map RP.printStmt stmts)

makeSocketPath :: MDoc -> MDoc
makeSocketPath socketFileBasename = [idoc|paste0(global_state$tmpdir, "/", #{dquotes socketFileBasename})|]

translateSegment :: SerialManifold -> MDoc
translateSegment m0 =
  let e = runIndex 0 (foldWithSerialManifoldM fm m0)
   in vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e
  where
    fm =
      FoldWithManifoldM
        { opFoldWithSerialManifoldM = \_ (SerialManifold_ m _ form headForm x) ->
            return $ translateManifold makeFunction makeLambda m form (Just headForm) x
        , opFoldWithNativeManifoldM = \_ (NativeManifold_ m _ form x) ->
            return $ translateManifold makeFunction makeLambda m form Nothing x
        , opFoldWithSerialExprM = lowerSerialExpr rLowerConfig
        , opFoldWithNativeExprM = lowerNativeExpr rLowerConfig
        , opFoldWithSerialArgM = genericMakeSerialArg
        , opFoldWithNativeArgM = genericMakeNativeArg
        }

    makeFunction :: MDoc -> [Arg TypeM] -> [MDoc] -> MDoc -> Maybe HeadManifoldForm -> MDoc
    makeFunction mname args priorLines body headForm =
      block 4 def (vsep $ priorLines <> [body])
      where
        makeExt (Just HeadManifoldFormRemoteWorker) = "_remote"
        makeExt _ = ""

        def = mname <> makeExt headForm <+> "<-" <+> "function" <> tupled (map argNamer args)

    makeLambda :: MDoc -> [MDoc] -> [MDoc] -> MDoc
    makeLambda mname contextArgs boundArgs =
      let functionCall = mname <> tupled (contextArgs <> boundArgs)
       in "function" <+> tupled boundArgs <> "{" <> functionCall <> "}"

evaluatePattern :: TypeF -> Pattern -> [MDoc] -> MDoc
evaluatePattern _ (PatternText firstStr fragments) xs =
  "paste0"
    <> tupled (dquotes (pretty firstStr) : concat [[x, dquotes (pretty s)] | (x, s) <- zip xs fragments])
evaluatePattern _ (PatternStruct (ungroup -> [ss])) [m] =
  hcat (m : map writeBasicSelector ss)
evaluatePattern _ (PatternStruct (ungroup -> sss)) [m] =
  "list" <> tupled [hcat (m : map writeBasicSelector ss) | ss <- sss]
evaluatePattern t0 (PatternStruct s0) (m0 : xs0) =
  patternSetter makeTuple makeRecord accessTuple accessRecord m0 t0 s0 xs0
  where
    makeTuple _ xs = "list" <> tupled xs

    makeRecord (NamF _ _ _ rs) xs = "list" <> tupled [pretty k <+> "=" <+> x | (k, x) <- zip (map fst rs) xs]
    makeRecord _ _ = error "Incorrectly typed record setter"

    accessTuple _ m i = m <> "[[" <> pretty (i + 1) <> "]]"

    accessRecord (NamF _ _ _ _) d k = d <> "[[" <> dquotes (pretty k) <> "]]"
    accessRecord t _ _ = error $ "Invalid record type: " <> show t
evaluatePattern _ (PatternStruct _) [] = error "Unreachable empty pattern"

writeBasicSelector :: Either Int Text -> MDoc
writeBasicSelector (Right k) = "[[" <> dquotes (pretty k) <> "]]"
writeBasicSelector (Left i) = "[[" <> pretty (i + 1) <> "]]"

makePool :: [MDoc] -> [MDoc] -> [MDoc] -> MDoc
makePool sources dynlibs manifolds =
  format
    (DF.embededFileText (DF.poolTemplate RLang))
    "# <<<BREAK>>>"
    [vsep sources, vsep dynlibs, vsep manifolds]
