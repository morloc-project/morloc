{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Translator.Python3
Description : Python3 translator
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Grammars.Translator.Python3
  ( translate
  , preprocess
  ) where

import qualified Data.Char as DC
import Data.Text (Text)
import Morloc.CodeGenerator.Grammars.Common
import Morloc.CodeGenerator.Grammars.Translator.Imperative (LowerConfig(..), expandSerialize, expandDeserialize, defaultFoldRules)
import qualified Morloc.CodeGenerator.Grammars.Translator.Printer.Python3 as PP
import Morloc.CodeGenerator.Grammars.Translator.PseudoCode (pseudocodeSerialManifold)
import Morloc.CodeGenerator.Grammars.Translator.Syntax (IndexM)
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Config as MC
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import Morloc.DataFiles as DF
import qualified Morloc.Language as ML
import Morloc.Monad (asks, gets, newIndex, runIndex)
import qualified Morloc.Monad as MM
import Morloc.Quasi
import qualified System.FilePath as SF

-- tree rewrites
preprocess :: SerialManifold -> MorlocMonad SerialManifold
preprocess = return . invertSerialManifold

translate :: [Source] -> [SerialManifold] -> MorlocMonad Script
translate srcs es = do
  -- setup library paths
  lib <- MT.pack <$> asks MC.configLibrary

  home <- pretty <$> asks MC.configHome
  let opt = home <> "/opt"

  -- translate sources
  includeDocs <-
    mapM
      translateSource
      (unique . mapMaybe srcPath $ srcs)

  -- diagnostics
  debugLog (vsep (map pseudocodeSerialManifold es) <> "\n")

  -- translate each manifold tree, rooted on a call from nexus or another pool
  let mDocs = map (translateSegment (qualifiedSrcName lib)) es

  -- make code for dispatching to manifolds
  let dispatch = makeDispatch es

  let code = makePool [opt, pretty lib] includeDocs mDocs dispatch
  let exefile = ML.makeExecutablePoolName Python3Lang

  return $
    Script
      { scriptBase = "pool"
      , scriptLang = Python3Lang
      , scriptCode = "." :/ File exefile (Code . render $ code)
      , scriptMake = []
      }
  where
    qualifiedSrcName :: Text -> Source -> MDoc
    qualifiedSrcName lib src = case srcPath src of
      Nothing -> pretty $ srcName src
      (Just path) -> makeNamespace lib path <> "." <> pretty (srcName src)

debugLog :: Doc ann -> MorlocMonad ()
debugLog d = do
  verbosity <- gets stateVerbosity
  when (verbosity > 0) $ (liftIO . putDoc) d

makeNamespace :: Text -> Path -> MDoc
makeNamespace lib =
  pretty
    . MT.liftToText (map DC.toLower)
    . MT.replace "/" "_"
    . MT.replace "-" "_"
    . MT.replace "." "_"
    . MT.stripPrefixIfPresent "/" -- strip the leading slash (if present)
    . MT.stripPrefixIfPresent "./" -- no path if relative to here
    . MT.stripPrefixIfPresent lib -- make the path relative to the library
    . MT.liftToText SF.dropExtensions
    . MT.pack

translateSource :: Path -> MorlocMonad MDoc
translateSource s = do
  lib <- MT.pack <$> asks configLibrary

  let importStr =
        pretty
          . MT.liftToText (map DC.toLower)
          . MT.replace "/" "."
          . MT.stripPrefixIfPresent "/" -- strip the leading slash (if present)
          . MT.stripPrefixIfPresent "./" -- no path if relative to here
          . MT.stripPrefixIfPresent lib -- make the path relative to the library
          . MT.liftToText SF.dropExtensions
          $ MT.pack s

  return $ makeNamespace lib s <+> "=" <+> "importlib.import_module(" <> dquotes importStr <> ")"

tupleKey :: Int -> MDoc -> MDoc
tupleKey i v = [idoc|#{v}[#{pretty i}]|]

selectAccessor :: NamType -> CVar -> (MDoc -> MDoc -> MDoc)
selectAccessor NamTable (CV "dict") = recordAccess
selectAccessor NamRecord _ = recordAccess
selectAccessor NamTable _ = objectAccess
selectAccessor NamObject _ = objectAccess

recordAccess :: MDoc -> MDoc -> MDoc
recordAccess record field = record <> "[" <> dquotes field <> "]"

objectAccess :: MDoc -> MDoc -> MDoc
objectAccess object field = object <> "." <> field

pythonLowerConfig :: (Source -> MDoc) -> LowerConfig IndexM
pythonLowerConfig makeSrcName = LowerConfig
  { lcSrcName = makeSrcName
  , lcTypeOf = \_ -> return Nothing
  , lcSerialAstType = \_ -> return Nothing
  , lcDeserialAstType = \_ -> return Nothing
  , lcRawDeserialAstType = \_ -> return Nothing
  , lcTemplateArgs = \_ -> return Nothing
  , lcTypeMOf = \_ -> return Nothing
  , lcPackerName = makeSrcName
  , lcUnpackerName = makeSrcName
  , lcRecordAccessor = \namType constructor -> selectAccessor namType constructor
  , lcDeserialRecordAccessor = \_ k v -> recordAccess v (pretty k)
  , lcTupleAccessor = tupleKey
  , lcNewIndex = newIndex
  , lcPrintExpr = PP.printExpr
  , lcPrintStmt = PP.printStmt
  , lcEvalPattern = \t p xs -> return $ evaluatePattern t p xs
  , lcListConstructor = \_ _ es -> list es
  , lcTupleConstructor = \_ -> tupled
  , lcRecordConstructor = \_ _ _ _ rs -> return $ defaultValue
      { poolExpr = "OrderedDict" <> tupled [pretty k <> "=" <> v | (k, v) <- rs] }
  , lcForeignCall = \socketFile mid args ->
      "morloc.foreign_call" <> tupled [makeSocketPath socketFile, pretty mid, list args]
  , lcRemoteCall = \socketFile mid res args -> do
      let resMem = pretty $ remoteResourcesMemory res
          resTime = pretty $ remoteResourcesTime res
          resCPU = pretty $ remoteResourcesThreads res
          resGPU = pretty $ remoteResourcesGpus res
          resStruct = "struct.pack" <> tupled [squotes "iiii", resMem, resTime, resCPU, resGPU]
          call =
            "morloc.remote_call"
              <> tupled [pretty mid, dquotes socketFile, dquotes ".morloc-cache", resStruct, list args]
      return $ defaultValue {poolExpr = call}
  , lcMakeLet = \namer i _ e1 e2 -> return $ makeLet namer i e1 e2
  , lcReturn = \e -> "return(" <> e <> ")"
  , lcSerialize = \v s -> do
      (serialized, assignments) <- serialize makeSrcName v s
      return $ defaultValue {poolExpr = serialized, poolPriorLines = assignments}
  , lcDeserialize = \_ v s -> deserialize makeSrcName v s
  , lcMakeFunction = \mname args _ priorLines body headForm ->
      let makeExt (Just HeadManifoldFormRemoteWorker) = "_remote"
          makeExt _ = ""
          def = "def" <+> mname <> makeExt headForm <> tupled (map argNamer args) <> ":"
          tryCatch [] = ""
          tryCatch xs =
            let tryBlock = nest 4 (vsep ("try:" : xs))
                exceptBlock =
                  nest 4 ( vsep
                    [ "except Exception as e:"
                    , [idoc|raise RuntimeError(f"Error (Python daemon in #{mname}):\n{e!s}")|]
                    ])
             in vsep [tryBlock, exceptBlock]
       in return . Just $ nest 4 (vsep [def, tryCatch priorLines, body])
  , lcMakeLambda = \mname contextArgs _ -> "functools.partial" <> tupled (mname : contextArgs)
  }
  where
    makeLet :: (Int -> MDoc) -> Int -> PoolDocs -> PoolDocs -> PoolDocs
    makeLet namer i (PoolDocs ms1' e1' rs1 pes1) (PoolDocs ms2' e2' rs2 pes2) =
      let rs = rs1 ++ [namer i <+> "=" <+> e1'] ++ rs2
       in PoolDocs (ms1' <> ms2') e2' rs (pes1 <> pes2)

serialize :: (Source -> MDoc) -> MDoc -> SerialAST -> IndexM (MDoc, [MDoc])
serialize makeSrcName v s = do
  (expr, stmts) <- expandSerialize (pythonLowerConfig makeSrcName) v s
  return (PP.printExpr expr, map PP.printStmt stmts)

deserialize :: (Source -> MDoc) -> MDoc -> SerialAST -> IndexM (MDoc, [MDoc])
deserialize makeSrcName v s = do
  (expr, stmts) <- expandDeserialize (pythonLowerConfig makeSrcName) v s
  return (PP.printExpr expr, map PP.printStmt stmts)

makeSocketPath :: MDoc -> MDoc
makeSocketPath socketFileBasename = [idoc|os.path.join(global_state["tmpdir"], #{dquotes socketFileBasename})|]

translateSegment :: (Source -> MDoc) -> SerialManifold -> MDoc
translateSegment makeSrcName m0 =
  let cfg = pythonLowerConfig makeSrcName
      e = runIndex 0 (foldWithSerialManifoldM (defaultFoldRules cfg) m0)
   in vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e

evaluatePattern :: TypeF -> Pattern -> [MDoc] -> MDoc
evaluatePattern _ (PatternText firstStr fragments) xs =
  "f"
    <> (dquotes . hcat) (pretty firstStr : [("{" <> x <> "}" <> pretty s) | (x, s) <- zip xs fragments])
-- getters (always have exactly one argument)
evaluatePattern _ (PatternStruct (ungroup -> [ss])) [m] =
  hcat (m : map writeBasicSelector ss)
evaluatePattern _ (PatternStruct (ungroup -> sss)) [m] =
  tupled [hcat (m : map writeBasicSelector ss) | ss <- sss]
-- setters (always have 1 + n arguments, where the first is the data structure)
evaluatePattern t0 (PatternStruct s0) (m0 : xs0) =
  patternSetter makeTuple makeRecord accessTuple accessRecord m0 t0 s0 xs0
  where
    makeTuple _ xs = tupled xs

    makeRecord (NamF _ _ _ rs) xs = "OrderedDict" <> tupled [pretty k <+> "=" <+> x | (k, x) <- zip (map fst rs) xs]
    makeRecord _ _ = error "Incorrectly typed record setter"

    accessTuple _ m i = m <> "[" <> pretty i <> "]"

    accessRecord (NamF o (FV _ cname) _ _) d k = (selectAccessor o cname) d (pretty k)
    accessRecord t _ _ = error $ "Invalid record type: " <> show t
evaluatePattern _ (PatternStruct _) [] = error "Unreachable empty pattern"

writeBasicSelector :: Either Int Text -> MDoc
writeBasicSelector (Right k) = "[" <> dquotes (pretty k) <> "]"
writeBasicSelector (Left i) = "[" <> pretty i <> "]"

makeDispatch :: [SerialManifold] -> MDoc
makeDispatch ms = vsep [localDispatch, remoteDispatch]
  where
    localDispatch = align . vsep $ ["dispatch = {", indent 4 (vsep . catMaybes $ map entry ms), "}"]

    entry :: SerialManifold -> Maybe MDoc
    entry (SerialManifold _ _ _ HeadManifoldFormRemoteWorker _) = Nothing
    entry (SerialManifold i _ _ _ _) = Just $ pretty i <> ":" <+> manNamer i <> ","

    remoteDispatch = align . vsep $ ["remote_dispatch = {", indent 4 (vsep remotes), "}"]

    remotes :: [MDoc]
    remotes = map entryInt . unique . concat $ map getRemotes ms

    entryInt :: Int -> MDoc
    entryInt i = pretty i <> ":" <+> manNamer i <> "_remote" <> ","

    getRemotes :: SerialManifold -> [Int]
    getRemotes = MM.runIdentity . foldSerialManifoldM (defaultValue {opSerialExprM = getRemoteSE})

    getRemoteSE :: SerialExpr_ [Int] [Int] [Int] [Int] [Int] -> MM.Identity [Int]
    getRemoteSE (AppPoolS_ _ (PoolCall i _ (RemoteCall _) _) xss) = return $ i : concat xss
    getRemoteSE x = return $ foldlSE mappend mempty x

makePool :: [MDoc] -> [MDoc] -> [MDoc] -> MDoc -> MDoc
makePool libs includeDocs manifolds dispatch =
  format
    (DF.embededFileText (DF.poolTemplate Python3Lang))
    "# <<<BREAK>>>"
    [path, includeStatements includeDocs, vsep manifolds, dispatch]
  where
    path = [idoc|sys.path = #{list (map makePath libs)} + sys.path|]
    makePath filename = [idoc|os.path.expanduser(#{dquotes(filename)})|]

    includeStatements [] = ""
    includeStatements _ = vsep ("import importlib" : includeDocs)
