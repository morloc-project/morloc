{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Translator.Generic
Description : Descriptor-driven translator for dynamically-typed languages
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Generic translator that generates pool code for dynamically-typed interpreted
languages based on a LangDescriptor. All language-specific behavior is driven
by descriptor fields -- no hardcoded language-specific code.
-}
module Morloc.CodeGenerator.Grammars.Translator.Generic
  ( translate
  , preprocess
  , CodegenManifest (..)
  , printProgram
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Morloc.CodeGenerator.Grammars.Common
import Morloc.CodeGenerator.Grammars.Translator.Imperative
  ( IAccessor (..)
  , IExpr (..)
  , IProgram (..)
  , IStmt (..)
  , IndexM
  , LowerConfig (..)
  , buildProgram
  , defaultDeserialize
  , defaultFoldRules
  , defaultSerialize
  )
import Morloc.CodeGenerator.Grammars.Translator.PseudoCode (pseudocodeSerialManifold)
import Morloc.CodeGenerator.LanguageDescriptor
import qualified Morloc.CodeGenerator.Serial as Serial
import Morloc.CodeGenerator.LogTemplate (RenderedTemplate (..), collectRenderedTemplates)
import Morloc.CodeGenerator.Namespace
import qualified Data.Map.Strict as Map
import qualified Morloc.Config as MC
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Morloc.DataFiles as DF
import qualified Morloc.LangRegistry as LR
import qualified Morloc.Language as ML
import qualified Morloc.Version as MV
import Morloc.Monad (asks, gets, newIndex, runIndex, getSchemaTable, registerSchemaIndex)
import qualified Morloc.Monad as MM
import Morloc.Quasi
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.IO (hClose, openBinaryTempFile)
import qualified System.Process as Proc

-- | Simple template substitution: replace {{key}} with value
substituteT :: Text -> [(Text, Text)] -> Text
substituteT = foldl (\t (k, v) -> T.replace ("{{" <> k <> "}}") v t)

preprocess :: SerialManifold -> MorlocMonad SerialManifold
preprocess = return . invertSerialManifold

translate :: Lang -> [Source] -> [SerialManifold] -> MorlocMonad Script
translate lang srcs es = do
  desc <- loadDescriptorForLang lang
  case ldCodegenCommand desc of
    Just cmd -> translateExternal cmd lang desc srcs es
    Nothing -> translateBuiltin lang desc srcs es

-- | Translate using the built-in generic renderer.
translateBuiltin :: Lang -> LangDescriptor -> [Source] -> [SerialManifold] -> MorlocMonad Script
translateBuiltin lang desc srcs es = do
  home <- pretty <$> asks MC.configHome
  lib <- MT.pack <$> asks MC.configLibrary
  let opt = home <> "/opt"

  -- translate source imports
  includeDocs <-
    mapM
      (translateSource desc)
      (unique . mapMaybe srcPath $ srcs)

  debugLog (vsep (map pseudocodeSerialManifold es) <> "\n")

  -- build src name function
  let srcNamer =
        if ldQualifiedImports desc
          then qualifiedSrcName lib
          else \src -> pretty (srcName src)

  -- add language-specific preamble from registry
  registry <- gets stateLangRegistry
  let preambleTemplates = case LR.lookupLang (ML.langName lang) registry of
        Just entry -> LR.lrePreamble entry
        Nothing -> []
  homeDir <- asks MC.configHome
  let preambleDocs = map (substitutePreamble home lib opt homeDir) preambleTemplates

  let allSources = preambleDocs ++ includeDocs
      (mDocs, schemas) = runIndex 0 $ do
        docs <- mapM (translateSegment desc srcNamer) es
        tbl <- getSchemaTable
        return (docs, tbl)
  labels <- collectLogLabels <$> gets stateManifoldConfig
  templates <- collectRenderedTemplates lang
  let program = buildProgram labels templates allSources mDocs es schemas

  let code = printProgram desc program
  let exefile = ML.makeExecutablePoolName lang
  let rendered = T.replace "__MORLOC_VERSION__" (MT.pack MV.versionStr) (render code)

  poolSubdir <- getPoolSubdir

  return $
    Script
      { scriptBase = "pool"
      , scriptLang = lang
      , scriptCode = "." :/ Dir "pools" [Dir poolSubdir [File exefile (Code rendered)]]
      , scriptMake = []
      }
  where
    substitutePreamble :: MDoc -> Text -> MDoc -> Path -> Text -> MDoc
    substitutePreamble homeDoc libText optDoc _homeDir t =
      pretty
        . T.replace "{{home}}" (render homeDoc)
        . T.replace "{{lib}}" libText
        . T.replace "{{opt}}" (render optDoc)
        $ t

-- | Translate using an external codegen tool.
translateExternal ::
  Text -> Lang -> LangDescriptor -> [Source] -> [SerialManifold] -> MorlocMonad Script
translateExternal cmd lang desc srcs es = do
  home <- asks MC.configHome
  lib <- MT.pack <$> asks MC.configLibrary

  includeDocs <-
    mapM
      (translateSource desc)
      (unique . mapMaybe srcPath $ srcs)

  debugLog (vsep (map pseudocodeSerialManifold es) <> "\n")

  let srcNamer =
        if ldQualifiedImports desc
          then qualifiedSrcName lib
          else \src -> pretty (srcName src)

  let (mDocs, schemas) = runIndex 0 $ do
        docs <- mapM (translateSegment desc srcNamer) es
        tbl <- getSchemaTable
        return (docs, tbl)
  labels <- collectLogLabels <$> gets stateManifoldConfig
  templates <- collectRenderedTemplates lang
  let program = buildProgram labels templates includeDocs mDocs es schemas

  -- find the lang.yaml path for the codegen tool
  let langYamlPath = home </> "lang" </> T.unpack (ML.langName lang) </> "lang.yaml"

  -- serialize IProgram to a temp file
  tmpDir <- liftIO Dir.getTemporaryDirectory
  (tmpPath, tmpHandle) <- liftIO $ openBinaryTempFile tmpDir "iprogram.bin"
  liftIO $ do
    BL.hPut tmpHandle (Binary.encode program)
    hClose tmpHandle

  -- invoke the codegen command: cmd lang.yaml iprogram.bin
  let cmdStr = T.unpack cmd
  (exitCode, stdoutStr, stderrStr) <-
    liftIO $
      Proc.readCreateProcessWithExitCode
        (Proc.proc cmdStr [langYamlPath, tmpPath])
        ""

  -- clean up temp file
  liftIO $ Dir.removeFile tmpPath

  case exitCode of
    Exit.ExitFailure code' ->
      MM.throwSystemError $
        "External codegen '"
          <> pretty cmd
          <> "' failed with exit code "
          <> pretty code'
          <> ":\n"
          <> pretty stderrStr
    Exit.ExitSuccess -> do
      -- parse the codegen manifest from stdout
      let manifest = Aeson.decodeStrict (TE.encodeUtf8 (T.pack stdoutStr)) :: Maybe CodegenManifest
      case manifest of
        Nothing ->
          MM.throwSystemError $
            "External codegen '"
              <> pretty cmd
              <> "' produced invalid manifest on stdout"
        Just m -> do
          let exefile = ML.makeExecutablePoolName lang
              poolContent = T.replace "__MORLOC_VERSION__" (MT.pack MV.versionStr) (cgmPoolCode m)
              buildCmds = map (SysRun . Code) (cgmBuildCommands m)
          poolSubdir <- getPoolSubdir
          return $
            Script
              { scriptBase = "pool"
              , scriptLang = lang
              , scriptCode = "." :/ Dir "pools" [Dir poolSubdir [File exefile (Code poolContent)]]
              , scriptMake = buildCmds
              }

-- | Manifest returned by an external codegen tool on stdout.
data CodegenManifest = CodegenManifest
  { cgmPoolCode :: Text
  -- ^ rendered pool file content
  , cgmBuildCommands :: [Text]
  -- ^ build commands to run after writing files
  }
  deriving (Show)

instance Aeson.FromJSON CodegenManifest where
  parseJSON = Aeson.withObject "CodegenManifest" $ \v ->
    CodegenManifest
      <$> v Aeson..: "pool_code"
      <*> (v Aeson..:? "build_commands" Aeson..!= [])

instance Aeson.ToJSON CodegenManifest where
  toJSON m =
    Aeson.object
      [ "pool_code" Aeson..= cgmPoolCode m
      , "build_commands" Aeson..= cgmBuildCommands m
      ]

{- | Load the language descriptor for a language.
Tries embedded lang.yaml first, then falls back to filesystem.
If the pool template is empty, loads it from the embedded or filesystem pool file.
-}
loadDescriptorForLang :: Lang -> MorlocMonad LangDescriptor
loadDescriptorForLang lang = do
  let name = ML.langName lang
      ext = ML.langExtension lang
  desc <- loadDescriptorByName name
  -- if pool template is empty, load from embedded or filesystem pool file
  if T.null (ldPoolTemplate desc)
    then do
      poolText <- loadPoolTemplate name ext
      return desc {ldPoolTemplate = poolText}
    else return desc
  where
    loadDescriptorByName :: T.Text -> MorlocMonad LangDescriptor
    loadDescriptorByName name =
      case lookup (T.unpack name) [(n, DF.embededFileText ef) | (n, ef) <- DF.langRegistryFiles] of
        Just yamlText -> case loadLangDescriptorFromText yamlText of
          Left err ->
            MM.throwSystemError $
              "Failed to parse embedded lang.yaml for " <> pretty name <> ": " <> pretty err
          Right desc -> return desc
        Nothing -> do
          -- try filesystem
          home <- asks MC.configHome
          let descPath = home </> "lang" </> T.unpack name </> "lang.yaml"
          result <- liftIO $ loadLangDescriptor descPath
          case result of
            Left err ->
              MM.throwSystemError $
                "Failed to load language descriptor for " <> pretty name <> ": " <> pretty err
            Right desc -> return desc

    loadPoolTemplate :: T.Text -> String -> MorlocMonad T.Text
    loadPoolTemplate name ext =
      -- try embedded pool template first
      case lookupEmbeddedPool name of
        Just t -> return t
        Nothing -> do
          -- try filesystem
          home <- asks MC.configHome
          let poolPath = home </> "lang" </> T.unpack name </> "pool." <> ext
          liftIO $ MT.readFile poolPath

    lookupEmbeddedPool :: T.Text -> Maybe T.Text
    lookupEmbeddedPool "py" = Just $ DF.embededFileText (DF.poolTemplateGeneric "py")
    lookupEmbeddedPool "r" = Just $ DF.embededFileText (DF.poolTemplateGeneric "r")
    lookupEmbeddedPool "cpp" = Just $ DF.embededFileText (DF.poolTemplate "cpp")
    lookupEmbeddedPool _ = Nothing

{- | Get the pool subdirectory name from the module name.
This ensures each program gets its own pool directory (e.g., pools/foo/).
-}
getPoolSubdir :: MorlocMonad String
getPoolSubdir = MM.getModuleName

debugLog :: Doc ann -> MorlocMonad ()
debugLog d = do
  verbosity <- gets stateVerbosity
  when (verbosity > 0) $ (liftIO . putDoc) d

translateSource :: LangDescriptor -> Path -> MorlocMonad MDoc
translateSource desc p = do
  -- Reject early if the file is missing. The path is what the user
  -- wrote in `from "..."`; it must exist somewhere the build can see.
  -- Without this, a missing source surfaces as a confusing runtime
  -- ImportError / module-not-found inside the pool.
  exists <- liftIO $ Dir.doesFileExist p
  unless exists . MM.throwSystemError $
    "Source file not found:" <+> pretty p
  let p' = MT.stripPrefixIfPresent "./" (MT.pack p)
      p'' = if ldIncludeRelToFile desc then "../" <> p' else p'
  if ldQualifiedImports desc
    then do
      lib <- MT.pack <$> asks MC.configLibrary
      let tmpl = ldImportTemplate desc
          ns = render (makeNamespace lib p)
          modPath = render (makeImportPath lib p)
      return . pretty $
        substituteT
          tmpl
          [ ("namespace", ns)
          , ("module_path", modPath)
          ]
    else do
      let tmpl = ldImportTemplate desc
      return . pretty $ substituteT tmpl [("path", p'')]

-- | Qualify a source function name with its module path.
qualifiedSrcName :: Text -> Source -> MDoc
qualifiedSrcName lib src = case srcPath src of
  Nothing -> pretty $ srcName src
  (Just path) -> makeNamespace lib path <> "." <> pretty (srcName src)

makeNamespace :: Text -> Path -> MDoc
makeNamespace lib =
  pretty
    . MT.liftToText (map toLower')
    . MT.replace "/" "_"
    . MT.replace "-" "_"
    . MT.replace "." "_"
    . MT.stripPrefixIfPresent "/"
    . MT.stripPrefixIfPresent "./"
    . MT.stripPrefixIfPresent lib
    . MT.liftToText dropExtensions
    . MT.pack
  where
    toLower' c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
    dropExtensions = reverse . drop 1 . dropWhile (/= '.') . reverse

makeImportPath :: Text -> Path -> MDoc
makeImportPath lib =
  pretty
    . MT.liftToText (map toLower')
    . MT.replace "/" "."
    . MT.stripPrefixIfPresent "/"
    . MT.stripPrefixIfPresent "./"
    . MT.stripPrefixIfPresent lib
    . MT.liftToText dropExtensions
    . MT.pack
  where
    toLower' c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
    dropExtensions = reverse . drop 1 . dropWhile (/= '.') . reverse

translateSegment :: LangDescriptor -> (Source -> MDoc) -> SerialManifold -> IndexM MDoc
translateSegment desc srcNamer m0 =
  let cfg = genericLowerConfig desc srcNamer
   in renderPoolDocs <$> foldWithSerialManifoldM (defaultFoldRules cfg) m0

-- | Build a LowerConfig from a LangDescriptor and a source name function
genericLowerConfig :: LangDescriptor -> (Source -> MDoc) -> LowerConfig IndexM
genericLowerConfig desc srcNamer = cfg
  where
    cfg =
      LowerConfig
        { lcSrcName = srcNamer
        , lcTypeOf = \_ -> return Nothing
        , lcSerialAstType = \_ -> return Nothing
        , lcDeserialAstType = \_ -> return Nothing
        , lcRawDeserialAstType = \_ -> return Nothing
        , lcTypeMOf = \_ -> return Nothing
        , lcPackerName = srcNamer
        , lcUnpackerName = srcNamer
        , lcRecordAccessor = genericRecordAccessor desc
        , lcDeserialRecordAccessor = \_ k v -> case ldKeyAccess desc of
            "double_bracket" -> v <> "[[" <> dquotes (pretty k) <> "]]"
            _ -> v <> "[" <> dquotes (pretty k) <> "]"
        , lcTupleAccessor = \i v -> case ldIndexStyle desc of
            ZeroBracket -> v <> "[" <> pretty i <> "]"
            OneBracket -> v <> "[" <> pretty (i + 1) <> "]"
            OneDoubleBracket -> [idoc|#{v}[[#{pretty (i + 1)}]]|]
        , lcNewIndex = newIndex
        , lcPrintExpr = genericPrintExpr desc
        , lcPrintStmt = genericPrintStmt desc
        , lcEvalPattern = \t p xs -> return $ genericEvalPattern desc t p xs
        , lcListConstructor = \v _ es -> case ldListStyle desc of
            BracketList -> list es
            FunctionCallList -> pretty (ldGenericListFn desc) <> tupled es
            TypeDependentList -> case v of
              (FV _ (CV typeName))
                | typeName `elem` ldAtomicTypes desc -> pretty (ldAtomicListFn desc) <> tupled es
              _ -> pretty (ldGenericListFn desc) <> tupled es
        , lcTupleConstructor = \_ _ -> case ldTupleConstructor desc of
            "" -> tupled
            name -> \es -> pretty name <> tupled es
        , lcRecordConstructor = \_ _ _ _ rs ->
            return $
              defaultValue
                { poolExpr =
                    pretty (ldRecordConstructor desc)
                      <> tupled [makeRecordKey desc k <+> pretty (ldRecordSeparator desc) <+> v | (k, v) <- rs]
                }
        , lcForeignCall = \socketFile mid args ->
            let midDoc = pretty mid <> pretty (ldForeignCallIntSuffix desc)
                argsDoc = case ldListStyle desc of
                  BracketList -> list args
                  _ -> pretty (ldGenericListFn desc) <> tupled args
             in pretty (ldForeignCallFn desc)
                  <> tupled [makeGenericSocketPath desc socketFile, midDoc, argsDoc]
        , lcRemoteCall = genericRemoteCall desc
        , lcCacheBody = genericCacheBody desc cfg
        , lcDebugWrap = genericDebugWrap desc cfg
        , lcMakeIf = genericMakeIf desc cfg
        , lcMakeLet = \namer i _ e1 e2 -> return $ genericMakeLet desc namer i e1 e2
        , lcReleaseStmt = \v -> pretty (ldReleasePacketFn desc) <> "(" <> pretty v <> ")"
        , lcReturn = \e -> pretty $ substituteT (ldReturnTemplate desc) [("expr", render e)]
        , lcMakeDoBlock = genericMakeDoBlock desc cfg
        , lcSerialize = defaultSerialize cfg
        , lcDeserialize = \_ -> defaultDeserialize cfg
        , lcMakeFunction = \mname args _ priorLines body headForm ->
            let makeExt (Just HeadManifoldFormRemoteWorker) = "_remote"
                makeExt _ = ""
                fullName = render (mname <> makeExt headForm)
                argsText = render (hsep (punctuate "," (map argNamer args)))
                header =
                  pretty $
                    substituteT
                      (ldFuncDefHeader desc)
                      [ ("name", fullName)
                      , ("args", argsText)
                      ]
                wrapError [] = []
                wrapError xs =
                  let openLine = ldErrorWrapOpen desc
                      closeLines = ldErrorWrapClose desc
                   in if T.null openLine
                        then xs
                        else
                          let tryBlock = nest 4 (vsep (pretty openLine : xs))
                              exceptBlock =
                                nest 4 . vsep $
                                  map (\l -> pretty $ substituteT l [("name", fullName)]) closeLines
                           in [vsep [tryBlock, exceptBlock]]
             in return . Just $ case ldBlockStyle desc of
                  IndentBlock ->
                    nest 4 (vsep [header, vsep (wrapError priorLines), body])
                  BraceBlock ->
                    block 4 header (vsep $ priorLines <> [body])
                  EndKeywordBlock ->
                    let endKw = ldBlockEnd desc
                     in vsep [header, indent 4 (vsep $ priorLines <> [body]), pretty endKw]
        , lcMakeLambda = \mname contextArgs boundArgs ->
            let tmpl = ldPartialTemplate desc
                fnText = render mname
                allArgsList = contextArgs <> boundArgs
                fnWithCtxList = mname : contextArgs
                fnWithCtx = render (hsep (punctuate "," fnWithCtxList))
                allArgs = render (hsep (punctuate "," allArgsList))
                boundArgsText = render (hsep (punctuate "," boundArgs))
             in pretty $
                  substituteT
                    tmpl
                    [ ("fn", fnText)
                    , ("fn_with_context", fnWithCtx)
                    , ("all_args", allArgs)
                    , ("bound_args", boundArgsText)
                    ]
        , lcRegisterSchema = registerSchemaIndex
        }

{- | Record access: for languages with ldDictStyleRecords=True,
use bracket access for NamRecord and dot access for others.
-}
genericRecordAccessor :: LangDescriptor -> NamType -> CVar -> MDoc -> MDoc -> MDoc
genericRecordAccessor desc namType _constructor record field
  | ldDictStyleRecords desc = case namType of
      NamRecord -> record <> "[" <> dquotes field <> "]"
      _ -> record <> "." <> field
  | otherwise = case ldFieldAccess desc of
      DotAccess -> record <> "." <> field
      DollarAccess -> record <> "$" <> field

-- | Reference an entry of the per-pool 'mlc_schema_table' literal by
-- the schema ID returned from 'lcRegisterSchema'. Adjusts the index
-- for languages whose container literals are 1-based.
genericSchemaRef :: LangDescriptor -> Int -> MDoc
genericSchemaRef desc sid = case ldIndexStyle desc of
  ZeroBracket -> "mlc_schema_table[" <> pretty sid <> "]"
  OneBracket -> "mlc_schema_table[" <> pretty (sid + 1) <> "]"
  OneDoubleBracket -> "mlc_schema_table[[" <> pretty (sid + 1) <> "]]"

-- | Wrap a labeled manifold's body in a cache check. The body's
-- serialized result is what lands on disk, so cross-pool and
-- intra-pool callers share the same format. Syntax (if/else shape,
-- null literal, assignment operator) is taken from the language
-- descriptor.
genericCacheBody ::
  LangDescriptor ->
  LowerConfig IndexM ->
  SerialAST ->
  Text ->
  Int ->
  [(Arg TypeM, SerialAST)] ->
  PoolDocs ->
  IndexM PoolDocs
genericCacheBody desc cfg resSa lbl midx args bodyPool = do
  let intr = pretty (ldIntrinsicPrefix desc)
      assign = pretty (ldAssignOp desc)
      hp = pretty (ldHelperVarPrefix desc)
      keyVar = hp <> "cache_key"
      cachedVar = hp <> "cached"
      resultVar = hp <> "cache_result"
      labelLit = dquotes (pretty lbl)
  preparedArgs <- mapM (prepareCacheArg desc cfg) args
  resSchemaId <- lcRegisterSchema cfg (render $ Serial.serialAstToMsgpackSchema resSa)
  let resSchemaRef = genericSchemaRef desc resSchemaId
      argRefs = [r | (r, _, _) <- preparedArgs]
      schemaRefs = [s | (_, s, _) <- preparedArgs]
      serializeStmts = concatMap (\(_, _, ss) -> ss) preparedArgs
      -- Packets are heterogeneous (R: raw vectors must go through
      -- list()); schemas are a homogeneous character vector (R: c()).
      genericList xs = case ldListStyle desc of
        BracketList -> list xs
        _ -> pretty (ldGenericListFn desc) <> tupled xs
      atomicList xs = case ldListStyle desc of
        BracketList -> list xs
        _ -> pretty (ldAtomicListFn desc) <> tupled xs
      packetList = genericList argRefs
      schemaList = atomicList schemaRefs
      assignLine v rhs = v <+> assign <+> rhs
      midxLit = pretty midx <> pretty (ldIntLiteralSuffix desc)
      keyCompute = assignLine keyVar
        (intr <> "cache_key_compute" <> tupled [midxLit, packetList, schemaList])
      cacheLookup = assignLine cachedVar
        (intr <> "cache_lookup" <> tupled [keyVar, labelLit])
      hitLines =
        [ intr <> "cache_record_hit()"
        , assignLine resultVar cachedVar
        ]
      missLines =
        [ intr <> "cache_record_miss()" ]
        ++ poolPriorLines bodyPool
        ++ [ assignLine resultVar (poolExpr bodyPool)
           , intr <> "cache_store"
               <> tupled [keyVar, labelLit, resultVar, resSchemaRef]
           , intr <> "cache_record_store()"
           ]
      condE = pretty $ substituteT (ldNullCheckTemplate desc)
        [("expr", render cachedVar)]
      ifBlock = case ldBlockStyle desc of
        IndentBlock -> vsep
          [ "if" <+> condE <> ":"
          , indent 4 (vsep hitLines)
          , "else:"
          , indent 4 (vsep missLines)
          ]
        BraceBlock -> vsep
          [ "if" <+> parens condE <+> "{"
          , indent 4 (vsep hitLines)
          , "} else {"
          , indent 4 (vsep missLines)
          , "}"
          ]
        EndKeywordBlock -> vsep
          [ "if" <+> condE
          , indent 4 (vsep hitLines)
          , "else"
          , indent 4 (vsep missLines)
          , "end"
          ]
      priorLines = serializeStmts ++ [keyCompute, cacheLookup, ifBlock]
  return $ defaultValue
    { poolExpr = resultVar
    , poolPriorLines = priorLines
    , poolCompleteManifolds = poolCompleteManifolds bodyPool
    , poolPriorExprs = poolPriorExprs bodyPool
    , poolReturnFlag = poolReturnFlag bodyPool
    }

-- | Debug-trace wrap. Wraps the manifold body in a per-language
-- try/catch. On exception the catch block serializes each arg via
-- its 'SerialAST', invokes @morloc_debug_record_frame@, and
-- re-raises. Zero cost on the happy path: only the try-frame
-- overhead.
--
-- Branches on 'ldBlockStyle' to render either Python (try:/except:)
-- or R (tryCatch). C++ is handled by 'cppDebugWrap' in the C++
-- translator; control never reaches here for it.
genericDebugWrap ::
  LangDescriptor ->
  LowerConfig IndexM ->
  Int ->
  [(Arg TypeM, SerialAST)] ->
  PoolDocs ->
  IndexM PoolDocs
genericDebugWrap desc cfg midx args bodyPool = do
  let intr = pretty (ldIntrinsicPrefix desc)
      assign = pretty (ldAssignOp desc)
      hp = pretty (ldHelperVarPrefix desc)
      resultVar = hp <> "debug_result"
      excVar = hp <> "debug_e"
      dumpExcVar = hp <> "dump_e"
      midxLit = pretty midx <> pretty (ldIntLiteralSuffix desc)
  preparedArgs <- mapM (prepareCacheArg desc cfg) args
  let argRefs = [r | (r, _, _) <- preparedArgs]
      schemaRefs = [s | (_, s, _) <- preparedArgs]
      catchSerializeStmts = concatMap (\(_, _, ss) -> ss) preparedArgs
      genericList xs = case ldListStyle desc of
        BracketList -> list xs
        _ -> pretty (ldGenericListFn desc) <> tupled xs
      atomicList xs = case ldListStyle desc of
        BracketList -> list xs
        _ -> pretty (ldAtomicListFn desc) <> tupled xs
      packetList = genericList argRefs
      schemaList = atomicList schemaRefs
      recordCall = intr <> "debug_record_frame"
        <> tupled [midxLit, packetList, schemaList]
      catchStmts = catchSerializeStmts ++ [recordCall]
      -- Defensive inner wrap around the dump: a serialization failure
      -- during the catch must NOT replace the original exception. If
      -- the inner block throws we silently drop the dump and re-raise
      -- the original.
      block = case ldBlockStyle desc of
        IndentBlock -> vsep
          -- Python
          [ "try:"
          , indent 4 (vsep (poolPriorLines bodyPool
              ++ [resultVar <+> assign <+> poolExpr bodyPool]))
          , "except Exception as" <+> excVar <> ":"
          , indent 4 "try:"
          , indent 8 (vsep catchStmts)
          , indent 4 "except Exception:"
          , indent 8 "pass"
          , indent 4 "raise"
          ]
        BraceBlock -> vsep
          -- R: tryCatch returns the body's value; we assign the whole
          -- thing. The dump is itself wrapped in tryCatch so any
          -- serialization failure is swallowed before re-raising.
          [ resultVar <+> assign <+> "tryCatch({"
          , indent 4 (vsep (poolPriorLines bodyPool ++ [poolExpr bodyPool]))
          , "}, error = function(" <> excVar <> ") {"
          , indent 4 "tryCatch({"
          , indent 8 (vsep catchStmts)
          , indent 4 "}, error = function(" <> dumpExcVar <> ") NULL)"
          , indent 4 "stop(" <> excVar <> ")"
          , "})"
          ]
        EndKeywordBlock -> vsep
          -- No supported lang uses EndKeyword for try/catch today;
          -- placeholder mirroring Ruby/Lua idioms.
          [ "begin"
          , indent 4 (vsep (poolPriorLines bodyPool
              ++ [resultVar <+> assign <+> poolExpr bodyPool]))
          , "rescue" <+> excVar
          , indent 4 "begin"
          , indent 8 (vsep catchStmts)
          , indent 4 "rescue"
          , indent 4 "end"
          , indent 4 "raise"
          , "end"
          ]
  return $ defaultValue
    { poolExpr = resultVar
    , poolPriorLines = [block]
    , poolCompleteManifolds = poolCompleteManifolds bodyPool
    , poolPriorExprs = poolPriorExprs bodyPool
    , poolReturnFlag = poolReturnFlag bodyPool
    }

-- | For a single cache arg produce: (the packet expression, the
-- schema-string expression, any supporting statements). Native args
-- are serialized through 'lcSerialize' into a packet; serial args
-- pass through by name.
prepareCacheArg ::
  LangDescriptor ->
  LowerConfig IndexM ->
  (Arg TypeM, SerialAST) ->
  IndexM (MDoc, MDoc, [MDoc])
prepareCacheArg desc cfg (a@(Arg _ tm), sa) = do
  schemaId <- lcRegisterSchema cfg (render $ Serial.serialAstToMsgpackSchema sa)
  let schemaRef = genericSchemaRef desc schemaId
  case tm of
    Native _ -> do
      pd <- lcSerialize cfg (argNamer a) sa
      return (poolExpr pd, schemaRef, poolPriorLines pd)
    _ -> return (argNamer a, schemaRef, [])

-- | Remote call with template-driven resource packing
genericRemoteCall :: LangDescriptor -> MDoc -> Int -> RemoteResources -> [MDoc] -> IndexM PoolDocs
genericRemoteCall desc socketFile mid res args = do
  let resMem = T.pack . show $ fromMaybe (-1) (remoteResourcesMemory res)
      resTime = T.pack . show $ maybe (-1) unTimeInSeconds (remoteResourcesTime res)
      resCPU = T.pack . show $ fromMaybe (-1) (remoteResourcesThreads res)
      resGPU = T.pack . show $ fromMaybe 0 (remoteResourcesGpus res)
      remoteFn =
        if T.null (ldRemoteCallFn desc)
          then pretty (ldForeignCallFn desc)
          else pretty (ldRemoteCallFn desc)
      resPacked =
        pretty $
          substituteT
            (ldResourcePackTemplate desc)
            [ ("mem", resMem)
            , ("time", resTime)
            , ("cpus", resCPU)
            , ("gpus", resGPU)
            ]
      call =
        remoteFn
          <> tupled [pretty mid, dquotes socketFile, dquotes ".morloc-cache", resPacked, list args]
  return $ defaultValue {poolExpr = call}

-- | Format a record key: bare identifier or quoted string
makeRecordKey :: LangDescriptor -> Key -> MDoc
makeRecordKey desc k
  | ldQuoteRecordKeys desc = dquotes (pretty k)
  | otherwise = pretty k

makeGenericSocketPath :: LangDescriptor -> MDoc -> MDoc
makeGenericSocketPath desc socketFileBasename =
  let tmpl = ldSocketPathTemplate desc
      socketText = render (dquotes socketFileBasename)
   in pretty $ substituteT tmpl [("socket", socketText)]

-- | Generic if/else rendering for descriptor-driven languages
genericMakeIf :: LangDescriptor -> LowerConfig IndexM -> NativeExpr -> PoolDocs -> PoolDocs -> PoolDocs -> IndexM PoolDocs
genericMakeIf desc cfg _ condDocs thenDocs elseDocs = do
  idx <- lcNewIndex cfg
  let v = helperNamer idx
      condE = poolExpr condDocs
      thenE = poolExpr thenDocs
      elseE = poolExpr elseDocs
      thenBlock = poolPriorLines thenDocs <> [v <+> pretty (ldAssignOp desc) <+> thenE]
      elseBlock = poolPriorLines elseDocs <> [v <+> pretty (ldAssignOp desc) <+> elseE]
      ifStmt = case ldBlockStyle desc of
        IndentBlock ->
          vsep
            [ v <+> pretty (ldAssignOp desc) <+> pretty (ldNullLiteral desc)
            , nest 4 (vsep ("if" <+> condE <> ":" : thenBlock))
            , nest 4 (vsep ("else:" : elseBlock))
            ]
        BraceBlock ->
          vsep
            [ v <+> pretty (ldAssignOp desc) <+> pretty (ldNullLiteral desc) <> ";"
            , "if" <+> parens condE <+> "{"
            , indent 4 (vsep thenBlock)
            , "} else {"
            , indent 4 (vsep elseBlock)
            , "}"
            ]
        EndKeywordBlock ->
          let endKw = ldBlockEnd desc
           in vsep
                [ v <+> "<-" <+> "if" <+> parens condE <+> "{"
                , indent 4 (vsep (poolPriorLines thenDocs <> [thenE]))
                , "} else {"
                , indent 4 (vsep (poolPriorLines elseDocs <> [elseE]))
                , "}" <+> pretty endKw
                ]
  return $
    PoolDocs
      { poolCompleteManifolds = poolCompleteManifolds condDocs <> poolCompleteManifolds thenDocs <> poolCompleteManifolds elseDocs
      , poolExpr = v
      , poolPriorLines = poolPriorLines condDocs <> [ifStmt]
      , poolPriorExprs = poolPriorExprs condDocs <> poolPriorExprs thenDocs <> poolPriorExprs elseDocs
      , poolReturnFlag = poolReturnFlag condDocs || poolReturnFlag thenDocs || poolReturnFlag elseDocs
      }

-- Build a suspended do-block thunk. When the thunk form can hold
-- statements (non-empty ldDoBlockBlock, e.g. an R closure body) the
-- prior statements are absorbed into that block. When it cannot (empty
-- ldDoBlockBlock, e.g. a Python lambda) and there ARE prior statements,
-- emit a named local def as a hoisted line and reference it (uncalled)
-- so its body runs only when the thunk is forced; hoisting the
-- statements into the enclosing scope instead would run the effect
-- before a handler. With no prior statements a plain expression thunk
-- (ldDoBlockExpr) always suffices.
genericMakeDoBlock :: LangDescriptor -> LowerConfig IndexM -> TypeF -> [MDoc] -> MDoc -> IndexM ([MDoc], MDoc)
genericMakeDoBlock desc cfg _ stmts expr
  | not (T.null suspendBlock) = case stmts of
      [] -> return ([], exprThunk)
      _ ->
        let body = render (vsep (stmts <> [expr]))
         in return ([], pretty $ substituteT suspendBlock [("body", body)])
  | null stmts = return ([], exprThunk)
  | otherwise = do
      idx <- lcNewIndex cfg
      let nm = helperNamer idx
          header =
            pretty $
              substituteT (ldFuncDefHeader desc) [("name", render nm), ("args", "")]
          retLine = pretty $ substituteT (ldReturnTemplate desc) [("expr", render expr)]
          def = case ldBlockStyle desc of
            BraceBlock -> block 4 header (vsep (stmts <> [retLine]))
            EndKeywordBlock ->
              vsep [header, indent 4 (vsep (stmts <> [retLine])), pretty (ldBlockEnd desc)]
            IndentBlock -> nest 4 (vsep [header, vsep (stmts <> [retLine])])
      return ([def], nm)
  where
    suspendBlock = ldDoBlockBlock desc
    exprThunk = pretty $ substituteT (ldDoBlockExpr desc) [("expr", render expr)]

genericMakeLet :: LangDescriptor -> (Int -> MDoc) -> Int -> PoolDocs -> PoolDocs -> PoolDocs
genericMakeLet desc namer i p1 p2 =
  let rs = poolPriorLines p1 ++ [namer i <+> pretty (ldAssignOp desc) <+> poolExpr p1] ++ poolPriorLines p2
   in PoolDocs
        { poolCompleteManifolds = poolCompleteManifolds p1 <> poolCompleteManifolds p2
        , poolExpr = poolExpr p2
        , poolPriorLines = rs
        , poolPriorExprs = poolPriorExprs p1 <> poolPriorExprs p2
        , poolReturnFlag = poolReturnFlag p1 || poolReturnFlag p2
        }

-- | Generic expression printer driven by descriptor
genericPrintExpr :: LangDescriptor -> IExpr -> MDoc
genericPrintExpr desc = go
  where
    go (IVar v) = pretty v
    go (IBoolLit True) = pretty (ldBoolTrue desc)
    go (IBoolLit False) = pretty (ldBoolFalse desc)
    go (INullLit _) = pretty (ldNullLiteral desc)
    go (IIntLit _ i) = viaShow i <> pretty (ldIntLiteralSuffix desc)
    go (IRealLit _ (RealFinite r)) = viaShow r
    go (IRealLit _ RealPosInf) = pretty (ldRealPosInf desc)
    go (IRealLit _ RealNegInf) = pretty (ldRealNegInf desc)
    go (IRealLit _ RealNaN)    = pretty (ldRealNaN desc)
    go (IStrLit Nothing s) = checkNul s `seq` textEsc' s
    go (IStrLit (Just t) s) = case Map.lookup t (ldStrLiteralMap desc) of
      Just prefix -> checkNul s `seq` pretty prefix <> textEsc' s
      Nothing -> error $ "Cannot render string literal with concrete type "
        ++ show t ++ " in language " ++ show (ldName desc)
        ++ ". Add an entry to ldStrLiteralMap in lang.yaml."
    go (IListLit es) = case ldListStyle desc of
      BracketList -> list (map go es)
      FunctionCallList -> pretty (ldGenericListFn desc) <> tupled (map go es)
      TypeDependentList -> pretty (ldGenericListFn desc) <> tupled (map go es)
    go (ITupleLit es) = case ldTupleConstructor desc of
      "" -> tupled (map go es)
      name -> pretty name <> tupled (map go es)
    go (IRecordLit _ _ entries) =
      pretty (ldRecordConstructor desc)
        <> tupled [makeRecordKey desc k <+> pretty (ldRecordSeparator desc) <+> go e | (k, e) <- entries]
    go (IAccess e (IIdx i)) = case ldIndexStyle desc of
      ZeroBracket -> go e <> "[" <> pretty i <> "]"
      OneBracket -> go e <> "[" <> pretty (i + 1) <> "]"
      OneDoubleBracket -> go e <> "[[" <> pretty (i + 1) <> "]]"
    go (IAccess e (IKey k)) = case ldKeyAccess desc of
      "double_bracket" -> go e <> "[[" <> dquotes (pretty k) <> "]]"
      _ -> go e <> "[" <> dquotes (pretty k) <> "]"
    go (IAccess e (IField f)) = case ldFieldAccess desc of
      DotAccess -> go e <> "." <> pretty f
      DollarAccess -> go e <> "$" <> pretty f
    go (ISerCall sid e) =
      pretty (ldSerializeFn desc) <> "(" <> go e <> ", " <> schemaRef sid <> ")"
    go (IDesCall sid _ e) =
      pretty (ldDeserializeFn desc) <> "(" <> go e <> ", " <> schemaRef sid <> ")"
    go (IPack packer e) = pretty packer <> parens (go e)
    go (ICall f Nothing argGroups) =
      pretty f <> hsep (map (tupled . map go) argGroups)
    go (ICall f (Just _) argGroups) =
      pretty f <> hsep (map (tupled . map go) argGroups)
    go (IForeignCall _ _ _) = error "use IRawExpr for generic foreign calls"
    go (IRemoteCall _ _ _ _) = error "use IRawExpr for generic remote calls"
    go (ILambda args body) =
      let argsText = render (hsep (punctuate "," (map pretty args)))
          bodyText = render (go body)
       in pretty $
            substituteT
              (ldLambdaTemplate desc)
              [ ("args", argsText)
              , ("body", bodyText)
              ]
    go (IRawExpr d) = pretty d
    go (IDoBlock e) =
      pretty $ substituteT (ldDoBlockExpr desc) [("expr", render (go e))]
    go (IEval e) = go e <> "()"
    go (IIntrinsicHash sid e) =
      let prefix = ldIntrinsicPrefix desc
       in pretty prefix <> "mlc_hash(" <> go e <> ", " <> schemaRef sid <> ")"
    go (IIntrinsicSave fmt sid e path) =
      let prefix = ldIntrinsicPrefix desc
          saveFn :: Text
          saveFn = case fmt of
            "json"     -> "mlc_save_json"
            "voidstar" -> "mlc_save_voidstar"
            _          -> "mlc_save"
       in pretty prefix <> pretty saveFn <> "(" <> go e <> ", " <> schemaRef sid <> ", " <> go path <> ")"
    go (IIntrinsicLoad sid _ path) =
      let prefix = ldIntrinsicPrefix desc
       in pretty prefix <> "mlc_load(" <> schemaRef sid <> ", " <> go path <> ")"
    go (IIntrinsicShow sid e) =
      let prefix = ldIntrinsicPrefix desc
       in pretty prefix <> "mlc_show(" <> go e <> ", " <> schemaRef sid <> ")"
    go (IIntrinsicRead sid _ e) =
      let prefix = ldIntrinsicPrefix desc
       in pretty prefix <> "mlc_read(" <> schemaRef sid <> ", " <> go e <> ")"

    schemaRef = genericSchemaRef desc

    -- Languages that opt out of NUL-in-Str (allow_string_null = false)
    -- cannot represent a `\0` inside a source-level string literal: R
    -- refuses to parse `"\000"` at all, and the pool would die before
    -- any runtime guard can fire. Catch the situation at codegen time
    -- and emit a clear compile error. Runtime-borne NUL strings
    -- (arriving via FFI from another pool or the nexus) are caught
    -- separately by the runtime null_check.
    checkNul :: Text -> ()
    checkNul s
      | ldAllowStringNull desc = ()
      | T.any (== '\0') s =
          error $
            "Embedded NUL byte in a Str literal destined for the "
              ++ show (ldName desc)
              ++ " pool. The "
              ++ show (ldName desc)
              ++ " language cannot represent NUL bytes in its native "
              ++ "string type. Move the literal to a language that "
              ++ "supports it (Python, C++, Julia, or the nexus itself), "
              ++ "or remove the NUL byte. (See lang.yaml's "
              ++ "allow_string_null field.)"
      | otherwise = ()

-- | Generic statement printer driven by descriptor
genericPrintStmt :: LangDescriptor -> IStmt -> MDoc
genericPrintStmt desc = go
  where
    printE = genericPrintExpr desc

    go (IAssign v Nothing e) = pretty v <+> pretty (ldAssignOp desc) <+> printE e
    go (IAssign v (Just _) e) = pretty v <+> pretty (ldAssignOp desc) <+> printE e
    go (IMapList resultVar _ iterVar collection bodyStmts yieldExpr) =
      case ldMapStyle desc of
        LoopAppend ->
          vsep
            [ pretty resultVar <+> pretty (ldAssignOp desc) <+> "[]"
            , nest
                4
                ( vsep
                    ( ("for" <+> pretty iterVar <+> "in" <+> printE collection <> ":")
                        : map go bodyStmts
                        ++ [pretty resultVar <> ".append(" <> printE yieldExpr <> ")"]
                    )
                )
            ]
        ApplyCallback ->
          block
            4
            ( pretty resultVar <+> pretty (ldAssignOp desc) <+> "lapply("
                <> printE collection
                <> "," <+> "function("
                <> pretty iterVar
                <> ")"
            )
            (vsep (map go bodyStmts ++ [printE yieldExpr]))
            <> ")"
        ListComprehension ->
          vsep
            [ pretty resultVar <+> pretty (ldAssignOp desc) <+> "["
                <> printE yieldExpr
                  <+> "for"
                  <+> pretty iterVar
                  <+> "in"
                  <+> printE collection
                <> "]"
            ]
    go (IReturn e) = pretty $ substituteT (ldReturnTemplate desc) [("expr", render (printE e))]
    go (IExprStmt e) = printE e
    go (IIf resultVar _ condExpr thenStmts thenExpr elseStmts elseExpr) =
      case ldBlockStyle desc of
        IndentBlock ->
          vsep
            [ pretty resultVar <+> pretty (ldAssignOp desc) <+> pretty (ldNullLiteral desc)
            , nest 4 (vsep ("if" <+> printE condExpr <> ":" : map go thenStmts ++ [pretty resultVar <+> pretty (ldAssignOp desc) <+> printE thenExpr]))
            , nest 4 (vsep ("else:" : map go elseStmts ++ [pretty resultVar <+> pretty (ldAssignOp desc) <+> printE elseExpr]))
            ]
        BraceBlock ->
          vsep
            [ pretty resultVar <+> pretty (ldAssignOp desc) <+> pretty (ldNullLiteral desc) <> ";"
            , "if" <+> parens (printE condExpr) <+> "{"
            , indent 4 (vsep (map go thenStmts ++ [pretty resultVar <+> pretty (ldAssignOp desc) <+> printE thenExpr <> ";"]))
            , "} else {"
            , indent 4 (vsep (map go elseStmts ++ [pretty resultVar <+> pretty (ldAssignOp desc) <+> printE elseExpr <> ";"]))
            , "}"
            ]
        EndKeywordBlock ->
          vsep
                [ pretty resultVar <+> "<-" <+> "if" <+> parens (printE condExpr) <+> "{"
                , indent 4 (vsep (map go thenStmts ++ [printE thenExpr]))
                , "} else {"
                , indent 4 (vsep (map go elseStmts ++ [printE elseExpr]))
                , "}"
                ]
    go (IIfNotNull resultVar _ source unwrapVar _ bodyStmts bodyExpr) =
      let srcVar = unwrapVar <> "_src"
          srcDoc = pretty srcVar
          srcBind = srcDoc <+> pretty (ldAssignOp desc) <+> printE source
          condDoc = pretty $ substituteT (ldNullCheckTemplate desc) [("expr", render srcDoc)]
          bindUnwrap = pretty unwrapVar <+> pretty (ldAssignOp desc) <+> srcDoc
       in case ldBlockStyle desc of
            IndentBlock ->
              vsep
                [ srcBind
                , pretty resultVar <+> pretty (ldAssignOp desc) <+> pretty (ldNullLiteral desc)
                , nest 4
                    ( vsep
                        ( ("if" <+> condDoc <> ":")
                            : bindUnwrap
                            : map go bodyStmts
                            ++ [pretty resultVar <+> pretty (ldAssignOp desc) <+> printE bodyExpr]
                        )
                    )
                ]
            BraceBlock ->
              vsep
                [ srcBind <> ";"
                , pretty resultVar <+> pretty (ldAssignOp desc) <+> pretty (ldNullLiteral desc) <> ";"
                , "if" <+> parens condDoc <+> "{"
                , indent 4 (vsep (bindUnwrap <> ";" : map go bodyStmts ++ [pretty resultVar <+> pretty (ldAssignOp desc) <+> printE bodyExpr <> ";"]))
                , "}"
                ]
            EndKeywordBlock ->
              vsep
                [ srcBind
                , pretty resultVar <+> "<-" <+> "if" <+> parens condDoc <+> "{"
                , indent 4 (vsep (bindUnwrap : map go bodyStmts ++ [printE bodyExpr]))
                , "} else {"
                , indent 4 (pretty (ldNullLiteral desc))
                , "}"
                ]
    go (IFunDef _ _ _ _) = error "IFunDef not yet implemented for generic printer"

-- | Assemble a complete pool file from descriptor, template, and IProgram
printProgram :: LangDescriptor -> IProgram -> MDoc
printProgram desc prog =
  format
    (ldPoolTemplate desc)
    (ldBreakMarker desc)
    sections
  where
    sections =
      [ vsep (map pretty (ipSources prog) ++ [schemaTableInit])
      , vsep (map pretty (ipManifolds prog) ++ logRebindings)
      , templateDispatch
      ]

    -- Rebind each labeled manifold function name to a logging shim. Placed
    -- immediately after the manifold definitions so any later reference
    -- (other manifolds calling this one, the dispatch table, partials
    -- passed into higher-order stdlib functions) resolves to the wrapped
    -- version. The shim is the per-language @__mlc_wrap_log@ helper; the
    -- three template strings are passed as arguments and the helper
    -- formats them on emission with the runtime values for @{date}@,
    -- @{runtime}@, and @{id}@.
    logRebindings :: [MDoc]
    logRebindings
      | T.null (ldLogWrap desc) = []
      | otherwise =
          [ manNamer i <+> pretty (ldAssignOp desc) <+> wrapFn desc
              <> tupled
                [ quoteString (renderedGroup tmpl)
                , quoteOrNone (renderedStart tmpl)
                , quoteOrNone (renderedPass tmpl)
                , quoteOrNone (renderedFail tmpl)
                , manNamer i
                ]
          | (i, tmpl) <- Map.toAscList (ipLogTemplates prog)
          ]

    -- The wrap helper name varies per language (Python: @__mlc_wrap_log@;
    -- R: @.mlc_wrap_log@). The 'ldLogWrap' field carries the bare helper
    -- name; an empty string opts the language out of logging.
    wrapFn :: LangDescriptor -> MDoc
    wrapFn d = pretty (ldLogWrap d)

    quoteOrNone :: Maybe Text -> MDoc
    quoteOrNone Nothing = pretty (ldNullLiteral desc)
    quoteOrNone (Just t) = dquotes (pretty (escapeQuotes "\"" "\\\"" (escapeStringLit t)))

    -- Always-present string (e.g. the label group name). Empty strings
    -- render as a normal "" literal; downstream pool helpers treat
    -- empty as "skip the per-label tee".
    quoteString :: Text -> MDoc
    quoteString t = dquotes (pretty (escapeQuotes "\"" "\\\"" (escapeStringLit t)))

    schemaTableInit
      | null (ipSchemaTable prog) = mempty
      | otherwise =
          let entries = [dquotes (pretty s) | s <- ipSchemaTable prog]
              listExpr = case ldListStyle desc of
                BracketList -> list entries
                _ -> pretty (ldGenericListFn desc) <> tupled entries
           in "mlc_schema_table = " <> listExpr

    templateDispatch = vsep [localD, remoteD]
      where
        renderEntry :: Text -> DispatchEntry -> MDoc
        renderEntry entryTmpl (DispatchEntry i _ _) =
          pretty $
            substituteT entryTmpl [("mid", T.pack (show i)), ("name", render (manNamer i))]

        localD =
          let hdr = ldDispatchLocalHeader desc
              entryTmpl = ldDispatchLocalEntry desc
              ftr = ldDispatchLocalFooter desc
              entries = map (renderEntry entryTmpl) (ipLocalDispatch prog)
           in if T.null hdr && T.null entryTmpl
                then mempty
                else
                  align . vsep $
                    filter
                      (not . isEmpty)
                      [ pretty hdr
                      , vsep entries
                      , pretty ftr
                      ]

        remoteD =
          let hdr = ldDispatchRemoteHeader desc
              entryTmpl = ldDispatchRemoteEntry desc
              ftr = ldDispatchRemoteFooter desc
              entries = map (renderEntry entryTmpl) (ipRemoteDispatch prog)
           in if T.null hdr && T.null entryTmpl
                then mempty
                else
                  align . vsep $
                    filter
                      (not . isEmpty)
                      [ pretty hdr
                      , vsep entries
                      , pretty ftr
                      ]

    isEmpty d = T.null (render d)

-- | Generic pattern evaluation
genericEvalPattern :: LangDescriptor -> TypeF -> Pattern -> [MDoc] -> MDoc
genericEvalPattern desc _ (PatternText firstStr fragments) xs =
  case ldPatternStyle desc of
    FStringPattern ->
      let qt = pretty (ldQuoteTerminator desc)
          esc = escapeQuotes (ldQuoteTerminator desc) (ldQuoteTerminatorEsc desc) . escapeStringLit
      in "f" <> qt <> hcat (pretty (esc firstStr) : [("{" <> x <> "}" <> pretty (esc s)) | (x, s) <- zip xs fragments]) <> qt
    ConcatCall ->
      let qt = ldQuoteTerminator desc
          esc = escapeQuotes qt (ldQuoteTerminatorEsc desc) . escapeStringLit
          wrap t = pretty qt <> pretty t <> pretty qt
      in pretty (ldConcatFn desc)
        <> tupled (wrap (esc firstStr) : concat [[x, wrap (esc s)] | (x, s) <- zip xs fragments])
-- getters (always have exactly one argument)
genericEvalPattern desc _ (PatternStruct (ungroup -> [ss])) [m] =
  hcat (m : map (writeSelector desc) ss)
genericEvalPattern desc _ (PatternStruct (ungroup -> sss)) [m] =
  case ldTupleConstructor desc of
    "" -> tupled [hcat (m : map (writeSelector desc) ss) | ss <- sss]
    name -> pretty name <> tupled [hcat (m : map (writeSelector desc) ss) | ss <- sss]
-- setters
genericEvalPattern desc t0 (PatternStruct s0) (m0 : xs0) =
  patternSetter makeTuple makeRecord accessTuple accessRecord m0 t0 s0 xs0
  where
    makeTuple _ xs = case ldTupleConstructor desc of
      "" -> tupled xs
      name -> pretty name <> tupled xs

    makeRecord (NamF _ _ _ rs) xs =
      pretty (ldRecordConstructor desc)
        <> tupled
          [makeRecordKey desc k <+> pretty (ldRecordSeparator desc) <+> x | (k, x) <- zip (map fst rs) xs]
    makeRecord _ _ = error "Incorrectly typed record setter"

    accessTuple _ m i = case ldIndexStyle desc of
      ZeroBracket -> m <> "[" <> pretty i <> "]"
      OneBracket -> m <> "[" <> pretty (i + 1) <> "]"
      OneDoubleBracket -> m <> "[[" <> pretty (i + 1) <> "]]"

    accessRecord (NamF o (FV _ cname) _ _) d k =
      genericRecordAccessor desc o cname d (pretty k)
    accessRecord t _ _ = error $ "Invalid record type: " <> show t
genericEvalPattern _ _ (PatternStruct _) [] = error "Unreachable empty pattern"
-- Bracket index: receiver[index]. Emits native indexing syntax per
-- language. Args are [index, receiver].
genericEvalPattern desc _ PatternBracketIndex [i, m] =
  case ldIndexStyle desc of
    ZeroBracket -> m <> "[" <> i <> "]"
    OneBracket -> m <> "[(" <> i <> ") + 1]"
    OneDoubleBracket -> m <> "[[(" <> i <> ") + 1]]"
genericEvalPattern _ _ PatternBracketIndex args =
  error $ "PatternBracketIndex expects 2 args (index, receiver), got " <> show (length args)
-- Bracket slice: receiver[start:stop:step]. For now uses native Python-style
-- slicing where the language supports it. Args are [start, stop, step, receiver].
-- start/stop/step may be Null (rendered as a language-specific null).
genericEvalPattern desc _ PatternBracketSlice [start, stop, step, m] =
  case ldIndexStyle desc of
    ZeroBracket -> m <> "[" <> start <> ":" <> stop <> ":" <> step <> "]"
    -- Non-zero-based languages (R) don't have native Python-style step
    -- slicing. Emit a fallback that will need a runtime helper.
    _ -> "morloc_slice" <> tupled [start, stop, step, m]
genericEvalPattern _ _ PatternBracketSlice args =
  error $ "PatternBracketSlice expects 4 args (start, stop, step, receiver), got " <> show (length args)

writeSelector :: LangDescriptor -> Either Int Text -> MDoc
writeSelector desc (Right k) = case ldKeyAccess desc of
  "double_bracket" -> "[[" <> dquotes (pretty k) <> "]]"
  _ -> "[" <> dquotes (pretty k) <> "]"
writeSelector desc (Left i) = case ldIndexStyle desc of
  ZeroBracket -> "[" <> pretty i <> "]"
  OneBracket -> "[" <> pretty (i + 1) <> "]"
  OneDoubleBracket -> "[[" <> pretty (i + 1) <> "]]"
