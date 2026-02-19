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
languages based on a LangDescriptor. Handles Python, R, Julia, and other
languages with Python/R-like semantics.
-}
module Morloc.CodeGenerator.Grammars.Translator.Generic
  ( translate
  , preprocess
  , CodegenManifest(..)
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
  ( LowerConfig(..), IndexM, defaultSerialize, defaultDeserialize
  , defaultFoldRules, buildProgram, IProgram(..), IExpr(..), IStmt(..)
  , IAccessor(..)
  )
import Morloc.CodeGenerator.LanguageDescriptor
import Morloc.CodeGenerator.Grammars.Translator.PseudoCode (pseudocodeSerialManifold)
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Config as MC
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Morloc.DataFiles as DF
import qualified Morloc.Language as ML
import qualified Morloc.LangRegistry as LR
import Morloc.Monad (asks, gets, liftIO, newIndex, runIndex)
import qualified Morloc.Monad as MM
import Morloc.Quasi
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.IO (hClose, openBinaryTempFile)
import qualified System.Process as Proc

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

  -- build src name function (Python needs qualified names)
  let srcNamer = if ldQualifiedImports desc
        then qualifiedSrcName lib
        else \src -> pretty (srcName src)

  -- add language-specific preamble from registry
  registry <- gets stateLangRegistry
  let preambleTemplates = case LR.lookupLang (ML.langName lang) registry of
        Just entry -> LR.lrePreamble entry
        Nothing -> []
  homeDir <- asks MC.configHome
  let preambleDocs = map (substituteTemplate home lib opt homeDir) preambleTemplates

  let allSources = preambleDocs ++ includeDocs
      mDocs = map (translateSegment desc srcNamer) es
      program = buildProgram allSources mDocs es

  let code = printProgram desc program
  let exefile = ML.makeExecutablePoolName lang

  return $
    Script
      { scriptBase = "pool"
      , scriptLang = lang
      , scriptCode = "." :/ Dir "pools" [File exefile (Code . render $ code)]
      , scriptMake = []
      }
  where
    makePath filename = [idoc|os.path.expanduser(#{dquotes filename})|]

    substituteTemplate :: MDoc -> Text -> MDoc -> Path -> Text -> MDoc
    substituteTemplate homeDoc libText optDoc homeDir t =
      pretty
        . T.replace "{{home}}" (render homeDoc)
        . T.replace "{{lib}}" libText
        . T.replace "{{opt}}" (render optDoc)
        $ t

-- | Translate using an external codegen tool.
translateExternal :: Text -> Lang -> LangDescriptor -> [Source] -> [SerialManifold] -> MorlocMonad Script
translateExternal cmd lang desc srcs es = do
  home <- asks MC.configHome
  lib <- MT.pack <$> asks MC.configLibrary

  includeDocs <-
    mapM
      (translateSource desc)
      (unique . mapMaybe srcPath $ srcs)

  debugLog (vsep (map pseudocodeSerialManifold es) <> "\n")

  let srcNamer = if ldQualifiedImports desc
        then qualifiedSrcName lib
        else \src -> pretty (srcName src)

  let mDocs = map (translateSegment desc srcNamer) es
      program = buildProgram includeDocs mDocs es

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
  (exitCode, stdoutStr, stderrStr) <- liftIO $
    Proc.readCreateProcessWithExitCode
      (Proc.proc cmdStr [langYamlPath, tmpPath])
      ""

  -- clean up temp file
  liftIO $ Dir.removeFile tmpPath

  case exitCode of
    Exit.ExitFailure code' ->
      MM.throwSystemError $
        "External codegen '" <> pretty cmd <> "' failed with exit code "
        <> pretty code' <> ":\n" <> pretty stderrStr
    Exit.ExitSuccess -> do
      -- parse the codegen manifest from stdout
      let manifest = Aeson.decodeStrict (TE.encodeUtf8 (T.pack stdoutStr)) :: Maybe CodegenManifest
      case manifest of
        Nothing ->
          MM.throwSystemError $
            "External codegen '" <> pretty cmd
            <> "' produced invalid manifest on stdout"
        Just m -> do
          let exefile = ML.makeExecutablePoolName lang
              poolContent = cgmPoolCode m
              buildCmds = map (SysRun . Code) (cgmBuildCommands m)
          return $
            Script
              { scriptBase = "pool"
              , scriptLang = lang
              , scriptCode = "." :/ Dir "pools" [File exefile (Code poolContent)]
              , scriptMake = buildCmds
              }

-- | Manifest returned by an external codegen tool on stdout.
data CodegenManifest = CodegenManifest
  { cgmPoolCode :: Text       -- ^ rendered pool file content
  , cgmBuildCommands :: [Text] -- ^ build commands to run after writing files
  } deriving (Show)

instance Aeson.FromJSON CodegenManifest where
  parseJSON = Aeson.withObject "CodegenManifest" $ \v ->
    CodegenManifest
      <$> v Aeson..: "pool_code"
      <*> (v Aeson..:? "build_commands" Aeson..!= [])

instance Aeson.ToJSON CodegenManifest where
  toJSON m = Aeson.object
    [ "pool_code" Aeson..= cgmPoolCode m
    , "build_commands" Aeson..= cgmBuildCommands m
    ]

-- | Load the language descriptor for a language.
-- Tries embedded lang.yaml first, then falls back to filesystem.
-- If the pool template is empty, loads it from the embedded or filesystem pool file.
loadDescriptorForLang :: Lang -> MorlocMonad LangDescriptor
loadDescriptorForLang lang = do
  let name = ML.langName lang
      ext = ML.langExtension lang
  desc <- loadDescriptorByName name
  -- if pool template is empty, load from embedded or filesystem pool file
  if T.null (ldPoolTemplate desc)
    then do
      poolText <- loadPoolTemplate name ext
      return desc { ldPoolTemplate = poolText }
    else return desc
  where
    loadDescriptorByName :: T.Text -> MorlocMonad LangDescriptor
    loadDescriptorByName name =
      case lookup (T.unpack name) [(n, DF.embededFileText ef) | (n, ef) <- DF.langRegistryFiles] of
        Just yamlText -> case loadLangDescriptorFromText yamlText of
          Left err -> MM.throwSystemError $
            "Failed to parse embedded lang.yaml for " <> pretty name <> ": " <> pretty err
          Right desc -> return desc
        Nothing -> do
          -- try filesystem
          home <- asks MC.configHome
          let descPath = home </> "lang" </> T.unpack name </> "lang.yaml"
          result <- liftIO $ loadLangDescriptor descPath
          case result of
            Left err -> MM.throwSystemError $
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

debugLog :: Doc ann -> MorlocMonad ()
debugLog d = do
  verbosity <- gets stateVerbosity
  when (verbosity > 0) $ (liftIO . putDoc) d

translateSource :: LangDescriptor -> Path -> MorlocMonad MDoc
translateSource desc p = do
  let p' = MT.stripPrefixIfPresent "./" (MT.pack p)
      p'' = if ldIncludeRelToFile desc then "../" <> p' else p'
  case ldFunctionDef desc of
    RAssignDef -> return $ "source(" <> dquotes (pretty p'') <> ")"
    EndBlockDef -> return $ "include(" <> dquotes (pretty p'') <> ")"
    _ -> do
      lib <- MT.pack <$> asks MC.configLibrary
      return $ makeNamespace lib p <+> "=" <+> "importlib.import_module("
        <> dquotes (makeImportPath lib p) <> ")"

-- | Qualify a source function name with its module path (Python).
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

translateSegment :: LangDescriptor -> (Source -> MDoc) -> SerialManifold -> MDoc
translateSegment desc srcNamer m0 =
  let cfg = genericLowerConfig desc srcNamer
   in renderPoolDocs $ runIndex 0 (foldWithSerialManifoldM (defaultFoldRules cfg) m0)

-- | Build a LowerConfig from a LangDescriptor and a source name function
genericLowerConfig :: LangDescriptor -> (Source -> MDoc) -> LowerConfig IndexM
genericLowerConfig desc srcNamer = cfg
  where
    cfg = LowerConfig
      { lcSrcName = srcNamer
      , lcTypeOf = \_ -> return Nothing
      , lcSerialAstType = \_ -> return Nothing
      , lcDeserialAstType = \_ -> return Nothing
      , lcRawDeserialAstType = \_ -> return Nothing
      , lcTemplateArgs = \_ -> return Nothing
      , lcTypeMOf = \_ -> return Nothing
      , lcPackerName = srcNamer
      , lcUnpackerName = srcNamer
      , lcRecordAccessor = genericRecordAccessor desc
      , lcDeserialRecordAccessor = \_ k v -> case ldFieldAccess desc of
          DotAccess -> v <> "[" <> dquotes (pretty k) <> "]"
          DollarAccess -> v <> "$" <> pretty k
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
          FunctionList -> "list" <> tupled es
          RAtomicList -> case v of
            (FV _ (CV "integer"))   -> "c" <> tupled es
            (FV _ (CV "numeric"))   -> "c" <> tupled es
            (FV _ (CV "double"))    -> "c" <> tupled es
            (FV _ (CV "logical"))   -> "c" <> tupled es
            (FV _ (CV "character")) -> "c" <> tupled es
            _ -> "list" <> tupled es
      , lcTupleConstructor = \_ -> case ldTupleConstructor desc of
          "" -> tupled
          name -> \es -> pretty name <> tupled es
      , lcRecordConstructor = \_ _ _ _ rs -> return $ defaultValue
          { poolExpr = pretty (ldRecordConstructor desc)
              <> tupled [makeRecordKey desc k <+> pretty (ldRecordSeparator desc) <+> v | (k, v) <- rs] }
      , lcForeignCall = \socketFile mid args ->
          let midDoc = pretty mid <> pretty (ldForeignCallIntSuffix desc)
              argsDoc = case ldListStyle desc of
                BracketList -> list args
                _ -> "list" <> tupled args
           in pretty (ldForeignCallFn desc)
              <> tupled [makeGenericSocketPath desc socketFile, midDoc, argsDoc]
      , lcRemoteCall = genericRemoteCall desc
      , lcMakeLet = \namer i _ e1 e2 -> return $ genericMakeLet desc namer i e1 e2
      , lcReturn = \e -> "return(" <> e <> ")"
      , lcMakeSuspend = \stmts expr -> case ldSuspend desc of
          PythonSuspend -> (stmts, "(lambda:" <+> expr <> ")")
          RSuspend -> (,) [] $ case stmts of
            [] -> "(function()" <+> expr <> ")"
            _ -> "function(){" <> nest 4 (line <> vsep (stmts <> [expr])) <> line <> "}"
          ArrowSuspend -> (stmts, "(() ->" <+> expr <> ")")
      , lcSerialize = defaultSerialize cfg
      , lcDeserialize = \_ -> defaultDeserialize cfg
      , lcMakeFunction = \mname args _ priorLines body headForm ->
          let makeExt (Just HeadManifoldFormRemoteWorker) = "_remote"
              makeExt _ = ""
           in return . Just $ case ldFunctionDef desc of
                PythonDef ->
                  let def = "def" <+> mname <> makeExt headForm <> tupled (map argNamer args) <> ":"
                      tryCatch [] = ""
                      tryCatch xs =
                        let tryBlock = nest 4 (vsep ("try:" : xs))
                            exceptBlock =
                              nest 4 (vsep
                                [ "except Exception as e:"
                                , [idoc|raise RuntimeError(f"Error (pool daemon in #{mname}):\n{e!s}")|]
                                ])
                         in vsep [tryBlock, exceptBlock]
                   in nest 4 (vsep [def, tryCatch priorLines, body])
                RAssignDef ->
                  let def = mname <> makeExt headForm <+> "<-" <+> "function" <> tupled (map argNamer args)
                   in block 4 def (vsep $ priorLines <> [body])
                EndBlockDef ->
                  let def = "function" <+> mname <> makeExt headForm <> tupled (map argNamer args)
                   in vsep [def, indent 4 (vsep $ priorLines <> [body]), "end"]
      , lcMakeLambda = \mname contextArgs boundArgs -> case ldPartialApp desc of
          FunctoolsPartial -> "functools.partial" <> tupled (mname : contextArgs)
          AnonymousWrapper ->
            let functionCall = mname <> tupled (contextArgs <> boundArgs)
             in "function" <+> tupled boundArgs <> "{" <> functionCall <> "}"
          ArrowWrapper ->
            let functionCall = mname <> tupled (contextArgs <> boundArgs)
             in tupled boundArgs <+> "->" <+> functionCall
      }

-- | Record access: for Python-like languages with ldDictStyleRecords=True,
-- use bracket access for dict/NamRecord and dot access for others.
genericRecordAccessor :: LangDescriptor -> NamType -> CVar -> MDoc -> MDoc -> MDoc
genericRecordAccessor desc namType constructor record field
  | ldDictStyleRecords desc = case (namType, constructor) of
      (NamTable, CV "dict") -> record <> "[" <> dquotes field <> "]"
      (NamRecord, _)        -> record <> "[" <> dquotes field <> "]"
      _                     -> record <> "." <> field
  | otherwise = case ldFieldAccess desc of
      DotAccess -> record <> "." <> field
      DollarAccess -> record <> "$" <> field

-- | Remote call with per-language resource packing
genericRemoteCall :: LangDescriptor -> MDoc -> Int -> RemoteResources -> [MDoc] -> IndexM PoolDocs
genericRemoteCall desc socketFile mid res args = do
  let resMem = pretty $ remoteResourcesMemory res
      resTime = pretty $ remoteResourcesTime res
      resCPU = pretty $ remoteResourcesThreads res
      resGPU = pretty $ remoteResourcesGpus res
      remoteFn = if T.null (ldRemoteCallFn desc)
                   then pretty (ldForeignCallFn desc)
                   else pretty (ldRemoteCallFn desc)
      resPacked = case ldResourcePackStyle desc of
        StructPackResources ->
          "struct.pack" <> tupled [squotes "iiii", resMem, resTime, resCPU, resGPU]
        NamedListResources ->
          [idoc|list(mem=#{resMem}L, time=#{resTime}L, cpus=#{resCPU}L, gpus=#{resGPU}L)|]
        PlainListResources ->
          list [resMem, resTime, resCPU, resGPU]
      call = remoteFn
        <> tupled [pretty mid, dquotes socketFile, dquotes ".morloc-cache", resPacked, list args]
  return $ defaultValue {poolExpr = call}

-- | Format a record key: bare identifier or quoted string
makeRecordKey :: LangDescriptor -> Key -> MDoc
makeRecordKey desc k
  | ldQuoteRecordKeys desc = dquotes (pretty k)
  | otherwise = pretty k

makeGenericSocketPath :: LangDescriptor -> MDoc -> MDoc
makeGenericSocketPath desc socketFileBasename =
  case ldForeignCallSocketPath desc of
    "paste0" -> [idoc|paste0(global_state$tmpdir, "/", #{dquotes socketFileBasename})|]
    "joinpath" -> [idoc|joinpath(global_state["tmpdir"], #{dquotes socketFileBasename})|]
    _ -> [idoc|os.path.join(global_state["tmpdir"], #{dquotes socketFileBasename})|]

genericMakeLet :: LangDescriptor -> (Int -> MDoc) -> Int -> PoolDocs -> PoolDocs -> PoolDocs
genericMakeLet desc namer i (PoolDocs ms1' e1' rs1 pes1) (PoolDocs ms2' e2' rs2 pes2) =
  let assignOp = case ldAssignment desc of
        EqualsAssign -> "="
        ArrowAssign -> "<-"
      rs = rs1 ++ [namer i <+> assignOp <+> e1'] ++ rs2
   in PoolDocs (ms1' <> ms2') e2' rs (pes1 <> pes2)

-- | Generic expression printer driven by descriptor
genericPrintExpr :: LangDescriptor -> IExpr -> MDoc
genericPrintExpr desc = go
  where
    go (IVar v) = pretty v
    go (IBoolLit True) = pretty (ldBoolTrue desc)
    go (IBoolLit False) = pretty (ldBoolFalse desc)
    go INullLit = pretty (ldNullLiteral desc)
    go (IIntLit i) = viaShow i
    go (IRealLit r) = viaShow r
    go (IStrLit s) = dquotes (pretty s)
    go (IListLit es) = case ldListStyle desc of
      BracketList -> list (map go es)
      FunctionList -> "list" <> tupled (map go es)
      RAtomicList -> "list" <> tupled (map go es)
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
    go (ISerCall schema e) =
      pretty (ldSerializeFn desc) <> "(" <> go e <> ", " <> dquotes (pretty schema) <> ")"
    go (IDesCall schema _ e) =
      pretty (ldDeserializeFn desc) <> "(" <> go e <> ", " <> dquotes (pretty schema) <> ")"
    go (IPack packer e) = pretty packer <> parens (go e)
    go (ICall f Nothing argGroups) =
      pretty f <> hsep (map (tupled . map go) argGroups)
    go (ICall f (Just _) argGroups) =
      pretty f <> hsep (map (tupled . map go) argGroups)
    go (IForeignCall _ _ _) = error "use IRawExpr for generic foreign calls"
    go (IRemoteCall _ _ _ _) = error "use IRawExpr for generic remote calls"
    go (ILambda args body) = case ldLambda desc of
      PythonLambda -> "lambda" <+> hsep (punctuate "," (map pretty args)) <> ":" <+> go body
      RFunction -> "function" <+> tupled (map pretty args) <> "{" <> go body <> "}"
      ArrowLambda -> tupled (map pretty args) <+> "->" <+> go body
    go (IRawExpr d) = pretty d
    go (ISuspend e) = case ldSuspend desc of
      PythonSuspend -> "lambda:" <+> go e
      RSuspend -> "function()" <+> go e
      ArrowSuspend -> "() ->" <+> go e
    go (IForce e) = go e <> "()"

-- | Generic statement printer driven by descriptor
genericPrintStmt :: LangDescriptor -> IStmt -> MDoc
genericPrintStmt desc = go
  where
    printE = genericPrintExpr desc
    assignOp = case ldAssignment desc of
      EqualsAssign -> "="
      ArrowAssign -> "<-"

    go (IAssign v Nothing e) = pretty v <+> assignOp <+> printE e
    go (IAssign v (Just _) e) = pretty v <+> assignOp <+> printE e
    go (IMapList resultVar _ iterVar collection bodyStmts yieldExpr) =
      case ldMapList desc of
        PythonForAppend -> vsep
          [ [idoc|#{pretty resultVar} = []|]
          , nest 4 (vsep
              ( [idoc|for #{pretty iterVar} in #{printE collection}:|]
              : map go bodyStmts
              ++ [[idoc|#{pretty resultVar}.append(#{printE yieldExpr})|]]
              ))
          ]
        RLapply ->
          block 4
            [idoc|#{pretty resultVar} <- lapply(#{printE collection}, function(#{pretty iterVar})|]
            (vsep (map go bodyStmts ++ [printE yieldExpr]))
            <> ")"
        Comprehension -> vsep
          [ pretty resultVar <+> assignOp <+> "[" <> printE yieldExpr
              <+> "for" <+> pretty iterVar <+> "in" <+> printE collection <> "]"
          ]
    go (IReturn e) = "return(" <> printE e <> ")"
    go (IExprStmt e) = printE e
    go (IFunDef _ _ _ _) = error "IFunDef not yet implemented for generic printer"

-- | Assemble a complete pool file from descriptor, template, and IProgram
printProgram :: LangDescriptor -> IProgram -> MDoc
printProgram desc prog =
  format
    (ldPoolTemplate desc)
    (ldBreakMarker desc)
    sections
  where
    sections = case ldDispatchTable desc of
      PythonDictDispatch ->
        [ vsep (map pretty (ipSources prog))
        , vsep (map pretty (ipManifolds prog))
        , pythonDispatch
        ]
      RListDispatch ->
        [ vsep (map pretty (ipSources prog))
        , vsep (map pretty (ipManifolds prog))
        , rDispatch
        ]
      ArrowDictDispatch ->
        [ vsep (map pretty (ipSources prog))
        , vsep (map pretty (ipManifolds prog))
        , arrowDictDispatch
        ]

    pythonDispatch = vsep [localD, remoteD]
      where
        localD = align . vsep $
          [ "dispatch = {"
          , indent 4 (vsep [pretty i <> ":" <+> manNamer i <> "," | DispatchEntry i _ <- ipLocalDispatch prog])
          , "}"
          ]
        remoteD = align . vsep $
          [ "remote_dispatch = {"
          , indent 4 (vsep [pretty i <> ":" <+> manNamer i <> "_remote" <> "," | DispatchEntry i _ <- ipRemoteDispatch prog])
          , "}"
          ]

    rDispatch = vsep [localD, remoteD]
      where
        localD = align . vsep $
          [ ".dispatch <- list()"
          , vsep [".dispatch[[" <> pretty i <> "L]] <-" <+> manNamer i | DispatchEntry i _ <- ipLocalDispatch prog]
          ]
        remoteD = align . vsep $
          [ ".remote_dispatch <- list()"
          , vsep [".remote_dispatch[[" <> pretty i <> "L]] <-" <+> manNamer i <> "_remote" | DispatchEntry i _ <- ipRemoteDispatch prog]
          ]

    arrowDictDispatch = vsep [localD, remoteD]
      where
        localD = align . vsep $
          [ "dispatch = Dict("
          , indent 4 (vsep [pretty i <+> "=>" <+> manNamer i <> "," | DispatchEntry i _ <- ipLocalDispatch prog])
          , ")"
          ]
        remoteD = align . vsep $
          [ "remote_dispatch = Dict("
          , indent 4 (vsep [pretty i <+> "=>" <+> manNamer i <> "_remote" <> "," | DispatchEntry i _ <- ipRemoteDispatch prog])
          , ")"
          ]

-- | Generic pattern evaluation
genericEvalPattern :: LangDescriptor -> TypeF -> Pattern -> [MDoc] -> MDoc
genericEvalPattern desc _ (PatternText firstStr fragments) xs =
  case ldPattern desc of
    PythonFString ->
      "f" <> (dquotes . hcat) (pretty firstStr : [("{" <> x <> "}" <> pretty s) | (x, s) <- zip xs fragments])
    RPaste0 ->
      "paste0" <> tupled (dquotes (pretty firstStr) : concat [[x, dquotes (pretty s)] | (x, s) <- zip xs fragments])
    DollarInterp ->
      "string" <> tupled (dquotes (pretty firstStr) : concat [[x, dquotes (pretty s)] | (x, s) <- zip xs fragments])
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
      pretty (ldRecordConstructor desc) <> tupled [makeRecordKey desc k <+> pretty (ldRecordSeparator desc) <+> x | (k, x) <- zip (map fst rs) xs]
    makeRecord _ _ = error "Incorrectly typed record setter"

    accessTuple _ m i = case ldIndexStyle desc of
      ZeroBracket -> m <> "[" <> pretty i <> "]"
      OneBracket -> m <> "[" <> pretty (i + 1) <> "]"
      OneDoubleBracket -> m <> "[[" <> pretty (i + 1) <> "]]"

    accessRecord (NamF o (FV _ cname) _ _) d k =
      genericRecordAccessor desc o cname d (pretty k)
    accessRecord t _ _ = error $ "Invalid record type: " <> show t
genericEvalPattern _ _ (PatternStruct _) [] = error "Unreachable empty pattern"

writeSelector :: LangDescriptor -> Either Int Text -> MDoc
writeSelector desc (Right k) = case ldFieldAccess desc of
  DotAccess -> "[" <> dquotes (pretty k) <> "]"
  DollarAccess -> "[[" <> dquotes (pretty k) <> "]]"
writeSelector desc (Left i) = case ldIndexStyle desc of
  ZeroBracket -> "[" <> pretty i <> "]"
  OneBracket -> "[" <> pretty (i + 1) <> "]"
  OneDoubleBracket -> "[[" <> pretty (i + 1) <> "]]"
