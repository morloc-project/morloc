{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Translator.Generic
Description : Descriptor-driven translator for plugin languages
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Generic translator that generates pool code for dynamically-typed interpreted
languages based on a LangDescriptor. Handles Tier 1 (easy) languages that
have Python/R-like semantics: dynamic typing, no templates, simple modules.
-}
module Morloc.CodeGenerator.Grammars.Translator.Generic
  ( translate
  , preprocess
  ) where

import Data.Text (Text)
import qualified Data.Text as T
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
import qualified Morloc.Language as ML
import Morloc.Monad (asks, gets, liftIO, newIndex, runIndex)
import qualified Morloc.Monad as MM
import Morloc.Quasi

preprocess :: SerialManifold -> MorlocMonad SerialManifold
preprocess = return . invertSerialManifold

translate :: Lang -> [Source] -> [SerialManifold] -> MorlocMonad Script
translate lang srcs es = do
  desc <- loadDescriptorForLang lang

  home <- pretty <$> asks MC.configHome
  let opt = home <> "/opt"

  includeDocs <-
    mapM
      (translateSource desc)
      (unique . mapMaybe srcPath $ srcs)

  debugLog (vsep (map pseudocodeSerialManifold es) <> "\n")

  let mDocs = map (translateSegment desc) es
      program = buildProgram includeDocs mDocs es

  let code = printProgram desc program
  let exefile = ML.makeExecutablePoolName lang

  return $
    Script
      { scriptBase = "pool"
      , scriptLang = lang
      , scriptCode = "." :/ Dir "pools" [File exefile (Code . render $ code)]
      , scriptMake = []
      }

-- | Load the language descriptor for a plugin language.
-- If the descriptor's pool template is empty, load it from a pool.<ext> file
-- in the same directory as lang.yaml.
loadDescriptorForLang :: Lang -> MorlocMonad LangDescriptor
loadDescriptorForLang (PluginLang pli) = do
  home <- asks MC.configHome
  let langName = MT.unpack (ML.pliName pli)
      langDir = home </> "lang" </> langName
      descPath = langDir </> "lang.yaml"
  result <- liftIO $ loadLangDescriptor descPath
  case result of
    Left err -> MM.throwSystemError $
      "Failed to load language descriptor for " <> pretty (ML.pliName pli)
      <> ": " <> pretty err
    Right desc -> do
      -- Load pool template from disk if not inline in the descriptor
      desc' <- if T.null (ldPoolTemplate desc)
        then do
          let poolPath = langDir </> "pool." <> ML.pliExtension pli
          poolText <- liftIO $ MT.readFile poolPath
          return desc { ldPoolTemplate = poolText }
        else return desc
      return desc'
loadDescriptorForLang lang =
  MM.throwSystemError $ "loadDescriptorForLang called for non-plugin language: " <> viaShow lang

debugLog :: Doc ann -> MorlocMonad ()
debugLog d = do
  verbosity <- gets stateVerbosity
  when (verbosity > 0) $ (liftIO . putDoc) d

translateSource :: LangDescriptor -> Path -> MorlocMonad MDoc
translateSource desc p = do
  let p' = MT.stripPrefixIfPresent "./" (MT.pack p)
      -- Pool files live in pools/ subdirectory; if the language's include
      -- resolves relative to the file (not CWD), we need to go up one level
      p'' = if ldIncludeRelToFile desc then "../" <> p' else p'
  case ldFunctionDef desc of
    RAssignDef -> return $ "source(" <> dquotes (pretty p'') <> ")"
    EndBlockDef -> return $ "include(" <> dquotes (pretty p'') <> ")"
    _ -> do
      lib <- MT.pack <$> asks MC.configLibrary
      return $ makeNamespace lib p <+> "=" <+> "importlib.import_module("
        <> dquotes (makeImportPath lib p) <> ")"

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

translateSegment :: LangDescriptor -> SerialManifold -> MDoc
translateSegment desc m0 =
  let cfg = genericLowerConfig desc
   in renderPoolDocs $ runIndex 0 (foldWithSerialManifoldM (defaultFoldRules cfg) m0)

-- | Build a LowerConfig from a LangDescriptor
genericLowerConfig :: LangDescriptor -> LowerConfig IndexM
genericLowerConfig desc = cfg
  where
    cfg = LowerConfig
      { lcSrcName = \src -> pretty (srcName src)
      , lcTypeOf = \_ -> return Nothing
      , lcSerialAstType = \_ -> return Nothing
      , lcDeserialAstType = \_ -> return Nothing
      , lcRawDeserialAstType = \_ -> return Nothing
      , lcTemplateArgs = \_ -> return Nothing
      , lcTypeMOf = \_ -> return Nothing
      , lcPackerName = \src -> pretty (srcName src)
      , lcUnpackerName = \src -> pretty (srcName src)
      , lcRecordAccessor = \_ _ record field -> case ldFieldAccess desc of
          DotAccess -> record <> "." <> field
          DollarAccess -> record <> "$" <> field
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
              <> tupled [dquotes (pretty k) <+> pretty (ldRecordSeparator desc) <+> v | (k, v) <- rs] }
      , lcForeignCall = \socketFile mid args ->
          let midDoc = pretty mid <> pretty (ldForeignCallIntSuffix desc)
              argsDoc = case ldListStyle desc of
                BracketList -> list args
                _ -> "list" <> tupled args
           in pretty (ldForeignCallFn desc)
              <> tupled [makeGenericSocketPath desc socketFile, midDoc, argsDoc]
      , lcRemoteCall = \socketFile mid res args -> do
          let resMem = pretty $ remoteResourcesMemory res
              resTime = pretty $ remoteResourcesTime res
              resCPU = pretty $ remoteResourcesThreads res
              resGPU = pretty $ remoteResourcesGpus res
              call = pretty (ldForeignCallFn desc)
                <> tupled [pretty mid, dquotes socketFile, dquotes ".morloc-cache"
                          , list [resMem, resTime, resCPU, resGPU], list args]
          return $ defaultValue {poolExpr = call}
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
    go (IVar v) = v
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
        <> tupled [dquotes (pretty k) <+> pretty (ldRecordSeparator desc) <+> go e | (k, e) <- entries]
    go (IAccess e (IIdx i)) = case ldIndexStyle desc of
      ZeroBracket -> go e <> "[" <> pretty i <> "]"
      OneBracket -> go e <> "[" <> pretty (i + 1) <> "]"
      OneDoubleBracket -> go e <> "[[" <> pretty (i + 1) <> "]]"
    go (IAccess e (IKey k)) = case ldKeyAccess desc of
      "double_bracket" -> go e <> "[[" <> dquotes (pretty k) <> "]]"
      _ -> go e <> "[" <> dquotes (pretty k) <> "]"
    go (IAccess e (IField f)) = case ldFieldAccess desc of
      DotAccess -> go e <> "." <> f
      DollarAccess -> go e <> "$" <> f
    go (ISerCall schema e) =
      pretty (ldSerializeFn desc) <> "(" <> go e <> ", " <> dquotes schema <> ")"
    go (IDesCall schema _ e) =
      pretty (ldDeserializeFn desc) <> "(" <> go e <> ", " <> dquotes schema <> ")"
    go (IPack packer e) = packer <> parens (go e)
    go (ICall f Nothing argGroups) =
      f <> hsep (map (tupled . map go) argGroups)
    go (ICall f (Just _) argGroups) =
      f <> hsep (map (tupled . map go) argGroups)
    go (IForeignCall _ _ _) = error "use IRawExpr for generic foreign calls"
    go (IRemoteCall _ _ _ _) = error "use IRawExpr for generic remote calls"
    go (ILambda args body) = case ldLambda desc of
      PythonLambda -> "lambda" <+> hsep (punctuate "," args) <> ":" <+> go body
      RFunction -> "function" <+> tupled args <> "{" <> go body <> "}"
      ArrowLambda -> tupled args <+> "->" <+> go body
    go (IRawExpr d) = d
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

    go (IAssign v Nothing e) = v <+> assignOp <+> printE e
    go (IAssign v (Just _) e) = v <+> assignOp <+> printE e
    go (IMapList resultVar _ iterVar collection bodyStmts yieldExpr) =
      case ldMapList desc of
        PythonForAppend -> vsep
          [ [idoc|#{resultVar} = []|]
          , nest 4 (vsep
              ( [idoc|for #{iterVar} in #{collection}:|]
              : map go bodyStmts
              ++ [[idoc|#{resultVar}.append(#{printE yieldExpr})|]]
              ))
          ]
        RLapply ->
          block 4
            [idoc|#{resultVar} <- lapply(#{collection}, function(#{iterVar})|]
            (vsep (map go bodyStmts ++ [printE yieldExpr]))
            <> ")"
        Comprehension -> vsep
          [ resultVar <+> assignOp <+> "[" <> printE yieldExpr
              <+> "for" <+> iterVar <+> "in" <+> collection <> "]"
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
        [ vsep (ipSources prog)
        , vsep (ipManifolds prog)
        , pythonDispatch
        ]
      RListDispatch ->
        [ vsep (ipSources prog)
        , vsep (ipManifolds prog)
        , rDispatch
        ]
      ArrowDictDispatch ->
        [ vsep (ipSources prog)
        , vsep (ipManifolds prog)
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
      pretty (ldRecordConstructor desc) <> tupled [dquotes (pretty k) <+> pretty (ldRecordSeparator desc) <+> x | (k, x) <- zip (map fst rs) xs]
    makeRecord _ _ = error "Incorrectly typed record setter"

    accessTuple _ m i = case ldIndexStyle desc of
      ZeroBracket -> m <> "[" <> pretty i <> "]"
      OneBracket -> m <> "[" <> pretty (i + 1) <> "]"
      OneDoubleBracket -> m <> "[[" <> pretty (i + 1) <> "]]"

    accessRecord (NamF o (FV _ _) _ _) d k = case ldFieldAccess desc of
      DotAccess -> d <> "[" <> dquotes (pretty k) <> "]"
      DollarAccess -> d <> "[[" <> dquotes (pretty k) <> "]]"
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
