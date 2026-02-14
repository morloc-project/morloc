{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Nexus
Description : Generate a JSON manifest for the static nexus binary
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Nexus
  ( generate
  ) where

import qualified Control.Monad as CM
import qualified Control.Monad.State as CMS
import Data.Char (ord)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as MT
import Numeric (showHex)
import qualified Morloc.BaseTypes as MBT
import qualified Morloc.CodeGenerator.Infer as Infer
import Morloc.CodeGenerator.Namespace
import qualified Morloc.CodeGenerator.Serial as Serial
import qualified Morloc.Config as MC
import Morloc.Data.Doc (render, pretty)
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Data.Time.Clock.POSIX as Time
import qualified System.Directory as Dir


-- ======================================================================
-- Data types
-- ======================================================================

data FData = FData
  { fdataSocket :: Socket
  , fdataSubcommand :: Text
  , fdataMid :: Int
  , fdataType :: Type
  , fdataSubSockets :: [Socket]
  , fdataArgSchemas :: [Text]
  , fdataReturnSchema :: Text
  , fdataCmdDocSet :: CmdDocSet
  }

data GastData = GastData
  { commandName :: Text
  , commandType :: Type
  , commandDocs :: CmdDocSet
  , commandExpr :: NexusExpr
  , commandReturnSchema :: Text
  , commandArgSchemas :: [Text]
  }

data NexusExpr
  = AppX Text NexusExpr [NexusExpr]
  | LamX [Text] NexusExpr
  | BndX Text Text
  | PatX Text Pattern
  | LstX Text [NexusExpr]
  | TupX Text [NexusExpr]
  | NamX Text [(Text, NexusExpr)]
  | StrX Text Text
  | LitX LitType Text

data LitType = F32X | F64X | I8X | I16X | I32X | I64X | U8X | U16X | U32X | U64X | BoolX | NullX


-- ======================================================================
-- Data extraction
-- ======================================================================

makeFData ::
  (AnnoS (Indexed Type) One (Indexed Lang), CmdDocSet) ->
  MorlocMonad (Type, Int, Lang, CmdDocSet, [Socket])
makeFData (e@(AnnoS (Idx i t) (Idx _ lang) _), d) = do
  sockets <- findSockets e
  return (t, i, lang, d, sockets)

findSockets :: AnnoS e One (Indexed Lang) -> MorlocMonad [Socket]
findSockets rAST = do
  config <- MM.ask
  return . map (MC.setupServerAndSocket config) . unique $ findAllLangsSAnno rAST

findAllLangsSAnno :: AnnoS e One (Indexed Lang) -> [Lang]
findAllLangsSAnno (AnnoS _ (Idx _ lang) e) = lang : findAllLangsExpr e
  where
    findAllLangsExpr (VarS _ (One x)) = findAllLangsSAnno x
    findAllLangsExpr (AppS x xs) = concatMap findAllLangsSAnno (x : xs)
    findAllLangsExpr (LamS _ x) = findAllLangsSAnno x
    findAllLangsExpr (LstS xs) = concatMap findAllLangsSAnno xs
    findAllLangsExpr (TupS xs) = concatMap findAllLangsSAnno xs
    findAllLangsExpr (NamS rs) = concatMap (findAllLangsSAnno . snd) rs
    findAllLangsExpr _ = []

getFData :: (Type, Int, Lang, CmdDocSet, [Socket]) -> MorlocMonad FData
getFData (t, i, lang, doc, sockets) = do
  mayName <- MM.metaName i
  (argSchemas, returnSchema) <- makeSchemas i lang t

  case mayName of
    (Just name') -> do
      config <- MM.ask
      let socket = MC.setupServerAndSocket config lang
      return $
        FData
          { fdataSocket = socket
          , fdataSubcommand = maybe (unEVar name') id (cmdDocName doc)
          , fdataMid = i
          , fdataType = t
          , fdataSubSockets = sockets
          , fdataArgSchemas = map render argSchemas
          , fdataReturnSchema = render returnSchema
          , fdataCmdDocSet = doc
          }
    Nothing -> MM.throwSourcedError i $ "No name in FData"


-- ======================================================================
-- Schema building
-- ======================================================================

makeSchemas :: Int -> Lang -> Type -> MorlocMonad ([MDoc], MDoc)
makeSchemas mid lang (FunT ts t) = do
  ss <- mapM (makeSchema mid lang) ts
  s <- makeSchema mid lang t
  return (ss, s)
makeSchemas mid lang t = do
  s <- makeSchema mid lang t
  return ([], s)

makeSchema :: Int -> Lang -> Type -> MorlocMonad MDoc
makeSchema mid lang t = do
  ft <- Infer.inferConcreteTypeUniversal lang t
  ast <- Serial.makeSerialAST mid lang ft
  return $ Serial.serialAstToMsgpackSchema ast

makeGastSchemas :: Type -> MorlocMonad (MDoc, [MDoc])
makeGastSchemas (FunT ts t) = do
  serialAsts <- mapM generalTypeToSerialAST (t : ts)
  let (s : ss) = map Serial.serialAstToMsgpackSchema serialAsts
  return (s, ss)
makeGastSchemas t = do
  s <- Serial.serialAstToMsgpackSchema <$> generalTypeToSerialAST t
  return (s, [])

generalTypeToSerialAST :: Type -> MorlocMonad SerialAST
generalTypeToSerialAST (VarT v)
  | v == MBT.real = return $ SerialReal (FV v (CV ""))
  | v == MBT.f32 = return $ SerialReal (FV v (CV ""))
  | v == MBT.f64 = return $ SerialReal (FV v (CV ""))
  | v == MBT.int = return $ SerialInt (FV v (CV ""))
  | v == MBT.i8 = return $ SerialInt8 (FV v (CV ""))
  | v == MBT.i16 = return $ SerialInt16 (FV v (CV ""))
  | v == MBT.i32 = return $ SerialInt32 (FV v (CV ""))
  | v == MBT.i64 = return $ SerialInt64 (FV v (CV ""))
  | v == MBT.u8 = return $ SerialUInt8 (FV v (CV ""))
  | v == MBT.u16 = return $ SerialUInt16 (FV v (CV ""))
  | v == MBT.u32 = return $ SerialUInt32 (FV v (CV ""))
  | v == MBT.u64 = return $ SerialUInt64 (FV v (CV ""))
  | v == MBT.bool = return $ SerialBool (FV v (CV ""))
  | v == MBT.str = return $ SerialString (FV v (CV ""))
  | v == MBT.unit = return $ SerialNull (FV v (CV ""))
  | otherwise = do
      scope <- MM.gets stateUniversalGeneralTypedefs
      case Map.lookup v scope of
        (Just [(_, _, _, True)]) -> error "Cannot handle terminal types"
        (Just [([], t', _, False)]) -> generalTypeToSerialAST (typeOf t')
        (Just [_]) -> error $ "Cannot currently handle parameterized pure morloc types"
        Nothing -> error $ "Failed to interpret type variable: " <> show (unTVar v)
        x -> error $ "Unexpected scope: " <> show x
generalTypeToSerialAST (AppT (VarT v) [t])
  | v == MBT.list = SerialList (FV v (CV "")) <$> generalTypeToSerialAST t
  | otherwise = do
      insts <- MM.gets stateTypeclasses
      error $ show insts
generalTypeToSerialAST (AppT (VarT v) ts)
  | v == (MBT.tuple (length ts)) = SerialTuple (FV v (CV "")) <$> mapM generalTypeToSerialAST ts
  | otherwise = do
      insts <- MM.gets stateTypeclasses
      error $ show insts
generalTypeToSerialAST (NamT o v [] rs) =
  SerialObject o (FV v (CV "")) []
    <$> mapM (secondM generalTypeToSerialAST) rs
generalTypeToSerialAST t = error $ "cannot serialize this type: " <> show t


-- ======================================================================
-- Pure expression extraction
-- ======================================================================

annotateGasts :: (AnnoS (Indexed Type) One (), CmdDocSet) -> MorlocMonad GastData
annotateGasts (x0@(AnnoS (Idx i gtype) _ _), docs) = do
  mayName <- MM.metaName i
  gname <- case mayName of
    Nothing -> MM.throwSourcedError i $ "No name found for call-free function"
    (Just n') -> return n'

  (retSchemaDoc, argSchemaDocs) <- makeGastSchemas gtype
  expr <- toNexusExpr x0

  return $
    GastData
      { commandName = maybe (unEVar gname) id (cmdDocName docs)
      , commandType = gtype
      , commandDocs = docs
      , commandExpr = expr
      , commandReturnSchema = render retSchemaDoc
      , commandArgSchemas = map render argSchemaDocs
      }
  where
    type2schema :: Type -> MorlocMonad Text
    type2schema t = (render . Serial.serialAstToMsgpackSchema) <$> generalTypeToSerialAST t

    toNexusExpr :: AnnoS (Indexed Type) One () -> MorlocMonad NexusExpr
    toNexusExpr (AnnoS (Idx _ t) _ (AppS e es)) = AppX <$> type2schema t <*> toNexusExpr e <*> mapM toNexusExpr es
    toNexusExpr (AnnoS _ _ (LamS vs e)) = LamX (map (render . pretty) vs) <$> toNexusExpr e
    toNexusExpr (AnnoS (Idx _ (FunT _ t)) _ (ExeS (PatCall p))) = PatX <$> type2schema t <*> pure p
    toNexusExpr (AnnoS (Idx _ t) _ (BndS v)) = BndX <$> type2schema t <*> pure (render (pretty v))
    toNexusExpr (AnnoS (Idx _ t) _ (LstS es)) = LstX <$> type2schema t <*> mapM toNexusExpr es
    toNexusExpr (AnnoS (Idx _ t) _ (TupS es)) = TupX <$> type2schema t <*> mapM toNexusExpr es
    toNexusExpr (AnnoS (Idx _ t) _ (NamS rs)) =
      NamX <$> type2schema t <*> mapM (\(k, e) -> (,) (unKey k) <$> toNexusExpr e) rs
    toNexusExpr (AnnoS (Idx _ t) _ (StrS v)) = StrX <$> type2schema t <*> pure v
    toNexusExpr (AnnoS (Idx _ t) _ (RealS v)) = do
      s <- generalTypeToSerialAST t
      return $ case s of
        (SerialFloat32 _) -> LitX F32X (MT.pack (show v))
        _ -> LitX F64X (MT.pack (show v))
    toNexusExpr (AnnoS (Idx _ t) _ (IntS v)) = do
      s <- generalTypeToSerialAST t
      return $ case s of
        (SerialInt8 _) -> LitX I8X (MT.pack (show v))
        (SerialInt16 _) -> LitX I16X (MT.pack (show v))
        (SerialInt _) -> LitX I32X (MT.pack (show v))
        (SerialInt32 _) -> LitX I32X (MT.pack (show v))
        (SerialInt64 _) -> LitX I64X (MT.pack (show v))
        (SerialUInt8 _) -> LitX U8X (MT.pack (show v))
        (SerialUInt16 _) -> LitX U16X (MT.pack (show v))
        (SerialUInt _) -> LitX U32X (MT.pack (show v))
        (SerialUInt32 _) -> LitX U32X (MT.pack (show v))
        (SerialUInt64 _) -> LitX U64X (MT.pack (show v))
        _ -> LitX I64X (MT.pack (show v))
    toNexusExpr (AnnoS _ _ (LogS True)) = return $ LitX BoolX "1"
    toNexusExpr (AnnoS _ _ (LogS False)) = return $ LitX BoolX "0"
    toNexusExpr (AnnoS _ _ UniS) = return $ LitX NullX "0"
    toNexusExpr _ = error $ "Unreachable value of type reached"


-- ======================================================================
-- JSON helpers
-- ======================================================================

jsonEscape :: Text -> Text
jsonEscape = MT.concatMap esc
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc '\b' = "\\b"
    esc '\f' = "\\f"
    esc c | c < ' ' = "\\u" <> MT.pack (pad4 (showHex (ord c) ""))
    esc c = MT.singleton c

    pad4 s = replicate (4 - length s) '0' ++ s

jsonStr :: Text -> Text
jsonStr t = "\"" <> jsonEscape t <> "\""

jsonInt :: Int -> Text
jsonInt = MT.pack . show

jsonBool :: Bool -> Text
jsonBool True = "true"
jsonBool False = "false"

jsonNull :: Text
jsonNull = "null"

jsonArr :: [Text] -> Text
jsonArr xs = "[" <> MT.intercalate "," xs <> "]"

jsonObj :: [(Text, Text)] -> Text
jsonObj pairs = "{" <> MT.intercalate "," [jsonStr k <> ":" <> v | (k, v) <- pairs] <> "}"

jsonStrArr :: [Text] -> Text
jsonStrArr = jsonArr . map jsonStr

jsonMaybeStr :: Maybe Text -> Text
jsonMaybeStr Nothing = jsonNull
jsonMaybeStr (Just t) = jsonStr t


-- ======================================================================
-- CLI argument serialization
-- ======================================================================

argToJson :: CmdArg -> Text
argToJson (CmdArgPos r) = jsonObj
  [ ("kind", jsonStr "pos")
  , ("metavar", jsonMaybeStr (argPosDocMetavar r))
  , ("type_desc", jsonStr (typeDescStr (argPosDocType r) (argPosDocLiteral r)))
  , ("quoted", jsonBool (argPosDocLiteral r == Just True && argPosDocType r == VarT MBT.str))
  , ("desc", jsonStrArr (argPosDocDesc r))
  ]
argToJson (CmdArgOpt r) = jsonObj
  [ ("kind", jsonStr "opt")
  , ("metavar", jsonStr (argOptDocMetavar r))
  , ("type_desc", jsonStr (typeDescStr (argOptDocType r) (argOptDocLiteral r)))
  , ("quoted", jsonBool (argOptDocLiteral r == Just True && argOptDocType r == VarT MBT.str))
  , ("short", cliOptShortJson (argOptDocArg r))
  , ("long", cliOptLongJson (argOptDocArg r))
  , ("default", jsonStr (argOptDocDefault r))
  , ("desc", jsonStrArr (argOptDocDesc r))
  ]
argToJson (CmdArgFlag r) = jsonObj
  [ ("kind", jsonStr "flag")
  , ("short", cliOptShortJson (argFlagDocOpt r))
  , ("long", cliOptLongJson (argFlagDocOpt r))
  , ("long_rev", flagRevJson (argFlagDocOptRev r))
  , ("default", jsonStr (argFlagDocDefault r))
  , ("desc", jsonStrArr (argFlagDocDesc r))
  ]
argToJson (CmdArgGrp r) = jsonObj
  [ ("kind", jsonStr "grp")
  , ("metavar", jsonStr (recDocMetavar r))
  , ("desc", jsonStrArr (recDocDesc r))
  , ("group_opt", grpOptJson (recDocOpt r))
  , ("entries", jsonArr [grpEntryJson k v | (k, v) <- recDocEntries r])
  ]
  where
    grpOptJson Nothing = jsonNull
    grpOptJson (Just opt) = jsonObj
      [ ("short", cliOptShortJson opt)
      , ("long", cliOptLongJson opt)
      ]

    grpEntryJson key entry = jsonObj
      [ ("key", jsonStr (unKey key))
      , ("arg", argToJson (either CmdArgFlag CmdArgOpt entry))
      ]

typeDescStr :: Type -> Maybe Bool -> Text
typeDescStr t isLiteral
  | t == VarT MBT.str && isLiteral /= Just True = "Str    (a filename or quoted JSON string)"
  | otherwise = render (pretty t)

cliOptShortJson :: CliOpt -> Text
cliOptShortJson (CliOptShort c) = jsonStr (MT.singleton c)
cliOptShortJson (CliOptBoth c _) = jsonStr (MT.singleton c)
cliOptShortJson _ = jsonNull

cliOptLongJson :: CliOpt -> Text
cliOptLongJson (CliOptLong l) = jsonStr l
cliOptLongJson (CliOptBoth _ l) = jsonStr l
cliOptLongJson _ = jsonNull

flagRevJson :: Maybe CliOpt -> Text
flagRevJson Nothing = jsonNull
flagRevJson (Just (CliOptLong l)) = jsonStr l
flagRevJson (Just (CliOptBoth _ l)) = jsonStr l
flagRevJson _ = jsonNull


-- ======================================================================
-- Expression tree serialization
-- ======================================================================

exprToJson :: NexusExpr -> Text
exprToJson (LitX lt val) = jsonObj
  [ ("tag", jsonStr "lit")
  , ("schema", jsonStr (litSchemaStr lt))
  , ("lit_type", jsonStr (litSchemaStr lt))
  , ("value", jsonStr val)
  ]
exprToJson (StrX schema val) = jsonObj
  [ ("tag", jsonStr "str")
  , ("schema", jsonStr schema)
  , ("value", jsonStr val)
  ]
exprToJson (LstX schema es) = jsonObj
  [ ("tag", jsonStr "container")
  , ("schema", jsonStr schema)
  , ("elements", jsonArr (map exprToJson es))
  ]
exprToJson (TupX schema es) = jsonObj
  [ ("tag", jsonStr "container")
  , ("schema", jsonStr schema)
  , ("elements", jsonArr (map exprToJson es))
  ]
exprToJson (NamX schema entries) = jsonObj
  [ ("tag", jsonStr "container")
  , ("schema", jsonStr schema)
  , ("elements", jsonArr (map (exprToJson . snd) entries))
  ]
exprToJson (AppX schema func args) = jsonObj
  [ ("tag", jsonStr "app")
  , ("schema", jsonStr schema)
  , ("func", exprToJson func)
  , ("args", jsonArr (map exprToJson args))
  ]
exprToJson (LamX vars body) = jsonObj
  [ ("tag", jsonStr "lambda")
  , ("vars", jsonStrArr vars)
  , ("body", exprToJson body)
  ]
exprToJson (BndX schema var) = jsonObj
  [ ("tag", jsonStr "bound")
  , ("schema", jsonStr schema)
  , ("var", jsonStr var)
  ]
exprToJson (PatX schema (PatternText p ps)) = jsonObj
  [ ("tag", jsonStr "interpolation")
  , ("schema", jsonStr schema)
  , ("strings", jsonStrArr (p : ps))
  ]
exprToJson (PatX schema (PatternStruct sel)) = jsonObj
  [ ("tag", jsonStr "pattern")
  , ("schema", jsonStr schema)
  , ("pattern", selectorToJson sel)
  ]

selectorToJson :: Selector -> Text
selectorToJson SelectorEnd = jsonObj [("type", jsonStr "end")]
selectorToJson (SelectorIdx t ts) = jsonObj
  [ ("type", jsonStr "idx")
  , ("selectors", jsonArr [idxSel i s | (i, s) <- t : ts])
  ]
  where
    idxSel i sub = jsonObj
      [ ("index", jsonInt i)
      , ("sub", selectorToJson sub)
      ]
selectorToJson (SelectorKey t ts) = jsonObj
  [ ("type", jsonStr "key")
  , ("selectors", jsonArr [keySel k s | (k, s) <- t : ts])
  ]
  where
    keySel k sub = jsonObj
      [ ("key", jsonStr k)
      , ("sub", selectorToJson sub)
      ]

litSchemaStr :: LitType -> Text
litSchemaStr F32X = "f4"
litSchemaStr F64X = "f8"
litSchemaStr I8X = "i1"
litSchemaStr I16X = "i2"
litSchemaStr I32X = "i4"
litSchemaStr I64X = "i8"
litSchemaStr U8X = "u1"
litSchemaStr U16X = "u2"
litSchemaStr U32X = "u4"
litSchemaStr U64X = "u8"
litSchemaStr BoolX = "b"
litSchemaStr NullX = "z"


-- ======================================================================
-- Manifest builder
-- ======================================================================

buildManifest :: Config -> String -> Int -> [(Lang, Socket)] -> [FData] -> [GastData] -> (Lang -> Int) -> Text
buildManifest config buildDir buildTime daemonSets fdata gasts langToPool = jsonObj
  [ ("version", "1")
  , ("build_dir", jsonStr (MT.pack buildDir))
  , ("build_time", jsonInt buildTime)
  , ("pools", jsonArr (map poolJson daemonSets))
  , ("commands", jsonArr (map remoteCmdJson fdata ++ map pureCmdJson gasts))
  ]
  where
    poolJson :: (Lang, Socket) -> Text
    poolJson (lang, _) = jsonObj
      [ ("lang", jsonStr (ML.showLangName lang))
      , ("exec", jsonStrArr (map MT.pack (makeExecArgs lang)))
      , ("socket", jsonStr ("pipe-" <> ML.showLangName lang))
      ]

    makeExecArgs :: Lang -> [String]
    makeExecArgs CppLang = ["pools" </> ML.makeExecutablePoolName CppLang]
    makeExecArgs CLang = ["pools" </> ML.makeExecutablePoolName CLang]
    makeExecArgs Python3Lang = [MC.configLangPython3 config, "pools" </> ML.makeExecutablePoolName Python3Lang]
    makeExecArgs RLang = [MC.configLangR config, "pools" </> ML.makeExecutablePoolName RLang]

    remoteCmdJson :: FData -> Text
    remoteCmdJson fd = jsonObj
      [ ("name", jsonStr (fdataSubcommand fd))
      , ("type", jsonStr "remote")
      , ("mid", jsonInt (fdataMid fd))
      , ("pool", jsonInt (langToPool (socketLang (fdataSocket fd))))
      , ("needed_pools", jsonArr (map (jsonInt . langToPool . socketLang) (fdataSubSockets fd)))
      , ("arg_schemas", jsonStrArr (fdataArgSchemas fd))
      , ("return_schema", jsonStr (fdataReturnSchema fd))
      , ("desc", jsonStrArr (cmdDocDesc (fdataCmdDocSet fd)))
      , ("return_type", jsonStr (returnTypeStr (fdataType fd)))
      , ("return_desc", jsonStrArr (snd (cmdDocRet (fdataCmdDocSet fd))))
      , ("args", jsonArr (map argToJson (cmdDocArgs (fdataCmdDocSet fd))))
      ]

    pureCmdJson :: GastData -> Text
    pureCmdJson g = jsonObj
      [ ("name", jsonStr (commandName g))
      , ("type", jsonStr "pure")
      , ("arg_schemas", jsonStrArr (commandArgSchemas g))
      , ("return_schema", jsonStr (commandReturnSchema g))
      , ("desc", jsonStrArr (cmdDocDesc (commandDocs g)))
      , ("return_type", jsonStr (returnTypeStr (commandType g)))
      , ("return_desc", jsonStrArr (snd (cmdDocRet (commandDocs g))))
      , ("args", jsonArr (map argToJson (cmdDocArgs (commandDocs g))))
      , ("expr", exprToJson (commandExpr g))
      ]

    returnTypeStr :: Type -> Text
    returnTypeStr (FunT _ t) = render (pretty t)
    returnTypeStr t = render (pretty t)


-- ======================================================================
-- Main entry point
-- ======================================================================

generate ::
  [(AnnoS (Indexed Type) One (), CmdDocSet)] ->
  [(AnnoS (Indexed Type) One (Indexed Lang), CmdDocSet)] ->
  MorlocMonad Script
generate cs rASTs = do
  config <- MM.ask
  st <- CMS.get

  -- Extract data for remote commands
  xs <- mapM makeFData rASTs
  fdata <- CM.mapM getFData xs

  -- Extract data for pure commands
  gasts <- mapM annotateGasts cs

  -- Get build time and working directory
  buildTime <- liftIO $ floor <$> Time.getPOSIXTime
  buildDir <- liftIO Dir.getCurrentDirectory

  -- Build pool list (deduplicated by language)
  let allSockets = concatMap (\x -> fdataSocket x : fdataSubSockets x) fdata
      daemonSets = uniqueFst [(socketLang s, s) | s <- allSockets]

      langToPoolIndex :: Lang -> Int
      langToPoolIndex lang =
        case findIndex ((== lang) . fst) daemonSets of
          Just idx -> idx
          Nothing -> error $ "Pool not found for language: " <> show lang

  -- Build manifest JSON with relative pool paths
  let manifestJson = buildManifest config buildDir buildTime daemonSets fdata gasts langToPoolIndex
      outfile = fromMaybe "nexus" (stateOutfile st)
      wrapperScript = makeWrapperScript manifestJson

  return $
    Script
      { scriptBase = outfile
      , scriptLang = ML.CLang
      , scriptCode = "." :/ File outfile (Code wrapperScript)
      , scriptMake = [SysExe outfile]
      }

-- Build a self-contained wrapper script with embedded manifest
makeWrapperScript :: Text -> Text
makeWrapperScript manifestJson =
  "#!/bin/sh\nexec mim \"$0\" \"$@\"\n### MANIFEST ###\n" <> manifestJson



-- ======================================================================
-- Utilities
-- ======================================================================

uniqueFst :: (Eq a) => [(a, b)] -> [(a, b)]
uniqueFst = f []
  where
    f _ [] = []
    f seen (x@(a, _) : xs)
      | a `elem` seen = f seen xs
      | otherwise = x : f (a : seen) xs
