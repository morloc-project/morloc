{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Nexus
Description : Templates for generating the nexus
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.CodeGenerator.Nexus
  ( generate
  ) where

import qualified Control.Monad.State as CMS
import Morloc.Data.Doc
import Morloc.DataFiles as DF
import Morloc.CodeGenerator.Namespace
import Morloc.Quasi
import qualified Data.Map as Map
import qualified Morloc.Data.Text as MT
import Data.Text (Text)
import qualified Control.Monad as CM
import qualified Morloc.Config as MC
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.BaseTypes as MBT
import qualified Morloc.CodeGenerator.Infer as Infer
import qualified Morloc.CodeGenerator.Serial as Serial
import Data.Char (ord)
import Text.Printf (printf)

data FData = FData
  { fdataSocket :: Socket
  , fdataSubcommand :: MDoc -- subcommand name
  , fdataSubcommandLength :: Int -- subcommand length
  , fdataMid :: Int -- manifold ID
  , fdataType :: Type -- argument type
  , fdataSubSockets :: [Socket] -- list of sockets needed for this command
  , fdataArgSchemas :: [MDoc] -- argument type schemas
  , fdataReturnSchema :: MDoc -- return type schema
  , fdataCmdDocSet :: CmdDocSet
  }

-- | Description of a pure morloc expression
data GastData = GastData
  { commandIndex :: Int -- ^ top index for the command
  , commandName :: EVar -- ^ user-exposed subcommand name in the nexus
  , commandType :: Type -- ^ the general type of the expression
  , commandArgs :: [EVar] -- ^ list of function arguments
  , commandDocs :: CmdDocSet -- ^ docstrings
  , commandExpr :: ([MDoc], MDoc)
  -- ^ lines of code setting up the expression and the final variable name
  , commandSchemas :: (MDoc, [MDoc])
  }

type SchemaStr = MDoc

-- | A data type that stores pure morloc expressions
data NexusExpr
  -- expressions that must be evaluated to data
  = AppX SchemaStr NexusExpr [NexusExpr]
  | LamX [MDoc] NexusExpr
  | BndX SchemaStr MDoc
  | PatX SchemaStr Pattern
  -- literal data
  | LstX SchemaStr [NexusExpr]
  | TupX SchemaStr [NexusExpr]
  | NamX SchemaStr [(MDoc, NexusExpr)]
  | StrX SchemaStr MDoc
  | LitX LitType MDoc

data LitType = F32X | F64X | I8X | I16X | I32X | I64X | U8X | U16X | U32X | U64X | BoolX | NullX


generate :: [(AnnoS (Indexed Type) One (), CmdDocSet)] -> [(Type, Int, Lang, CmdDocSet, [Socket])] -> MorlocMonad Script
generate cs xs = do

  config <- MM.ask

  gasts <- mapM annotateGasts cs

  -- find the path for extensions
  -- this includes the mlcmpack module needed for MessagePack handling
  let home = MC.configHome config
      includeDir = home </> "include"

  fdata <- CM.mapM getFData xs -- [FData]

  -- get the length of the longest subcommand name (needed for alignment)
  let allSubcommandsLengths = map fdataSubcommandLength fdata <> map (MT.length . unEVar . commandName) gasts
  let longestSubcommand = if length allSubcommandsLengths > 0
                          then maximum allSubcommandsLengths
                          else 0

  outfile <- CMS.gets (fromMaybe "nexus" . stateOutfile)
  let nexusfile = "nexus.c"
  return $
    Script
      { scriptBase = nexusfile
      , scriptLang = ML.CLang
      , scriptCode = "." :/ File nexusfile (Code . render $ main config fdata longestSubcommand gasts)
      , scriptMake = [SysRun . Code $ "gcc -o " <> MT.pack outfile <> " -O -I" <> MT.pack includeDir <> " " <> MT.pack nexusfile]
      }

annotateGasts :: (AnnoS (Indexed Type) One (), CmdDocSet) -> MorlocMonad GastData
annotateGasts (x0@(AnnoS (Idx i gtype) _ _), docs) = do
  mayName <- MM.metaName i
  gname <- case mayName of
    Nothing -> MM.throwError . OtherError $ "No name found for call-free function"
    (Just n') -> return n'

  let gargs = findArgs x0

  schemas <- makeGastSchemas gtype

  expr <- toNexusExpr x0 |>> makePureExpression i

  return $ GastData
    { commandIndex = i
    , commandName = maybe gname EV (cmdDocName docs)
    , commandType = gtype
    , commandArgs = gargs
    , commandDocs = docs
    , commandExpr = expr
    , commandSchemas = schemas
    }

  where
    findArgs (AnnoS _ _ (LamS vs _)) = vs
    findArgs _ = []

    type2schema t = generalTypeToSerialAST t |>> Serial.serialAstToMsgpackSchema |>> dquotes

    -- make potentially multi-line strings legal C literals
    escapeString = MT.concatMap escapeChar
      where
        escapeChar '"'  = "\\\""
        escapeChar '\\' = "\\\\"
        escapeChar '\n' = "\\n"
        escapeChar '\r' = "\\r"
        escapeChar '\t' = "\\t"
        escapeChar '\b' = "\\b"
        escapeChar '\f' = "\\f"
        escapeChar '\v' = "\\v"
        escapeChar c
          | c < ' ' || c == '\DEL' = MT.pack $ "\\x" ++ printf "%02x" (ord c)
          | otherwise = MT.singleton c

    toNexusExpr :: AnnoS (Indexed Type) One () -> MorlocMonad NexusExpr
    toNexusExpr (AnnoS (Idx _ t) _ (AppS e es)) = AppX <$> type2schema t <*> toNexusExpr e <*> mapM toNexusExpr es
    toNexusExpr (AnnoS _ _ (LamS vs e)) = LamX (map pretty vs) <$> toNexusExpr e
    toNexusExpr (AnnoS (Idx _ (FunT _ t)) _ (ExeS (PatCall p))) = PatX <$> type2schema t <*> pure p
    toNexusExpr (AnnoS (Idx _ t) _ (BndS v)) = BndX <$> type2schema t <*> pure (pretty v)
    toNexusExpr (AnnoS (Idx _ t) _ (LstS es)) = LstX <$> type2schema t <*> mapM toNexusExpr es
    toNexusExpr (AnnoS (Idx _ t) _ (TupS es)) = TupX <$> type2schema t <*> mapM toNexusExpr es
    toNexusExpr (AnnoS (Idx _ t) _ (NamS rs)) = NamX <$> type2schema t <*> mapM (bimapM (pure . pretty) toNexusExpr) rs
    toNexusExpr (AnnoS (Idx _ t) _ (StrS v)) = StrX <$> type2schema t <*> (pure . dquotes . pretty . escapeString $ v)
    toNexusExpr (AnnoS (Idx _ t) _ (RealS v)) = do
      s <- generalTypeToSerialAST t
      return $ case s of
        (SerialFloat32 _) -> LitX F32X (viaShow v <> "f")
        _ -> LitX F64X (viaShow v)
    toNexusExpr (AnnoS (Idx _ t) _ (IntS v)) = do
      s <- generalTypeToSerialAST t
      return $ case s of
          (SerialInt8 _)   -> LitX I8X  (pretty v)
          (SerialInt16 _)  -> LitX I16X (pretty v)
          (SerialInt _)    -> LitX I32X (pretty v)
          (SerialInt32 _)  -> LitX I32X (pretty v)
          (SerialInt64 _)  -> LitX I64X (pretty v <> "LL")
          (SerialUInt8 _)  -> LitX U8X  (pretty v)
          (SerialUInt16 _) -> LitX U16X (pretty v)
          (SerialUInt _)   -> LitX U32X (pretty v <> "U")
          (SerialUInt32 _) -> LitX U32X (pretty v <> "U")
          (SerialUInt64 _) -> LitX U64X (pretty v <> "ULL")
          _                -> LitX I64X (pretty v <> "LL") -- other int literals default to i64
    toNexusExpr (AnnoS _ _ (LogS True))  = return $ LitX BoolX "1"
    toNexusExpr (AnnoS _ _ (LogS False)) = return $ LitX BoolX "0"
    toNexusExpr (AnnoS _ _ UniS) = return $ LitX NullX "0"
    toNexusExpr _ = error $ "Unreachable value of type reached"

makePureExpression :: Int -> NexusExpr -> ([MDoc], MDoc)
makePureExpression index e0 = (code, varName) where
  finalExpr = makeExpr e0
  varName = "expr_" <> pretty index
  code = ["morloc_expression_t*" <+> varName <+> "=" <+> finalExpr <> ";"]

makeExpr :: NexusExpr -> MDoc
makeExpr (LstX s es) =
  let args = hsep ["," <+> (makeExpr e) | e <- es]
  in [idoc|make_morloc_container(#{s}, #{pretty (length es)}#{args})|]
makeExpr (TupX s es) =
  let args = hsep ["," <+> (makeExpr e) | e <- es]
  in [idoc|make_morloc_container(#{s}, #{pretty (length es)}#{args})|]
makeExpr (NamX s rs) =
  let args = hsep ["," <+> (makeExpr e) | (_, e) <- rs]
  in [idoc|make_morloc_container(#{s}, #{pretty (length rs)}#{args})|]
makeExpr (AppX s e es) =
  let args = punctuate ", " (map makeExpr es)
  in [idoc|make_morloc_app(#{s}, #{makeExpr e}, #{pretty (length es)}, #{hsep args})|]
makeExpr (LamX vs e) =
  let vars = punctuate ", " ["strdup" <> (parens . dquotes $ v) | v <- vs]
  in [idoc|make_morloc_lambda(#{makeExpr e}, #{pretty (length vs)}, #{hsep vars})|]
makeExpr (PatX s (PatternText p ps)) =
  let vars = punctuate ", " ["strdup" <> (parens . dquotes . pretty $ v) | v <- (p:ps)]
  in [idoc|make_morloc_interpolation(#{s}, #{pretty (1 + length ps)}, #{hsep vars})|]
makeExpr (PatX s (PatternStruct p)) = [idoc|make_morloc_pattern(#{s}, #{makePatternExpr p})|]  where
  makePatternExpr :: Selector -> MDoc
  makePatternExpr (SelectorIdx t ts) =
    let vars = punctuate ", " [ pretty i <> "," <+> makePatternExpr v  | (i, v) <- (t:ts)]
    in [idoc|make_morloc_pattern_idx(#{pretty (length (t:ts))}, #{hsep vars})|]
  makePatternExpr (SelectorKey t ts) =
    let vars = punctuate ", " [ "strdup" <> (parens . dquotes . pretty $ k) <> "," <+> makePatternExpr v  | (k, v) <- (t:ts)]
    in [idoc|make_morloc_pattern_key(#{pretty (length (t:ts))}, #{hsep vars})|]
  makePatternExpr SelectorEnd = [idoc|make_morloc_pattern_end()|]
makeExpr (BndX s v) = [idoc|make_morloc_bound_var(#{s}, strdup(#{dquotes v}))|]
makeExpr (StrX s x) = [idoc|make_morloc_literal(#{s}, (primitive_t){.s = strdup(#{x})})|]
makeExpr (LitX t x) = [idoc|make_morloc_literal(#{dquotes (litSchema t)}, (primitive_t){.#{litSchema t} = #{x}})|]

litSchema :: LitType -> MDoc
litSchema F32X = "f4"
litSchema F64X = "f8"
litSchema I8X = "i1"
litSchema I16X = "i2"
litSchema I32X = "i4"
litSchema I64X = "i8"
litSchema U8X = "u1"
litSchema U16X = "u2"
litSchema U32X = "u4"
litSchema U64X = "u8"
litSchema BoolX = "b"
litSchema NullX = "z"

getFData :: (Type, Int, Lang, CmdDocSet, [Socket]) -> MorlocMonad FData
getFData (t, i, lang, doc, sockets) = do

  mayName <- MM.metaName i
  (argSchemas, returnSchema) <- makeSchemas i lang t

  case mayName of
    (Just name') -> do
      config <- MM.ask
      let socket = MC.setupServerAndSocket config lang
      return $ FData
        { fdataSocket = setSocketPath socket
        , fdataSubcommand = maybe (pretty name') pretty (cmdDocName doc)
        , fdataSubcommandLength = MT.length (unEVar name')
        , fdataMid = i
        , fdataType = t
        , fdataSubSockets = map setSocketPath sockets
        , fdataArgSchemas = map dquotes argSchemas
        , fdataReturnSchema = dquotes returnSchema
        , fdataCmdDocSet = doc
        }
    Nothing -> MM.throwError . GeneratorError $ "No name in FData"


makeGastSchemas :: Type -> MorlocMonad (MDoc, [MDoc])
makeGastSchemas (FunT ts t) = do
  (s:ss) <- mapM generalTypeToSerialAST (t:ts) |>> map Serial.serialAstToMsgpackSchema
  return (s, ss)
makeGastSchemas t = do
  s <- Serial.serialAstToMsgpackSchema <$> generalTypeToSerialAST t
  return (s, [])

-- I leave the concrete types empty since these will be represented
-- automatically in the nexus language based on the schema
generalTypeToSerialAST :: Type -> MorlocMonad SerialAST
generalTypeToSerialAST (VarT v)
  | v == MBT.real = return $ SerialReal   (FV v (CV ""))
  | v == MBT.f32  = return $ SerialReal   (FV v (CV ""))
  | v == MBT.f64  = return $ SerialReal   (FV v (CV ""))
  | v == MBT.int  = return $ SerialInt    (FV v (CV ""))
  | v == MBT.i8   = return $ SerialInt8   (FV v (CV ""))
  | v == MBT.i16  = return $ SerialInt16  (FV v (CV ""))
  | v == MBT.i32  = return $ SerialInt32  (FV v (CV ""))
  | v == MBT.i64  = return $ SerialInt64  (FV v (CV ""))
  | v == MBT.u8   = return $ SerialUInt8  (FV v (CV ""))
  | v == MBT.u16  = return $ SerialUInt16 (FV v (CV ""))
  | v == MBT.u32  = return $ SerialUInt32 (FV v (CV ""))
  | v == MBT.u64  = return $ SerialUInt64 (FV v (CV ""))
  | v == MBT.bool = return $ SerialBool   (FV v (CV ""))
  | v == MBT.str  = return $ SerialString (FV v (CV ""))
  | v == MBT.unit = return $ SerialNull   (FV v (CV ""))
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
generalTypeToSerialAST (NamT o v [] rs)
  = SerialObject o (FV v (CV "")) []
  <$> mapM (secondM generalTypeToSerialAST) rs
generalTypeToSerialAST t = error $ "cannot serialize this type: " <> show t


-- place the socket files in the temporary directory for the given process
setSocketPath :: Socket -> Socket
setSocketPath s = s { socketPath = [idoc|os.path.join(tmpdir, #{dquotes (socketPath s)})|] }

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


main :: MC.Config -> [FData] -> Int -> [GastData] -> MDoc
main config fdata longestCommandLength cdata = format
  (DF.embededFileText DF.nexusTemplate)
  "// <<<BREAK>>>"
  [ usageCode fdata longestCommandLength cdata
  , vsep subcommandDispatchers
  , dispatch
  ]
  where
    (subcommandDispatchers, dispatch) = dispatchCode config fdata cdata

usageCode :: [FData] -> Int -> [GastData] -> MDoc
usageCode fdata longestCommandLength cdata =
  [idoc|
    fprintf(stderr, "%s", "Usage: ./nexus [OPTION]... COMMAND [ARG]...\n");
    fprintf(stderr, "%s", "\n");
    fprintf(stderr, "%s", "Nexus Options:\n");
    fprintf(stderr, "%s", " -h, --help            Print this help message\n");
    fprintf(stderr, "%s", " -o, --output-file     Print to this file instead of STDOUT\n");
    fprintf(stderr, "%s", " -f, --output-format   Output format [json|mpk|voidstar]\n");
    fprintf(stderr, "%s", "\n");
    fprintf(stderr, "%s", "Exported commands (call with -h/--help for more info):\n");
    #{align $ vsep (map (usageLineT longestCommandLength) fdata ++ map (usageLineConst longestCommandLength) cdata)}
|]

usageLineT :: Int -> FData -> MDoc
usageLineT longestCommandLength fdata =
  [idoc|fprintf(stderr, "%s", "  #{fdataSubcommand fdata}#{desc (cmdDocDesc doc)}\n");|]
  where
    doc = fdataCmdDocSet fdata
    padding = longestCommandLength - (fdataSubcommandLength fdata) + 5
    desc [] = ""
    desc (x:_) = pretty (replicate padding ' ') <> pretty x

usageLineConst :: Int -> GastData -> MDoc
usageLineConst longestCommandLength cmd =
  [idoc|fprintf(stderr, "%s", "  #{pretty (commandName cmd)}#{desc (cmdDocDesc doc)}\n");|]
  where
    doc = commandDocs cmd
    padding = longestCommandLength - (MT.length . unEVar . commandName $ cmd) + 2
    desc [] = ""
    desc (x:_) = pretty (replicate padding ' ') <> pretty x

writeTypes :: MDoc -> Type -> [MDoc]
writeTypes padding (FunT inputs output)
  = zipWith (writeType padding) (map Just [1..]) inputs
  ++ writeTypes padding output
writeTypes padding t = [writeType padding Nothing t]

writeType :: MDoc -> Maybe Int -> Type -> MDoc
writeType padding (Just i) t = [idoc|fprintf(stderr, "%s", "  #{padding}param #{pretty i}: #{pretty t}\n");|]
writeType padding Nothing  t = [idoc|fprintf(stderr, "%s", "  #{padding}return: #{fixLineWrapping $ pretty t}\n");|]

-- Long type names may be wrapped to multiple lines. This funtion adds new line
-- escapes at the end of each line (required in C strings)
fixLineWrapping :: MDoc -> MDoc
fixLineWrapping typestr = case lines (render' typestr) of
    [] -> pretty ("" :: String)
    [x] -> pretty x
    xs -> vsep $ [pretty (str <> "\\") | str <- init xs] <> [pretty (last xs)]

createStructLong [] = "{ NULL }"
createStructLong (x:xs) = align . vsep $ ["{" <+> x] <> ["," <+> x | x <- xs] <> ["}"]

dispatchCode :: Config -> [FData] -> [GastData] -> ([MDoc], MDoc)
dispatchCode _ [] [] = (["// no dispatchers registered"], "// nothing to dispatch")
dispatchCode config fdata cdata = (dispatchers, dispatchBlock)
    where
    dispatchBlock = indent 4 $ vsep
        [ [idoc|uint32_t mid = 0;|]
        , [idoc|int retcode = 0;|]
        , [idoc|char buffer[256];|]
        , [idoc|#{vsep socketDocs}|]
        , [idoc|if(config.packet_path != NULL){|]
        , [idoc|    morloc_socket_t* all_sockets[] = #{allSocketsList};|]
        , [idoc|    start_daemons(all_sockets);|]
        , [idoc|    run_call_packet(config);|]
        , [idoc|    clean_exit(0);|]
        , [idoc|}|]
        , [idoc|#{cIfElse (head cases) (tail cases) (Just elseClause)}|]
        ]

    makeSocketDoc socket = vsep
        [ [idoc|morloc_socket_t #{varName} = { 0 };|]
        , [idoc|#{varName}.lang = strdup("#{pretty $ socketLang socket}");|]
        , [idoc|#{varName}.syscmd = (char**)calloc(5, sizeof(char*));|]
        , [idoc|#{execArgsDoc}|]
        , [idoc|// Use a fixed buffer, then strdup to allocate the final string|]
        , [idoc|snprintf(buffer, 256, "%s/#{socketBasename}", tmpdir);|]
        , [idoc|#{varName}.syscmd[#{pretty $ length execArgs}] = strdup(buffer);|]
        , [idoc|#{varName}.syscmd[#{pretty $ length execArgs} + 1] = strdup(tmpdir);|]
        , [idoc|#{varName}.syscmd[#{pretty $ length execArgs} + 2] = strdup(shm_basename);|]
        , [idoc|#{varName}.syscmd[#{pretty $ length execArgs} + 3] = NULL;|]
        , [idoc|#{varName}.socket_filename = strdup(buffer);|]
        ]
        where

            varName = (pretty . ML.makeExtension $ socketLang socket) <> "_socket"

            makeExecutionArgs :: Lang -> [String]
            makeExecutionArgs CppLang = ["./" <> ML.makeExecutablePoolName CppLang]
            makeExecutionArgs CLang = ["./" <> ML.makeExecutablePoolName CLang]
            makeExecutionArgs Python3Lang = [configLangPython3 config, ML.makeExecutablePoolName Python3Lang]
            makeExecutionArgs RLang = [configLangR config, ML.makeExecutablePoolName RLang]

            execArgs = makeExecutionArgs (socketLang socket)
            execArgsDoc = vsep [ [idoc|#{varName}.syscmd[#{pretty i}] = strdup("#{pretty arg}");|]
                               | (i, arg) <- zip ([0..] :: [Int]) execArgs ]

            socketBasename = "pipe-" <> pretty (ML.showLangName (socketLang socket))

    uniqueFst :: Eq a => [(a, b)] -> [(a, b)]
    uniqueFst = f [] where
        f _ [] = []
        f seen (x@(a, _):xs)
            | a `elem` seen = f seen xs
            | otherwise = x : f (a:seen) xs

    allSockets = concat [ (fdataSocket x) : (fdataSubSockets x) | x <- fdata]

    daemonSets = uniqueFst [ (socketLang s, s) | s <- allSockets ]

    allSocketsList = encloseSep "{ " " }" ", " (allSocketDocs <> ["(morloc_socket_t*)NULL"])
        where
        allSocketDocs = [ "&" <> (pretty . ML.makeExtension $ lang) <> "_socket" | (lang, _) <- daemonSets]


    socketDocs = [makeSocketDoc s | (_, s) <- daemonSets]

    (dispatchers, cases) = unzip $ map makeCaseDoc fdata <> map makeGastCaseDoc cdata

    elseClause = [idoc|fprintf(stderr, "Unrecognized command '%s'\n", cmd);|]

cIfElse :: (MDoc, MDoc) -> [(MDoc, MDoc)] -> Maybe MDoc -> MDoc
cIfElse (cond1, block1) ifelses elseBlock = vsep $
  [block 4 ("if" <+> parens cond1) block1 ] <>
  [block 4 ("else if" <+> parens c) b | (c, b) <- ifelses] <>
  maybe [] (return . block 4 "else") elseBlock


makeCaseDoc :: FData -> (MDoc, (MDoc, MDoc))
makeCaseDoc fdata@(FData socket sub _ midx _ sockets schemas returnSchema _) =
    (dispatcher, (cond, body))
    where

    cond = [idoc|strcmp(cmd, "#{sub}") == 0|]
    body = vsep
      [ [idoc|morloc_socket_t* sockets[] = #{socketList};|]
      , [idoc|dispatch_#{sub}(argc, argv, shm_basename, config, #{lang}_socket, sockets);|]
      ]

    socketList = encloseSep "{ " " }" ", " $
        [ "&" <> (pretty . ML.makeExtension $ socketLang s) <> "_socket" | s <- sockets] <> ["(morloc_socket_t*)NULL"]

    lang = pretty . ML.makeExtension $ socketLang socket

    dispatcher = [idoc|
void dispatch_#{sub}(
    int argc,
    char* argv[],
    const char* shm_basename,
    config_t config,
    morloc_socket_t socket,
    morloc_socket_t** sockets
){
    int opt;
    const int OPT_HELP_VERBOSE = 256;

    static struct option long_options[] = {
        {"help", no_argument, 0, OPT_HELP_VERBOSE},
        {0, 0, 0, 0}
    };

    const char *short_options = "h";

    while ((opt = getopt_long(argc, argv, short_options, long_options, NULL)) != -1) {
        switch (opt) {
            case 'h': {
                #{align (subcmdHelpShortF fdata)}
                clean_exit(0);
            }; break;
            case OPT_HELP_VERBOSE: {
                #{align (subcmdHelpLongF fdata)}
                clean_exit(0);
            }; break;
        }
    }

    start_daemons(sockets);

    char* arg_schemas[] =
      #{argSchemasList};

    char return_schema[] = #{returnSchema};

    int nargs = 0;
    while(argv[optind + nargs] != NULL){
      nargs++;
    }

    argument_t** args = (argument_t**)calloc(nargs + 1, sizeof(argument_t*));
    for(int i = 0; i < nargs; i++){
      args[i] = initialize_positional(strdup(argv[optind + i]));
    }

    run_command(#{pretty midx}, args, arg_schemas, return_schema, socket, config);
}
|]

    argSchemasList = createStructLong $ schemas <> ["(char*)NULL"]


makeGastCaseDoc :: GastData -> (MDoc, (MDoc, MDoc))
makeGastCaseDoc gdata = (dispatcher, (cond, body))
    where
    func = pretty . unEVar . commandName $ gdata
    returnSchema = dquotes . fst . commandSchemas $ gdata
    (exprBody, exprVar) = commandExpr gdata
    argSchemasList = createStructLong $ ((map dquotes . snd . commandSchemas $ gdata) <> ["NULL"])

    cond = [idoc|strcmp(cmd, "#{func}") == 0|]
    body = [idoc|dispatch_#{func}(argc, argv, shm_basename, config);|]

    dispatcher = [idoc|
void dispatch_#{func}(
    int argc,
    char* argv[],
    const char* shm_basename,
    config_t config
){
    int opt;
    const int OPT_HELP_VERBOSE = 256;

    static struct option long_options[] = {
        {"help", no_argument, 0, OPT_HELP_VERBOSE},
        {0, 0, 0, 0}
    };

    const char *short_options = "h";

    while ((opt = getopt_long(argc, argv, short_options, long_options, NULL)) != -1) {
        switch (opt) {
            case 'h': {
                #{align (subcmdHelpShortG gdata)}
                clean_exit(0);
            }; break;
            case OPT_HELP_VERBOSE: {
                #{align (subcmdHelpLongG gdata)}
                clean_exit(0);
            }; break;
        }
    }

    char* arg_schemas[] = #{argSchemasList};
    char return_schema[] = #{returnSchema};
    #{vsep exprBody}

    int nargs = 0;
    while(argv[optind + nargs] != NULL){
      nargs++;
    }

    argument_t** args = (argument_t**)calloc(nargs + 1, sizeof(argument_t*));
    for(int i = 0; i < nargs; i++){
      args[i] = initialize_positional(strdup(argv[optind + i]));
    }

    run_pure_command(#{exprVar}, args, arg_schemas, return_schema, config);
}
|]

linePrinter :: [MDoc] -> MDoc
linePrinter xs = vsep [ [idoc|fprintf(stderr, "#{x}\n");|] | x <- xs]

subcmdHelpShortF :: FData -> MDoc
subcmdHelpShortF f = subcmdHelpShort (fdataSubcommand f) (fdataType f) (fdataCmdDocSet f)

subcmdHelpLongF :: FData -> MDoc
subcmdHelpLongF f = subcmdHelpLong (fdataSubcommand f) (fdataType f) (fdataCmdDocSet f)

subcmdHelpShortG :: GastData -> MDoc
subcmdHelpShortG g = subcmdHelpShort (pretty $ commandName g) (commandType g) (commandDocs g)

subcmdHelpLongG :: GastData -> MDoc
subcmdHelpLongG g = subcmdHelpLong (pretty $ commandName g) (commandType g) (commandDocs g)

subcmdHelpShort :: MDoc -> Type -> CmdDocSet -> MDoc
subcmdHelpShort cmd t0 docs = linePrinter
  $  [usage t0]
  <> desc docs
  <> args t0

  where
    usage (FunT (length -> n) _) =
      "Usage: ./nexus" <+> foldl (<+>) cmd ["ARG" <> pretty i | i <- take n ([1..] :: [Int])]
    usage _ = "Usage: ./nexus" <+> cmd

    desc (CmdDocSet [] _ _) = []
    desc (CmdDocSet (x:[]) _ _) = ["", pretty x]
    desc (CmdDocSet (x:details) _ _) = ["", pretty x, ""] <> (map pretty details)

    args (FunT ts _)
      =  ["", "Subcommand Arguments:"]
      <> concat (zipWith makeArg [1..] ts)
    args _ = []

    makeArg :: Int -> Type -> [MDoc]
    makeArg i t = [" ARG" <> pretty i <> ":  " <> pretty t]

subcmdHelpLong :: MDoc -> Type -> CmdDocSet -> MDoc
subcmdHelpLong = subcmdHelpShort
