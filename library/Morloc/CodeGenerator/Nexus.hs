{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

{-|
Module      : Morloc.CodeGenerator.Nexus
Description : Templates for generating the nexus
Copyright   : (c) Zebulun Arendsee, 2016-2024
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
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Text as MT
import qualified Control.Monad as CM
import qualified Morloc.Config as MC
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.CodeGenerator.Infer as Infer
import qualified Morloc.CodeGenerator.Serial as Serial

data FData = FData
  { fdataSocket :: Socket
  , fdataSubcommand :: MDoc -- subcommand name
  , fdataSubcommandLength :: Int -- subcommand length
  , fdataMid :: Int -- manifold ID
  , fdataType :: Type -- argument type
  , fdataSubSockets :: [Socket] -- list of sockets needed for this command
  , fdataArgSchemas :: [MDoc] -- argument type schemas
  , fdataReturnSchema :: MDoc -- return type schema
  , fdataArgDocs :: Maybe [[MT.Text]]
  , fdataFunDocs :: [MT.Text]
  }

generate :: [NexusCommand] -> [(Type, Int, Lang, [Socket])] -> MorlocMonad Script
generate cs xs = do

  config <- MM.ask

  -- find the path for extensions
  -- this includes the mlcmpack module needed for MessagePack handling
  let home = MC.configHome config
      includeDir = home </> "include"

  fdata <- CM.mapM getFData xs -- [FData]

  -- get the length of the longest subcommand name (needed for alignment)
  let allSubcommandsLengths = map fdataSubcommandLength fdata <> map (MT.length . unEVar . commandName) cs
  let longestSubcommand = if length allSubcommandsLengths > 0
                          then maximum allSubcommandsLengths
                          else 0

  outfile <- CMS.gets (fromMaybe "nexus" . stateOutfile)
  let nexusfile = "nexus.c"
  return $
    Script
      { scriptBase = nexusfile
      , scriptLang = ML.CLang
      , scriptCode = "." :/ File nexusfile (Code . render $ main config fdata longestSubcommand cs)
      , scriptMake = [SysRun . Code $ "gcc -o " <> MT.pack outfile <> " -O -I" <> MT.pack includeDir <> " " <> MT.pack nexusfile]
      }

getFData :: (Type, Int, Lang, [Socket]) -> MorlocMonad FData
getFData (t, i, lang, sockets) = do

  (es, ss) <- MM.getDocStrings i

  mayName <- MM.metaName i
  (argSchemas, returnSchema) <- makeSchemas i lang t

  case mayName of
    (Just name') -> do
      config <- MM.ask
      let socket = MC.setupServerAndSocket config lang
      return $ FData
        { fdataSocket = setSocketPath socket
        , fdataSubcommand = pretty name'
        , fdataSubcommandLength = MT.length (unEVar name')
        , fdataMid = i
        , fdataType = t
        , fdataSubSockets = map setSocketPath sockets
        , fdataArgSchemas = map dquotes argSchemas
        , fdataReturnSchema = dquotes returnSchema
        , fdataArgDocs = es
        , fdataFunDocs = ss
        }
    Nothing -> MM.throwError . GeneratorError $ "No name in FData"


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


main :: MC.Config -> [FData] -> Int -> [NexusCommand] -> MDoc
main config fdata longestCommandLength cdata
    = format (DF.embededFileText DF.nexusTemplate) "// <<<BREAK>>>" [ usageCode fdata longestCommandLength cdata, dispatchCode config fdata cdata ]

usageCode :: [FData] -> Int -> [NexusCommand] -> MDoc
usageCode fdata longestCommandLength cdata =
  [idoc|
    fprintf(stderr, "%s", "Usage: ./nexus [OPTION]... COMMAND [ARG]...\n");
    fprintf(stderr, "%s", "\n");
    fprintf(stderr, "%s", "Nexus Options:\n");
    fprintf(stderr, "%s", " -h, --help            Print this help message\n");
    fprintf(stderr, "%s", " -o, --output-file     Print to this file instead of STDOUT\n");
    fprintf(stderr, "%s", " -f, --output-format   Output format [json|mpk|voidstar]\n");
    fprintf(stderr, "%s", "\n");
    fprintf(stderr, "%s", "Exported Commands:\n");
    #{align $ vsep (map (usageLineT longestCommandLength) fdata ++ map (usageLineConst longestCommandLength) cdata)}
|]

usageLineT :: Int -> FData -> MDoc
usageLineT longestCommandLength fdata = vsep
  ( [idoc|fprintf(stderr, "%s", "  #{fdataSubcommand fdata}#{desc (fdataFunDocs fdata)}\n");|]
  : typeStrs
  )
  where
    padding = longestCommandLength - (fdataSubcommandLength fdata) + 2

    desc [] = ""
    desc (x:_) = pretty (replicate padding ' ') <> pretty x

    typePadding = pretty $ replicate (longestCommandLength + 5) ' '

    typeStrs = writeTypes typePadding (fdataType fdata)

usageLineConst :: Int -> NexusCommand -> MDoc
usageLineConst longestCommandLength cmd = vsep
  ( [idoc|fprintf(stderr, "%s", "  #{pretty (commandName cmd)}#{desc (commandDocs cmd)}\n");|]
  : writeTypes typePadding (commandType cmd)
  )
  where
    padding = longestCommandLength - (MT.length . unEVar . commandName $ cmd) + 2

    typePadding = pretty $ replicate (longestCommandLength + 5) ' '

    desc [] = ""
    desc (x:_) = pretty (replicate padding ' ') <> pretty x

writeTypes :: MDoc -> Type -> [MDoc]
writeTypes padding (FunT inputs output)
  = zipWith (writeType padding) (map Just [1..]) inputs
  ++ writeTypes padding output
writeTypes padding t = [writeType padding Nothing t]

writeType :: MDoc -> Maybe Int -> Type -> MDoc
writeType padding (Just i) t = [idoc|fprintf(stderr, "%s", "  #{padding}param #{pretty i}: #{pretty t}\n");|]
writeType padding Nothing  t = [idoc|fprintf(stderr, "%s", "  #{padding}return: #{pretty t}\n");|]

dispatchCode :: Config -> [FData] -> [NexusCommand] -> MDoc
dispatchCode _ [] [] = "// nothing to dispatch"
dispatchCode config fdata cdata = [idoc|
    uint32_t mid = 0;
    int retcode = 0;
    char buffer[256];
    #{vsep socketDocs}
    if(config.packet_path != NULL){
        morloc_socket_t* all_sockets[] = #{allSocketsList};
        start_daemons(all_sockets);
        run_call_packet(config);
        clean_exit(0);
    }

    #{cIfElse (head cases) (tail cases) (Just elseClause)}
    |]
    where
    makeSocketDoc socket =
        [idoc|
    morloc_socket_t #{varName} = { 0 };
    #{varName}.lang = strdup("#{pretty $ socketLang socket}");
    #{varName}.syscmd = (char**)calloc(5, sizeof(char*));
    #{execArgsDoc}
    // Use a fixed buffer, then strdup to allocate the final string
    snprintf(buffer, 256, "%s/#{socketBasename}", tmpdir);
    #{varName}.syscmd[#{pretty $ length execArgs}] = strdup(buffer);
    #{varName}.syscmd[#{pretty $ length execArgs} + 1] = strdup(tmpdir);
    #{varName}.syscmd[#{pretty $ length execArgs} + 2] = strdup(shm_basename);
    #{varName}.syscmd[#{pretty $ length execArgs} + 3] = NULL;
    #{varName}.socket_filename = strdup(buffer);
        |]
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

    makeCaseDoc (FData socket sub _ midx _ sockets schemas returnSchema _ _) =
        ( [idoc|strcmp(cmd, "#{sub}") == 0|]
        , [idoc|    mid = #{pretty midx};
    morloc_socket_t* sockets[] = #{socketList};
    const char* arg_schemas[] = #{argSchemasList};
    char return_schema[] = #{returnSchema};
    start_daemons(sockets);
    run_command(mid, args, arg_schemas, return_schema, #{lang}_socket, config);
          |]
        )
        where
        socketList = encloseSep "{ " " }" ", " $
            [ "&" <> (pretty . ML.makeExtension $ socketLang s) <> "_socket" | s <- sockets] <> ["(morloc_socket_t*)NULL"]

        argSchemasList = encloseSep "{ " " }" ", " $ schemas <> ["(char*)NULL"]
        lang = pretty . ML.makeExtension $ socketLang socket

    cases = map makeCaseDoc fdata <> map makeGastCaseDoc cdata

    elseClause = [idoc|fprintf(stderr, "Unrecognized command '%s'\n", cmd);|]

cIfElse :: (MDoc, MDoc) -> [(MDoc, MDoc)] -> Maybe MDoc -> MDoc
cIfElse (cond1, block1) ifelses elseBlock = hsep $
    [ "if" <> block 4 (parens cond1) block1 ] <>
    [ block 4 ("else if" <+> parens condX) blockX  | (condX, blockX) <- ifelses] <>
    [ maybe "" (block 4 "else") elseBlock ]

makeGastCaseDoc :: NexusCommand -> (MDoc, MDoc)
makeGastCaseDoc nc = (cond, body)
    where
    cond = [idoc|strcmp(cmd, "#{func}") == 0|]
    func = pretty . unEVar . commandName $ nc
    (argDefs, argStr) = case commandSubs nc of
        [] -> ("", "")
        xs -> ( vsep (map makeArgDef $ zip ([0..] :: [Int]) xs)
              , hsep ["," <+> "arg_str_" <> pretty i | (i, _) <- zip ([0..] :: [Int]) xs]
              )
    body = vsep
        [ argDefs
        , [idoc|printf("#{commandForm nc}\n"#{argStr});|]
        ]

    makeArgDef :: (Int, (JsonPath, MT.Text, JsonPath)) -> MDoc
    makeArgDef (i, (_, key, [])) = [idoc|char* arg_str_#{pretty i} = args[#{pretty (lookupKey key (commandArgs nc))}];|]
    makeArgDef (i, (_, key, path)) = vsep
        [ [idoc|char* errmsg_#{pretty i} = NULL;|]
        , [idoc|path_t path_#{pretty i}[] = #{pathStr}; |]
        , [idoc|size_t path_length_#{pretty i} = #{pretty $ length path}; |]
        , [idoc|char* arg_str_#{pretty i} = access_json_by_path(args[#{pretty (lookupKey key (commandArgs nc))}], path_#{pretty i}, path_length_#{pretty i}, &errmsg_#{pretty i});|]
        , [idoc|if(errmsg_#{pretty i} != NULL) { fprintf(stderr, "%s", "failed to parse json argument\n"); exit(1); } |]
        ]
        where
            pathStr = encloseSep "{" "}" "," $ map makeElementStr path

            makeElementStr :: JsonAccessor -> MDoc
            makeElementStr (JsonIndex ji) = [idoc|{JSON_PATH_TYPE_IDX, {.index = #{pretty ji}}}|]
            makeElementStr (JsonKey k) = [idoc|{JSON_PATH_TYPE_KEY, {.key = "#{pretty k}"}}|]


    lookupKey :: MT.Text -> [EVar] -> Int
    lookupKey key vs = f 0 vs where
        f _ [] = error "Invalid key" -- this should not be reachable
        f i ((EV v):rs)
            | key == v = i
            | otherwise = f (i+1) rs
