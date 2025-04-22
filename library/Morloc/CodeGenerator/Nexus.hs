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
import qualified Morloc.Data.Text as MT
import qualified Control.Monad as CM
import qualified Morloc.Config as MC
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.CodeGenerator.Infer as Infer
import qualified Morloc.CodeGenerator.Serial as Serial

type FData =
  ( Socket
  , MDoc -- subcommand name
  , Int -- manifold ID
  , Type -- argument type
  , [Socket] -- list of sockets needed for this command
  , [MDoc] -- argument type schemas
  , MDoc -- return type schema
  )

generate :: [NexusCommand] -> [(Type, Int, Lang, [Socket])] -> MorlocMonad Script
generate cs xs = do

  config <- MM.ask

  -- find the path for extensions
  -- this includes the mlcmpack module needed for MessagePack handling
  let home = MC.configHome config
      includeDir = home </> "include"

  fdata <- CM.mapM getFData xs -- [FData]
  outfile <- CMS.gets (fromMaybe "nexus.c" . stateOutfile)
  return $
    Script
      { scriptBase = outfile
      , scriptLang = ML.CLang
      , scriptCode = "." :/ File outfile (Code . render $ main config fdata cs)
      , scriptMake = [SysRun . Code $ "gcc -o nexus -O -I" <> MT.pack includeDir <> " " <> MT.pack outfile]
      }

getFData :: (Type, Int, Lang, [Socket]) -> MorlocMonad FData
getFData (t, i, lang, sockets) = do
  mayName <- MM.metaName i
  (arg_schemas, return_schema) <- makeSchemas i lang t
  case mayName of
    (Just name') -> do
      config <- MM.ask
      let socket = MC.setupServerAndSocket config lang 
      return (setSocketPath socket, pretty name', i, t, map setSocketPath sockets, map dquotes arg_schemas, dquotes return_schema)
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


main :: MC.Config -> [FData] -> [NexusCommand] -> MDoc
main config fdata cdata
    = format (DF.embededFileText DF.nexusTemplate) "// <<<BREAK>>>" [ usageCode fdata cdata, dispatchCode config fdata cdata ]

usageCode :: [FData] -> [NexusCommand] -> MDoc
usageCode fdata cdata =
  [idoc|
    fprintf(stderr, "The following commands are exported:\n");
    #{align $ vsep (map usageLineT fdata ++ map usageLineConst cdata)}
|]

usageLineT :: FData -> MDoc
usageLineT (_, name', _, t, _, _, _) = vsep
  ( [idoc|fprintf(stderr, "  #{name'}\n");|]
  : writeTypes t
  )

usageLineConst :: NexusCommand -> MDoc
usageLineConst cmd = vsep
  ( [idoc|fprintf(stderr, "  #{pretty (commandName cmd)}\n");|]
  : writeTypes (commandType cmd)
  )

writeTypes :: Type -> [MDoc]
writeTypes (FunT inputs output)
  = zipWith writeType (map Just [1..]) inputs
  ++ writeTypes output
writeTypes t = [writeType Nothing t]

writeType :: Maybe Int -> Type -> MDoc
writeType (Just i) t = [idoc|fprintf(stderr, "    param #{pretty i}: #{pretty t}\n");|]
writeType Nothing  t = [idoc|fprintf(stderr, "    return: #{pretty t}\n");|]

dispatchCode _ [] [] = "// nothing to dispatch"
dispatchCode config fdata cdata = [idoc|
    uint32_t mid = 0;

    #{vsep socketDocs}

    #{cIfElse (head cases) (tail cases) (Just elseClause)}
    |]
    where
    makeSocketDoc socket = 
        [idoc|
    morloc_socket_t #{varName} = { 0 };
    #{varName}.lang = strdup("#{pretty $ socketLang socket}");
    #{varName}.syscmd = (char**)calloc(#{pretty $ length execArgs + 4}, sizeof(morloc_socket_t*));
    #{execArgsDoc} 
    asprintf(&#{varName}.syscmd[#{pretty $ length execArgs}], "%s/#{socketBasename}", tmpdir);
    #{varName}.syscmd[#{pretty $ length execArgs + 1}] = strdup(tmpdir);
    #{varName}.syscmd[#{pretty $ length execArgs + 2}] = strdup(shm_basename);
    #{varName}.syscmd[#{pretty $ length execArgs + 3}] = NULL;
    asprintf(&#{varName}.socket_filename, "%s/#{socketBasename}", tmpdir);

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

    allSockets = concat [ s:ss | (s, _, _, _, ss, _, _) <- fdata]

    daemonSets = uniqueFst [ (socketLang s, s) | s <- allSockets ]

    socketDocs = [makeSocketDoc s | (_, s) <- daemonSets]

    makeCaseDoc (socket, sub, midx, _, sockets, schemas, returnSchema) = 
        ( [idoc|strcmp(cmd, "#{sub}") == 0|]
        ,[idoc|    uint32_t mid = #{pretty midx};
    morloc_socket_t* sockets[] = #{socketList};
    const char* arg_schemas[] = #{argSchemasList};
    char return_schema[] = #{returnSchema};
    run_command(mid, args, arg_schemas, return_schema, #{lang}_socket, sockets);
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
        , [idoc|if(errmsg_#{pretty i} != NULL) { fprintf(stderr, "failed to parse json argument\n"); exit(1); } |]
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
