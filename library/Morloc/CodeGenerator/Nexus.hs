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

  callNames <- mapM (MM.metaName . (\(_, i, _, _) -> i)) xs |>> catMaybes |>> map pretty
  let gastNames = map (pretty . commandName) cs
      names = callNames <> gastNames
  fdata <- CM.mapM getFData xs -- [FData]
  outfile <- CMS.gets (fromMaybe "nexus.c" . stateOutfile)
  return $
    Script
      { scriptBase = outfile
      , scriptLang = ML.CLang
      , scriptCode = "." :/ File outfile (Code . render $ main config names fdata cs)
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


main :: MC.Config -> [MDoc] -> [FData] -> [NexusCommand] -> MDoc
main config names fdata cdata
    = format (DF.embededFileText DF.nexusTemplate) "// <<<BREAK>>>" [ usageCode fdata cdata, dispatchCode config fdata ]

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

dispatchCode config fdata = [idoc|
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
            cmd = dquotes . hsep $ socketServerInit socket

            makeExecutionArgs :: Lang -> [String]
            makeExecutionArgs CppLang = ["./" <> ML.makeExecutablePoolName CppLang]
            makeExecutionArgs CLang = ["./" <> ML.makeExecutablePoolName CLang]
            makeExecutionArgs Python3Lang = [configLangPython3 config, ML.makeExecutablePoolName Python3Lang]
            makeExecutionArgs RLang = [configLangR config, ML.makeExecutablePoolName RLang]

            execArgs = makeExecutionArgs (socketLang socket)
            execArgsDoc = vsep [ [idoc|#{varName}.syscmd[#{pretty i}] = strdup("#{pretty arg}");|]
                               | (i, arg) <- zip ([0..] :: [Int]) execArgs ]

            executableName = pretty $ ML.makeExecutablePoolName (socketLang socket)

            socketBasename = "pipe-" <> pretty (ML.showLangName (socketLang socket))

    uniqueFst :: Eq a => [(a, b)] -> [(a, b)]
    uniqueFst = f [] where
        f _ [] = []
        f seen (x@(a, _):xs)
            | a `elem` seen = f seen xs 
            | otherwise = x : f (a:seen) xs

    allSockets = concat [ s:ss | (s, _, _, _, ss, _, _) <- fdata]

    daemonSets = uniqueFst [ (socketLang s, s) | s <- allSockets ]

    socketDocs = [makeSocketDoc s | (a, s) <- daemonSets]

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

    cases = map makeCaseDoc fdata

    elseClause = [idoc|fprintf(stderr, "Unrecognized command '%s'\n", cmd);|]

cIfElse :: (MDoc, MDoc) -> [(MDoc, MDoc)] -> Maybe MDoc -> MDoc
cIfElse (cond1, block1) ifelses elseBlock = hsep $
    [ "if" <> block 4 (parens cond1) block1 ] <>
    [ block 4 ("else if" <+> parens condX) blockX  | (condX, blockX) <- ifelses] <>
    [ maybe "" (block 4 "else") elseBlock ]




--  [ usageT fdata cdata <> "\n" <>
--    vsep (map functionCT cdata ++ map functionT fdata) <> "\n" <>
--    mapT names
--  ]
--
-- mapT :: [Doc ann] -> Doc ann
-- mapT names = [idoc|command_table = #{dict}|] where
--     dict = encloseSep "{" "}" "," (map mapEntryT names)
--
-- mapEntryT :: Doc ann -> Doc ann
-- mapEntryT n = [idoc|"#{n}" : call_#{n}|]
--
-- functionT :: FData -> MDoc
-- functionT (Socket lang _ _, subcommand, mid, t, sockets, schemas, return_schema) =
--   [idoc|
-- def call_#{subcommand}(args, tmpdir, shm_basename):
--     if len(args) != #{pretty (nargs t)}:
--         clean_exit("Expected #{pretty (nargs t)} arguments to '#{subcommand}', given " + str(len(args)))
--     else:
--         run_command(
--             mid = #{pretty mid},
--             args = args,
--             pool_lang = #{poolLangDoc},
--             sockets = #{socketsDoc},
--             arg_schema = #{list(schemas)},
--             return_schema = #{return_schema}
--         )
-- |]
--   where
--     poolLangDoc = dquotes . pretty $ ML.showLangName lang
--     socketsDoc = list [align . vsep $ map (\x -> makeSocketDoc x <> ",") sockets]
--
--     makeSocketDoc :: Socket -> MDoc
--     makeSocketDoc (Socket lang' cmdDocs pipeDoc) =
--       tupled [ dquotes . pretty $ ML.showLangName lang'
--              , list (map dquotes cmdDocs <> [pipeDoc, "tmpdir", "shm_basename"])
--              , pipeDoc
--              ]
--
-- functionCT :: NexusCommand -> MDoc
-- functionCT (NexusCommand cmd _ json_str args subs) =
--   [idoc|
-- def call_#{pretty cmd}(args, tmpdir, shm_basename):
--     if len(args) != #{pretty $ length args}:
--         errmsg = "Expected #{pretty $ length args} arguments to '#{pretty cmd}', given " + str(len(args))
--         clean_exit(1, errmsg)
--     else:
--         json_obj = json.loads('''#{json_str}''')
--         #{align . vsep $ readArguments ++ replacements}
--         print(json.dumps(json_obj, separators=(",", ":")))
--         clean_exit(0)
-- |]
--   where
--     readArguments = zipWith readJsonArg args [0..]
--     replacements = map (uncurry3 replaceJson) subs
--
-- replaceJson :: JsonPath -> MT.Text -> JsonPath -> MDoc
-- replaceJson pathTo v pathFrom
--   = access "json_obj" pathTo
--   <+> "="
--   <+> access [idoc|json_#{pretty v}|] pathFrom
--
-- access :: MDoc -> JsonPath -> MDoc
-- access = foldl pathElement
--
-- pathElement :: MDoc -> JsonAccessor -> MDoc
-- pathElement jsonObj (JsonIndex i) = jsonObj <> brackets (pretty i)
-- pathElement jsonObj (JsonKey key) = jsonObj <> brackets (dquotes (pretty key))
--
-- readJsonArg :: EVar -> Int -> MDoc
-- readJsonArg (EV v) i = [idoc|json_#{pretty v} = json.loads([*args][#{pretty i}])|]
