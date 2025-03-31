{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

{-|
Module      : Morloc.CodeGenerator.Nexus
Description : Templates for generating a Perl nexus
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

  -- find the path for extensions
  -- this includes the mlcmpack module needed for MessagePack handling
  home <- MM.asks MC.configHome
  let includeDir = home </> "include"
      

  callNames <- mapM (MM.metaName . (\(_, i, _, _) -> i)) xs |>> catMaybes |>> map pretty
  let gastNames = map (pretty . commandName) cs
      names = callNames <> gastNames
  fdata <- CM.mapM getFData xs -- [FData]
  outfile <- CMS.gets (fromMaybe "nexus.c" . stateOutfile)
  return $
    Script
      { scriptBase = outfile
      , scriptLang = ML.CLang
      , scriptCode = "." :/ File outfile (Code . render $ main (pretty home) names fdata cs)
      , scriptMake = [SysRun . Code $ "gcc -O -I" <> MT.pack includeDir <> " " <> MT.pack outfile]
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


main :: MDoc -> [MDoc] -> [FData] -> [NexusCommand] -> MDoc
main homeDir names fdata cdata
    = format (DF.embededFileText DF.nexusTemplate) "// <<<BREAK>>>" [ usageCode fdata, dispatchCode fdata ]

usageCode _ = [idoc|fprintf(stderr, "Usage STUB");|]

dispatchCode fdata = [idoc|
    uint32_t mid = 0;

    #{vsep socketDocs}

    #{cIfElse (head cases) (tail cases) (Just elseClause)}
    |]
    where
    makeSocketDoc fdata@(socket, sub, midx, argType, sockets, schemas, returnSchema) = 
        [idoc|
    morloc_socket_t #{varName} = { 0 };
    #{varName}.lang = strdup("#{lang}");
    #{varName}.syscmd[0] = strdup("./pool-#{lang}.out");
    asprintf(&#{varName}.syscmd[1], "%s/pipe-#{lang}", tmpdir);
    #{varName}.syscmd[2] = strdup(tmpdir);
    #{varName}.syscmd[3] = strdup(shm_basename);
    asprintf(&#{varName}.socket_filename, "%s/pipe-#{lang}", tmpdir);

        |]
        where
            lang = pretty . ML.makeExtension $ socketLang socket
            varName = lang <> "_socket"
            cmd = dquotes . hsep $ socketServerInit socket

    socketDocs = map makeSocketDoc fdata

    makeCaseDoc fdata@(socket, sub, midx, argType, sockets, schemas, returnSchema) = 
        ( [idoc|strcmp(cmd, "#{sub}") == 0|]
        ,[idoc|    uint32_t mid = #{pretty midx};
    morloc_socket_t* sockets[] = #{socketList};
    const char* arg_schemas[] = #{argSchemasList};
    char return_schema[] = #{returnSchema};
    run_command(mid, args, arg_schemas, return_schema, #{lang}_socket, sockets, tmpdir);
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
    [ block 4 ("if else" <+> parens condX) blockX  | (condX, blockX) <- ifelses] <>
    [ maybe "" (\elseBlock -> block 4 "else" elseBlock) elseBlock ]
