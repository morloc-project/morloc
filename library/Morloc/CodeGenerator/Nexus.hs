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
  )

generate :: [NexusCommand] -> [(Type, Int, Lang, [Socket])] -> MorlocMonad Script
generate cs xs = do
  callNames <- mapM (MM.metaName . (\(_, i, _, _) -> i)) xs |>> catMaybes |>> map pretty
  let gastNames = map (pretty . commandName) cs
      names = callNames <> gastNames
  fdata <- CM.mapM getFData xs -- [FData]
  outfile <- CMS.gets (fromMaybe "nexus.py" . stateOutfile)
  return $
    Script
      { scriptBase = outfile
      , scriptLang = ML.Python3Lang
      , scriptCode = "." :/ File outfile (Code . render $ main names fdata cs)
      , scriptMake = [SysExe outfile]
      }

getFData :: (Type, Int, Lang, [Socket]) -> MorlocMonad FData
getFData (t, i, lang, sockets) = do
  mayName <- MM.metaName i
  schemas <- makeSchemas i lang t
  case mayName of
    (Just name') -> do
      config <- MM.ask
      let socket = MC.setupServerAndSocket config lang 
      return (socket, pretty name', i, t, sockets, schemas)
    Nothing -> MM.throwError . GeneratorError $ "No name in FData"

makeSchemas :: Int -> Lang -> Type -> MorlocMonad [MDoc]
makeSchemas mid lang (FunT ts _) = mapM (makeSchema mid lang) ts
makeSchemas _ _ _ = return [] 

makeSchema :: Int -> Lang -> Type -> MorlocMonad MDoc
makeSchema mid lang t = do
  ft <- Infer.inferConcreteTypeUniversal lang t
  ast <- Serial.makeSerialAST mid lang ft
  return $ typeSchema ast

typeSchema :: SerialAST -> MDoc
typeSchema = f . Serial.serialAstToJsonType
  where
    f :: JsonType -> MDoc
    f (VarJ v) = lst [var v, "None"]
    f (ArrJ v ts) = lst [var v, lst (map f ts)]
    f (NamJ (CV "dict") es) = lst [dquotes "dict", dict (map entry es)]
    f (NamJ (CV "record") es) = lst [dquotes "record", dict (map entry es)]
    f (NamJ v es) = lst [pretty v, dict (map entry es)]

    entry :: (Key, JsonType) -> MDoc
    entry (v, t) = pretty v <> "=" <> f t

    dict :: [MDoc] -> MDoc
    dict xs = "OrderedDict" <> lst xs

    lst :: [MDoc] -> MDoc
    lst xs = encloseSep "(" ")" "," xs

    var :: CVar -> MDoc
    var v = dquotes (pretty v)


main :: [MDoc] -> [FData] -> [NexusCommand] -> MDoc
main names fdata cdata =
  [idoc|#{nexusSourceUtility langSrc}

#{usageT fdata cdata}

#{vsep (map functionCT cdata ++ map functionT fdata)}

#{mapT names}

#{nexusSourceMain langSrc}
|]
  where
    langSrc = DF.nexusFiles

mapT :: [Doc ann] -> Doc ann
mapT names = [idoc|command_table = #{dict}|] where
    dict = encloseSep "{" "}" "," (map mapEntryT names)

mapEntryT :: Doc ann -> Doc ann
mapEntryT n = [idoc|"#{n}" : call_#{n}|]

usageT :: [FData] -> [NexusCommand] -> MDoc
usageT fdata cdata =
  [idoc|
def usage():
    print("The following commands are exported:")
    #{align $ vsep (map usageLineT fdata ++ map usageLineConst cdata)}
|]

usageLineT :: FData -> MDoc
usageLineT (_, name', _, t, _, _) = vsep
  ( [idoc|print("  #{name'}")|]
  : writeTypes t
  )

usageLineConst :: NexusCommand -> MDoc
usageLineConst cmd = vsep
  ( [idoc|print("  #{pretty (commandName cmd)}")|]
  : writeTypes (commandType cmd)
  )

writeTypes :: Type -> [MDoc]
writeTypes (FunT inputs output)
  = zipWith writeType (map Just [1..]) inputs
  ++ writeTypes output
writeTypes t = [writeType Nothing t]

writeType :: Maybe Int -> Type -> MDoc
writeType (Just i) t = [idoc|print('''    param #{pretty i}: #{pretty t}''')|]
writeType Nothing  t = [idoc|print('''    return: #{pretty t}''')|]


functionT :: FData -> MDoc
functionT (Socket lang _ _, subcommand, mid, t, sockets, schemas) =
  [idoc|
def call_#{subcommand}(args):
    if len(args) != #{pretty (nargs t)}:
        clean_exit("Expected #{pretty (nargs t)} arguments to '#{subcommand}', given " + str(len(args)))
    else:
        run_command(
            mid = #{pretty mid},
            args = args,
            pool_lang = #{poolLangDoc},
            sockets = #{socketsDoc},
            arg_schema = #{list(schemas)},
        )
|]
  where
    poolLangDoc = dquotes . pretty $ ML.showLangName lang
    socketsDoc = list [align . vsep $ map (\x -> makeSocketDoc x <> ",") sockets]

    makeSocketDoc :: Socket -> MDoc
    makeSocketDoc (Socket lang' cmdDocs pipeDoc) = tupled [dquotes . pretty $ ML.showLangName lang', list (map dquotes cmdDocs), dquotes pipeDoc]

functionCT :: NexusCommand -> MDoc
functionCT (NexusCommand cmd _ json_str args subs) =
  [idoc|
def call_#{pretty cmd}(args):
    if len(args) != #{pretty $ length args}:
        sys.exit("Expected #{pretty $ length args} arguments to '#{pretty cmd}', given " + str(len(args)))
    else:
        json_obj = json.loads('''#{json_str}''')
        #{align . vsep $ readArguments ++ replacements}
        print(json.dumps(json_obj, separators=(",", ":")))
|]
  where
    readArguments = zipWith readJsonArg args [0..]
    replacements = map (uncurry3 replaceJson) subs

replaceJson :: JsonPath -> MT.Text -> JsonPath -> MDoc
replaceJson pathTo v pathFrom
  = access "json_obj" pathTo
  <+> "="
  <+> access [idoc|json_#{pretty v}|] pathFrom

access :: MDoc -> JsonPath -> MDoc
access = foldl pathElement

pathElement :: MDoc -> JsonAccessor -> MDoc
pathElement jsonObj (JsonIndex i) = jsonObj <> brackets (pretty i)
pathElement jsonObj (JsonKey key) = jsonObj <> brackets (dquotes (pretty key))

readJsonArg :: EVar -> Int -> MDoc
readJsonArg (EV v) i = [idoc|json_#{pretty v} = json.loads([*args][#{pretty i}])|]
