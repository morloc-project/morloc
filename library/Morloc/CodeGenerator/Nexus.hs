{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

{-|
Module      : Morloc.CodeGenerator.Nexus
Description : Templates for generating a Perl nexus
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.CodeGenerator.Nexus
  ( generate
  ) where

import qualified Control.Monad.State as CMS
import Morloc.Data.Doc
import Morloc.CodeGenerator.Namespace
import Morloc.Quasi
import Morloc.Pretty ()
import qualified Morloc.Data.Text as MT
import qualified Control.Monad as CM
import qualified Morloc.Config as MC
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM

type FData =
  ( [MDoc] -- pool call command arguments, (e.g., ["RScript", "pool.R", "4", "--"])
  , MDoc -- subcommand name
  , Type -- argument type
  )

generate :: [NexusCommand] -> [(Type, Int)] -> MorlocMonad Script
generate cs xs = do
  config <- MM.ask

  callNames <- mapM (MM.metaName . snd) xs |>> catMaybes |>> map pretty
  let gastNames = map (pretty . commandName) cs
      names = callNames <> gastNames 
  fdata <- CM.mapM getFData xs -- [FData]
  outfile <- CMS.gets (fromMaybe "nexus.py" . stateOutfile)
  return $
    Script
      { scriptBase = outfile
      , scriptLang = ML.Python3Lang
      , scriptCode = "." :/ File outfile (Code . render $ main names fdata (pretty $ configLangPython3 config) cs)
      , scriptMake = [SysExe outfile]
      }

getFData :: (Type, Int) -> MorlocMonad FData
getFData (t, i) = do
  mayName <- MM.metaName i
  case mayName of
    (Just name') -> do
      config <- MM.ask
      let lang = langOf t
      case MC.buildPoolCallBase config lang i of
        (Just cmds) -> return (cmds, pretty name', t)
        Nothing ->
          MM.throwError . GeneratorError $
          "No execution method found for language: " <> ML.showLangName (fromJust lang)
    Nothing -> MM.throwError . GeneratorError $ "No name in FData"

main :: [MDoc] -> [FData] -> MDoc -> [NexusCommand] -> MDoc
main names fdata pythonExe cdata =
  [idoc|#!/usr/bin/env #{pythonExe}

import json
import subprocess
import sys

#{usageT fdata cdata}

#{vsep (map functionCT cdata ++ map functionT fdata)}

#{mapT names}

def dispatch(cmd, *args):
    if(cmd in ["-h", "--help", "-?", "?"]):
        usage()
    else:
        command_table[cmd](*args)

if __name__ == '__main__':
    if len(sys.argv) == 1:
        usage()
    else:
        cmd = sys.argv[1]
        args = sys.argv[2:]
        dispatch(cmd, *args)
|]

mapT names = [idoc|command_table = #{dict}|] where
    dict = encloseSep "{" "}" "," (map mapEntryT names)

mapEntryT n = [idoc|"#{n}" : call_#{n}|]

usageT :: [FData] -> [NexusCommand] -> MDoc
usageT fdata cdata =
  [idoc|
def usage():
    print("The following commands are exported:")
    #{align $ vsep (map usageLineT fdata ++ map usageLineConst cdata)}
|]

usageLineT :: FData -> MDoc
usageLineT (_, name', t) = vsep
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
functionT (cmd, subcommand, t) =
  [idoc|
def call_#{subcommand}(*args):
    if len([*args]) != #{pretty n}:
        sys.exit("Expected #{pretty n} arguments to '#{subcommand}', given " + str(len([*args])))
    else:
        subprocess.run(#{poolcallArgs})
|]
  where
    n = nargs t
    poolcallArgs = list $ map dquotes cmd <> ["*args"]

functionCT :: NexusCommand -> MDoc
functionCT (NexusCommand cmd _ json_str args subs) =
  [idoc|
def call_#{pretty cmd}(*args): 
    if len([*args]) != #{pretty $ length args}:
        sys.exit("Expected #{pretty $ length args} arguments to '#{pretty cmd}', given " + str(len([*args])))
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
