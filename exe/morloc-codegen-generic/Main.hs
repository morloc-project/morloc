{- |
Module      : Main
Description : Generic pool code generator for morloc
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0

Standalone executable that assembles pool files for dynamically-typed
interpreted languages. Receives a serialized IProgram via a binary file
and a language descriptor via lang.yaml.

Usage: morloc-codegen-generic <lang.yaml> <iprogram.bin>

Reads:
  - argv[1]: path to lang.yaml (language descriptor)
  - argv[2]: path to binary-encoded IProgram

Writes to stdout:
  - JSON CodegenManifest with pool_code and build_commands
-}
module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>), takeExtension)
import System.IO (hPutStrLn, stderr)

import Morloc.CodeGenerator.Grammars.Translator.Generic (CodegenManifest(..), printProgram)
import Morloc.CodeGenerator.Grammars.Translator.Imperative (IProgram)
import Morloc.CodeGenerator.LanguageDescriptor (LangDescriptor(..), loadLangDescriptor)
import Morloc.Data.Doc (render)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [langYamlPath, iprogramPath] -> run langYamlPath iprogramPath
    _ -> do
      hPutStrLn stderr "Usage: morloc-codegen-generic <lang.yaml> <iprogram.bin>"
      exitFailure

run :: FilePath -> FilePath -> IO ()
run langYamlPath iprogramPath = do
  -- load language descriptor
  descResult <- loadLangDescriptor langYamlPath
  desc <- case descResult of
    Left err -> do
      hPutStrLn stderr $ "Failed to load " ++ langYamlPath ++ ": " ++ err
      exitFailure
    Right d -> return d

  -- load pool template from disk if not inline
  desc' <- if T.null (ldPoolTemplate desc)
    then do
      let langDir = takeDirectory langYamlPath
          ext = ldExtension desc
          poolPath = langDir </> "pool." ++ ext
      poolText <- TIO.readFile poolPath
      return desc { ldPoolTemplate = poolText }
    else return desc

  -- deserialize IProgram
  binaryData <- BL.readFile iprogramPath
  let program = Binary.decode binaryData :: IProgram

  -- assemble pool file
  let poolCode = render (printProgram desc' program)

  -- output manifest as JSON
  let manifest = CodegenManifest
        { cgmPoolCode = poolCode
        , cgmBuildCommands = []
        }
  BL.putStr (Aeson.encode manifest)
