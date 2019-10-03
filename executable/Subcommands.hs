{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Subcommands
Description : Morloc executable subcommands
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Subcommands
  ( getConfig
  , cmdInstall
  , cmdMake
  , cmdRemove
  , cmdTypecheck
  ) where

import Morloc.Namespace
import System.Console.Docopt
import qualified Morloc as M
import qualified Morloc.Config as Config
import qualified Morloc.Data.Text as MT
import qualified Morloc.Module as Mod
import qualified Morloc.Monad as MM
import qualified Morloc.Parser.API as Papi
import qualified Morloc.Parser.Parser as P
import qualified Morloc.TypeChecker.API as T

type Subcommand = Arguments -> Config.Config -> IO ()

getArgOrDie :: Arguments -> Option -> MT.Text
getArgOrDie args opt =
  case getArg args opt of
    Nothing -> error ("Invalid command: Expected option '" <> show opt)
    (Just x) -> MT.pack x

-- | read the global morloc config file or return a default one
getConfig :: Arguments -> IO Config.Config
getConfig args = do
  let givenPath = getArg args (longOption "config")
  let isVanilla = isPresent args (longOption "vanilla")
  defaultPath <- Config.getDefaultConfigFilepath
  let configPath =
        if isVanilla
          then Nothing
          else case givenPath of
                 (Just f) -> Just (MT.pack f)
                 Nothing -> Just defaultPath
  -- load the config file
  Config.loadMorlocConfig configPath

-- | handle the code, either from a file or a raw string
readScript :: Arguments -> IO (Maybe Path, MT.Text)
readScript args
  | isPresent args (longOption "expression") = return (Nothing, script)
  | otherwise = do
    code <- MT.readFile (MT.unpack script)
    return (Just script, code)
  where
    script = getArgOrDie args (argument "script")

-- | install a module
cmdInstall :: Subcommand
cmdInstall args conf =
  (MM.runMorlocMonad conf cmdInstall') >>= MM.writeMorlocReturn
  where
    cmdInstall' = do
      let name = getArgOrDie args (argument "name")
      if isPresent args (longOption "github")
        then Mod.installModule (Mod.GithubRepo name)
        else Mod.installModule (Mod.CoreGithubRepo name)

-- | remove a previously installed module (NOT YET IMPLEMENTED)
cmdRemove :: Subcommand
cmdRemove _ _ = do
  putStrLn "not removing anything"

-- | build a Morloc program, generating the nexus and pool files
cmdMake :: Subcommand
cmdMake args config = do
  (path, code) <- readScript args
  MM.runMorlocMonad config (M.writeProgram path code) >>=
    MM.writeMorlocReturn

cmdTypecheck :: Subcommand
cmdTypecheck args config = do
  let expr = getArgOrDie args (argument "script")
  expr' <-
    if isPresent args (longOption "expression")
      then return expr
      else MT.readFile (MT.unpack expr)
  let base =
        if isPresent args (longOption "expression")
          then Nothing
          else Just expr
  let writer =
        if isPresent args (longOption "raw")
          then Papi.ugly
          else Papi.cute
  if isPresent args (longOption "type")
    then print $ P.readType expr'
    else MM.runMorlocMonad
           config
           (M.typecheck base expr' >>= MM.liftIO . writer) >>=
         MM.writeMorlocReturn
