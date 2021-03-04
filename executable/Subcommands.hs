{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Subcommands
Description : Morloc executable subcommands
Copyright   : (c) Zebulun Arendsee, 2021
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

import UI
import Morloc.Namespace
import qualified Morloc as M
import qualified Morloc.Config as Config
import qualified Morloc.Data.Text as MT
import qualified Morloc.Module as Mod
import qualified Morloc.Monad as MM
import qualified Morloc.Frontend.API as F

type Subcommand = CliCommand -> Config.Config -> IO ()

-- | read the global morloc config file or return a default one
getConfig :: CliCommand -> IO Config.Config
getConfig = undefined

getVerbosity :: CliCommand -> Int
getVerbosity = undefined

-- | handle the code, either from a file or a raw string
readScript :: CliCommand -> IO (Maybe Path, Code)
readScript = undefined

-- | install a module
cmdInstall :: Subcommand
cmdInstall = undefined

-- | remove a previously installed module (NOT YET IMPLEMENTED)
cmdRemove :: Subcommand
cmdRemove = undefined

-- | build a Morloc program, generating the nexus and pool files
cmdMake :: Subcommand
cmdMake = undefined

-- | run the typechecker on a module but do not build it
cmdTypecheck :: Subcommand
cmdTypecheck = undefined


-- import Morloc.Namespace
-- import qualified Morloc as M
-- import qualified Morloc.Config as Config
-- import qualified Morloc.Data.Text as MT
-- import qualified Morloc.Module as Mod
-- import qualified Morloc.Monad as MM
-- import qualified Morloc.Frontend.API as F
--
-- type Subcommand = Arguments -> Config.Config -> IO ()
--
-- getArgOrDie :: Arguments -> Option -> MT.Text
-- getArgOrDie args opt =
--   case getArg args opt of
--     Nothing -> error ("Invalid command: Expected option '" <> show opt)
--     (Just x) -> MT.pack x
--
-- -- | read the global morloc config file or return a default one
-- getConfig :: Arguments -> IO Config.Config
-- getConfig args = do
--   let givenPath = getArg args (longOption "config")
--   let isVanilla = isPresent args (longOption "vanilla")
--   defaultPath <- Config.getDefaultConfigFilepath
--   let configPath =
--         if isVanilla
--           then Nothing
--           else case givenPath of
--                  (Just f) -> Just . Path . MT.pack $ f
--                  Nothing -> Just defaultPath
--   -- load the config file
--   Config.loadMorlocConfig configPath
--
-- getVerbosity :: Arguments -> Int
-- getVerbosity args =
--   if isPresent args (longOption "verbose")
--     then 1
--     else 0
--
-- -- | handle the code, either from a file or a raw string
-- readScript :: Arguments -> IO (Maybe Path, Code)
-- readScript args = do
--   let input = getArgOrDie args (argument "script")
--   if isPresent args (longOption "expression")
--   then do
--     let code = Code input
--     return (Nothing, code)
--   else do
--     let filename = Path input
--     code <- fmap Code $ MT.readFile (MT.unpack input)
--     return (Just filename, code)
--
-- -- | install a module
-- cmdInstall :: Subcommand
-- cmdInstall args conf =
--   (MM.runMorlocMonad (getVerbosity args) conf cmdInstall') >>= MM.writeMorlocReturn
--   where
--     cmdInstall' = do
--       let name = getArgOrDie args (argument "name")
--       if isPresent args (longOption "github")
--         then Mod.installModule (Mod.GithubRepo name)
--         else Mod.installModule (Mod.CoreGithubRepo name)
--
-- -- | remove a previously installed module (NOT YET IMPLEMENTED)
-- cmdRemove :: Subcommand
-- cmdRemove _ _ = do
--   putStrLn "not removing anything"
--
-- -- | build a Morloc program, generating the nexus and pool files
-- cmdMake :: Subcommand
-- cmdMake args config = do
--   (path, code) <- readScript args
--   MM.runMorlocMonad (getVerbosity args) config (M.writeProgram path code) >>=
--     MM.writeMorlocReturn
--
-- cmdTypecheck :: Subcommand
-- cmdTypecheck args config = do
--   let expr = getArgOrDie args (argument "script")
--   expr' <-
--     if isPresent args (longOption "expression")
--       then return expr
--       else MT.readFile (MT.unpack expr)
--   let base =
--         if isPresent args (longOption "expression")
--           then Nothing
--           else Just (Path expr)
--   let writer =
--         if isPresent args (longOption "raw")
--           then F.ugly
--           else F.cute
--   if isPresent args (longOption "type")
--     then print $ F.readType expr'
--     else MM.runMorlocMonad
--            (getVerbosity args)
--            config
--            (M.typecheck base (Code expr') >>= MM.liftIO . writer) >>=
--          MM.writeMorlocReturn
