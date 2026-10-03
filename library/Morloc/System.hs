{- |
Module      : Morloc.System
Description : Filesystem re-exports and YAML config loading
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Re-exports "System.Directory", "System.Directory.Tree", and
"System.FilePath.Posix" so that other modules can import a single module
for all filesystem operations. Also provides 'loadYamlConfig' for loading
YAML configuration with defaults, 'shellQuote' for paths placed in shell
command lines, and 'openLockFile' for locks held while other programs run.
-}
module Morloc.System
  ( module System.Directory.Tree
  , module System.Directory
  , module System.FilePath.Posix
  , loadYamlConfig
  , shellQuote
  , openLockFile
  ) where

import Morloc.Namespace.Prim

import Data.Aeson (FromJSON (..))
import System.IO (Handle)
import System.Posix.IO (OpenFileFlags (..), OpenMode (ReadWrite), defaultFileFlags, fdToHandle, openFd)
import qualified Data.Yaml.Config as YC
import System.Directory
import System.Directory.Tree
import System.FilePath.Posix

loadYamlConfig ::
  (FromJSON a) =>
  -- | possible locations of the config file
  Maybe [String] ->
  -- | default values taken from the environment (or a hashmap)
  YC.EnvUsage ->
  -- | default configuration
  IO a ->
  IO a
loadYamlConfig (Just fs) e _ = YC.loadYamlSettings fs [] e
loadYamlConfig Nothing _ d = d

-- | POSIX single-quote a path so spaces and shell metacharacters survive a
-- shell command line. Embedded single quotes are escaped with the standard
-- @'\\''@ idiom.
shellQuote :: FilePath -> String
shellQuote p = "'" <> concatMap esc p <> "'"
  where
    esc '\'' = "'\\''"
    esc c = [c]

-- | Open a file to hold an advisory lock on. The descriptor is close-on-exec
-- from the moment it exists: programs started while the lock is held, by this
-- thread or any other, would otherwise inherit it, and one that outlives this
-- process (a daemon a setup script starts) would hold the lock after it exits,
-- leaving the next waiter blocked for good.
openLockFile :: FilePath -> IO Handle
openLockFile path =
  openFd path ReadWrite defaultFileFlags {creat = Just 0o644, cloexec = True} >>= fdToHandle
