{-|
Module      : Morloc.Monad
Description : A great big stack of monads
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

MorlocMonad is a monad stack that is passed throughout the morloc codebase.
Most functions that raise errors, perform IO, or access global configuration
will return `MorlocMonad a` types. The stack consists of a State, Writer,
Except, and Reader monad.
-}
module Morloc.Monad
  ( MorlocReturn
  , runMorlocMonad
  , evalMorlocMonad
  , writeMorlocReturn
  , runCommand
  , runCommandWith
  , logFile
  , logFileWith
  , readLang
  , say
  -- * reusable counter
  , startCounter
  , getCounter
  -- * re-exports
  , module Control.Monad.Trans
  , module Control.Monad.Except
  , module Control.Monad.Reader
  , module Control.Monad.State
  , module Control.Monad.Writer
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import Morloc.Error () -- for MorlocError Show instance
import Morloc.Namespace
import Morloc.Data.Doc
import System.IO (stderr)
import qualified Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified System.Directory as SD
import qualified System.Exit as SE
import qualified System.Process as SP

runMorlocMonad ::
     Int -> Config -> MorlocMonad a -> IO (MorlocReturn a)
runMorlocMonad v config ev =
  runStateT (runWriterT (runExceptT (runReaderT ev config))) (emptyState v)

-- | Evaluate a morloc monad
evalMorlocMonad ::
     Int
  -> Config -- ^ use default config object if Nothing
  -> MorlocMonad a
  -> IO a
evalMorlocMonad v config m = do
  ((x, _), _) <- runMorlocMonad v config m
  case x of
    (Left err) -> error (show err)
    (Right value) -> return value 

emptyState :: Int -> MorlocState
emptyState v = MorlocState {
    statePackageMeta = []
  , stateVerbosity = v
  , stateCounter = -1
}

startCounter :: MorlocMonad ()
startCounter = do
  s <- get
  put $ s {stateCounter = 0}

getCounter :: MorlocMonad Int
getCounter = do
  s <- get
  let i = stateCounter s
  put $ s {stateCounter = (stateCounter s) + 1}
  return i

writeMorlocReturn :: MorlocReturn a -> IO ()
writeMorlocReturn ((Left err, msgs), _)
  =  MT.hPutStrLn stderr (MT.unlines msgs) -- write messages
  >> MT.hPutStrLn stderr (MT.show' err) -- write terminal failing message
writeMorlocReturn ((_, msgs), _) = MT.hPutStrLn stderr (MT.unlines msgs)

-- | Execute a system call
runCommand ::
     MT.Text -- function making the call (used only in debugging messages on error)
  -> MT.Text -- system command
  -> MorlocMonad ()
runCommand loc cmd = do
  liftIO . MT.putStrLn $ "$ " <> cmd
  (exitCode, _, err) <-
    liftIO $ SP.readCreateProcessWithExitCode (SP.shell . MT.unpack $ cmd) []
  case exitCode of
    SE.ExitSuccess -> tell [MT.pack err]
    _ -> throwError (SystemCallError cmd loc (MT.pack err)) |>> (\_ -> ())

say :: MDoc -> MorlocMonad ()
say d = liftIO . putDoc $ " : " <> d <> "\n"

-- | Execute a system call and return a function of the STDOUT
runCommandWith ::
     MT.Text -- function making the call (used only in debugging messages on error)
  -> (MT.Text -> a) -- ^ A function of the output (run on success)
  -> MT.Text -- ^ System command
  -> MorlocMonad a
runCommandWith loc f cmd = do
  liftIO . MT.putStrLn $ "$ " <> cmd
  (exitCode, out, err) <-
    liftIO $ SP.readCreateProcessWithExitCode (SP.shell . MT.unpack $ cmd) []
  case exitCode of
    SE.ExitSuccess -> return $ f (MT.pack out)
    _ -> throwError (SystemCallError cmd loc (MT.pack err))

-- | Write a object to a file in the Morloc temporary directory
logFile ::
     Show a
  => String -- ^ A filename
  -> a
  -> MorlocMonad a
logFile s m = do
  tmpdir <- asks configTmpDir
  liftIO $ SD.createDirectoryIfMissing True (MT.unpack . unPath $ tmpdir)
  let path = (MT.unpack . unPath $ tmpdir) <> "/" <> s
  liftIO $ MT.writeFile path (MT.pretty m)
  return m

-- | Write a object to a file in the Morloc temporary directory
logFileWith ::
     (Show b)
  => String -- ^ A filename
  -> (a -> b) -- ^ A function to convert a to something presentable
  -> a
  -> MorlocMonad a
logFileWith s f m = do
  tmpdir <- asks configTmpDir
  liftIO $ SD.createDirectoryIfMissing True (MT.unpack . unPath $ tmpdir)
  let path = (MT.unpack . unPath $ tmpdir) <> "/" <> s
  liftIO $ MT.writeFile path (MT.pretty (f m))
  return m

-- | Attempt to read a language name. This is a wrapper around the
-- @Morloc.Language::readLangName@ that appropriately handles error.
readLang :: MT.Text -> MorlocMonad Lang
readLang langStr =
  case ML.readLangName langStr of
    (Just x) -> return x
    Nothing -> throwError $ UnknownLanguage langStr
