{-|
Module      : Morloc.Monad
Description : A great big stack of monads
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Monad
( 
    MorlocMonad
  , MorlocReturn
  , runMorlocMonad
  , writeMorlocReturn
  , runCommand
  , module Control.Monad.Trans 
  , module Control.Monad.Except 
  , module Control.Monad.Reader
  , module Control.Monad.State
  , module Control.Monad.Writer
) where

import Morloc.Types
import Morloc.Operators
import qualified Morloc.Data.Text as MT
import qualified Morloc.Error as ME

import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified System.Exit as SE
import qualified System.Process as SP
import System.IO (stderr)

type MorlocMonadGen c e l s a = ReaderT c (ExceptT e (WriterT l (StateT s IO))) a
type MorlocState = Map.Map MT.Text MT.Text
type MorlocMonad a = MorlocMonadGen Config MorlocError [MT.Text] MorlocState a
type MorlocReturn a = ((Either MorlocError a, [MT.Text]), MorlocState)

runMorlocMonad :: Config -> MorlocMonad a -> IO (MorlocReturn a)
runMorlocMonad config ev = runStateT (runWriterT(runExceptT(runReaderT ev config))) Map.empty

writeMorlocReturn :: MorlocReturn a -> IO ()
writeMorlocReturn ((Left err, msgs), _)
  =  MT.hPutStr stderr (MT.unlines msgs) -- write messages
  >> MT.hPutStr stderr (ME.errmsg err) -- write terminal failing message
writeMorlocReturn ((_, msgs), _) = MT.hPutStr stderr (MT.unlines msgs)

runCommand :: MT.Text -> MorlocMonad ()
runCommand cmd = do
  (_, _, herr, handle) <- liftIO $ SP.runInteractiveCommand (MT.unpack cmd)
  exitCode <- liftIO $ SP.waitForProcess handle
  err <- liftIO $ MT.hGetContents herr
  case exitCode of
    SE.ExitSuccess     -> tell [err] -- log a message
    (SE.ExitFailure _) -> throwError (SystemCallError err) |>> (\_ -> ()) -- raise an error
