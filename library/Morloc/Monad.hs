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
    MorlocReturn
  , runMorlocMonad
  , writeMorlocReturn
  , runCommand
  , runCommandWith
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

runMorlocMonad :: Config -> Maybe SparqlEndPoint -> MorlocMonad a -> IO (MorlocReturn a)
runMorlocMonad config db ev = runStateT (runWriterT(runExceptT(runReaderT ev config))) (MorlocState db)

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

runCommandWith :: (MT.Text -> a) -> MT.Text -> MorlocMonad a
runCommandWith f cmd = do
    (_, hout, herr, handle) <- liftIO $ SP.runInteractiveCommand (MT.unpack cmd)
    exitCode <- liftIO $ SP.waitForProcess handle
    out <- liftIO $ MT.hGetContents hout
    err <- liftIO $ MT.hGetContents herr
    case exitCode of
      SE.ExitSuccess -> return $ f out
      _ -> throwError (SystemCallError err)
