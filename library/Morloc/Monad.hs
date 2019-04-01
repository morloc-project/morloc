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
  , runMorlocMonad
  , module Control.Monad.Trans 
  , module Control.Monad.Except 
  , module Control.Monad.Reader
  , module Control.Monad.State
  , module Control.Monad.Writer
) where

import Morloc.Types
import qualified Morloc.Data.Doc as MD
import qualified Morloc.Data.Text as MT

import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map

type MorlocMonadGen c e l s a = ReaderT c (ExceptT e (WriterT l (StateT s IO))) a
type MorlocState = Map.Map MT.Text MT.Text
type MorlocMonad a = MorlocMonadGen Config MorlocError MD.Doc MorlocState a

runMorlocMonad :: Config -> MorlocMonad a -> IO ((Either MorlocError a, MD.Doc), MorlocState)
runMorlocMonad config ev = runStateT (runWriterT(runExceptT(runReaderT ev config))) Map.empty
