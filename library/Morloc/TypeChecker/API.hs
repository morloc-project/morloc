{-|
Module      : Morloc.TypeChecker.API
Description : The primary API for the morloc type system
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.TypeChecker.API
  ( typecheck
  , runStack
  , Module(..)
  ) where

import Morloc.Namespace
import qualified Control.Monad.Except as ME
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.State as MS
import qualified Control.Monad.Writer as MW
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.TypeChecker.Infer as Infer

typecheck :: [Module] -> MorlocMonad [Module]
typecheck ms = do
  verbosity <- MS.gets stateVerbosity 
  x <- liftIO $ runStack verbosity (Infer.typecheck ms)
  case x of
    ((Right result, _), _) -> return result
    ((Left err, _), _) -> MM.throwError err

-- | currently I do nothing with the Reader and Writer monads, but I'm leaving
-- them in for now since I will need them when I plug this all into Morloc.
runStack :: Int -> Stack a -> IO ((Either MorlocError a, [MT.Text]), StackState)
runStack verbosity e
  = flip MS.runStateT emptyState
  . MW.runWriterT
  . ME.runExceptT
  . MR.runReaderT e
  $ StackConfig verbosity

emptyState = StackState
  { stateVar = 0
  , stateQul = 0
  , stateSer = []
  }
