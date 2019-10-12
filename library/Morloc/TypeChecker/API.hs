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
import qualified Control.Monad.Identity as MI
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.State as MS
import qualified Control.Monad.Writer as MW
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.TypeChecker.Infer as Infer

typecheck :: [Module] -> MorlocMonad [Module]
typecheck ms =
  case runStack (Infer.typecheck ms) of
    (Right result, _) -> return result
    (Left err, _) -> MM.throwError err

-- | currently I do nothing with the Reader and Writer monads, but I'm leaving
-- them in for now since I will need them when I plug this all into Morloc.
runStack :: Stack a -> (Either MorlocError a, [MT.Text])
runStack e =
  fst .
  MI.runIdentity .
  flip MS.runStateT emptyState . MW.runWriterT . ME.runExceptT . MR.runReaderT e $
  StackConfig 0

emptyState = StackState 0 0 []
