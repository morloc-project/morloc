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
  , Module(..)
  ) where

import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import Morloc.Namespace
import qualified Morloc.TypeChecker.Infer as XI
import Morloc.TypeChecker.Namespace

typecheck :: [Module] -> MorlocMonad [Module]
typecheck ms =
  case runStack (XI.typecheck ms) of
    (Right result, _) -> return result
    (Left err, _) -> MM.throwError . TypeError $ MT.show' err
