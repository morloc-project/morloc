{- |
Module      : Morloc.CodeGenerator.Grammars.Translator.Syntax
Description : Generic translator framework for code generation
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Provides shared arg handlers and the IndexM type alias used by translators.
-}
module Morloc.CodeGenerator.Grammars.Translator.Syntax
  ( IndexM
  , genericMakeSerialArg
  , genericMakeNativeArg
  ) where

import Control.Monad.Identity (Identity)
import qualified Control.Monad.State as CMS
import Morloc.CodeGenerator.Grammars.Common (PoolDocs)
import Morloc.CodeGenerator.Namespace
import Morloc.Monad (IndexState)

-- Shared across ALL languages (no parameterization needed)
genericMakeSerialArg :: (Monad m) => SerialArg -> SerialArg_ PoolDocs PoolDocs -> m (TypeS, PoolDocs)
genericMakeSerialArg sr (SerialArgManifold_ x) = return (typeSof sr, x)
genericMakeSerialArg sr (SerialArgExpr_ x) = return (typeSof sr, x)

genericMakeNativeArg :: (Monad m) => NativeArg -> NativeArg_ PoolDocs PoolDocs -> m (TypeM, PoolDocs)
genericMakeNativeArg nr (NativeArgManifold_ x) = return (typeMof nr, x)
genericMakeNativeArg nr (NativeArgExpr_ x) = return (typeMof nr, x)

type IndexM = CMS.StateT IndexState Identity
