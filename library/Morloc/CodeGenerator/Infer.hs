{-|
Module      : Morloc.CodeGenerator.Infer
Description : Infer concrete types
Copyright   : (c) Zebulun Arendsee, 2023
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Infer (inferConcreteTypes) where

import Morloc.CodeGenerator.Namespace

inferConcreteTypes
  :: SAnno (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (SAnno Int One (Indexed TypeP))
inferConcreteTypes = undefined
