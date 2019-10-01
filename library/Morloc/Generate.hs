{-|
Module      : Morloc.Generate
Description : Generate code from the RDF representation of a Morloc script 
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Generate (generate) where

import Morloc.Namespace
import qualified Morloc.Nexus.Nexus as MN
import qualified Morloc.Pools.Pools as MP
import Control.Monad.State (gets)

generate :: [Manifold] -> MorlocMonad (Script, [Script])
generate ms = do
  -- nexus :: Script
  -- generate the nexus script
  nexus <- MN.generate ms

  -- get serial maps from state (these were stored in `connect`)
  hss <- gets stateSerialMaps

  -- pools :: [Script]
  pools <- MP.generate ms hss

  -- (Script, [Script])
  return (nexus, pools)
