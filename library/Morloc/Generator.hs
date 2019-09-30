{-|
Module      : Morloc.Generator
Description : Generate code from the RDF representation of a Morloc script 
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Generator (generate) where

import Morloc.Types.API
import Morloc.Global
import Morloc.Operators
import qualified Morloc.Nexus.Nexus as MN
import qualified Morloc.Pools.Pools as MP
import qualified Morloc.Monad as MM

generate :: [Module] -> MorlocMonad (Script, [Script])
generate mods = do
  -- root :: Module -- fail if there is not a single "root" module, e.g., main
  root <- rootModule mods 
  -- ms :: [manifold]
  -- build all manifold paths starting from exported root declarations the
  -- state monad handles scope and module attributes as well as assignment of
  -- unique integer IDs to all manifolds.
  modelState <- initProgramState mods
  (ms, stat) <- MM.liftIO
             . (flip MM.runStateT) modelState
             $ fmap concat (mapM (module2manifolds root) (moduleBody root))

  -- nexus :: Script
  -- generate the nexus script
  nexus <- MN.generate ms
  -- hss :: Map.Map Lang SerialMap
  hss <- makeSerialMaps mods

  -- pools :: [Script]
  pools <- MP.generate ms hss

  -- (Script, [Script])
  return (nexus, pools)
