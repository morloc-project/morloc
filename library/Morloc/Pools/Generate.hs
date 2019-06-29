{-|
Module      : Generate
Description : Generate all pools breadth wise
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This is a draft of a possible replacement for the crappy current generator.
-}

module Morloc.Pools.Generate
  ( 
    generatePools
  ) where

import Morloc.Global

data Program = Program [Manifold] [SerialMap] 

data Function = Function {
  functionLang :: Lang 
  -- etc
  -- etc
  -- etc
}

generatePools :: (SparqlDatabaseLike db) => db -> [Manifold] -> MorlocMonad [Script]
generatePools db manifolds = do
  serialMaps <- getSerialMaps db
  mapM (generateFunction serialMaps) manifolds >>= makeScripts 

-- | get one serial map for each language
getSerialMaps :: (SparqlDatabaseLike db) => db -> MorlocMonad [SerialMap]
getSerialMaps = undefined

-- | get all manifolds
getManifolds :: (SparqlDatabaseLike db) => db -> MorlocMonad [Manifold]
getManifolds = undefined

-- | create a function from each manifold
generateFunction :: [SerialMap] -> Manifold -> MorlocMonad Function
generateFunction = undefined

-- | Gather functions into pools and call the language-specific generators
makeScripts :: [Function] -> MorlocMonad [Script]
makeScripts = undefined
