{-|
Module      : Morloc.Generator
Description : Generate code from the RDF representation of a Morloc script 
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Generator (generate) where

import Xi
import Morloc.Global
import qualified Morloc.Language as ML
import qualified Morloc.Nexus.Nexus as MN
import qualified Morloc.Pools.Pools as MP

generate :: [Module] -> MorlocMonad (Script, [Script])
generate = undefined

-- -- | Given a SPARQL endpoint, generate an executable program
-- generate :: SparqlDatabaseLike db => db -> MorlocMonad (Script, [Script])
-- generate db = do
--   manifolds <- fromSparqlDb db
--   nexus <- MN.generate ML.PerlLang manifolds
--   pools <- MP.generate db manifolds
--   return (nexus, pools)
