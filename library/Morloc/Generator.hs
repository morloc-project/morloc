{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Generator
Description : Generate code from the RDF representation of a Morloc script 
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Generator
(
    Nexus
  , Pool
  , generate  
) where

import Morloc.Types
import Morloc.Config (Config)
import qualified Morloc.Nexus.Nexus as MN
import qualified Morloc.Pools.Pools as MP

type Nexus = Script
type Pool  = Script

-- | Given a SPARQL endpoint, generate an executable program
generate :: SparqlDatabaseLike db => Config -> db -> IO (Nexus, [Pool])
generate con e = (,) <$> MN.generate con "perl" e <*> MP.generate con e
