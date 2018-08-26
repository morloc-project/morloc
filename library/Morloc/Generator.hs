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

import qualified Morloc.Nexus as MN
import qualified Morloc.Pools as MP
import Morloc.Types (SparqlEndPoint, Script)

type Nexus = Script
type Pool  = Script

-- | Given a SPARQL endpoint, generate an executable program
generate :: SparqlEndPoint -> IO (Nexus, [Pool])
generate e = (,) <$> MN.generate e <*> MP.generate e
