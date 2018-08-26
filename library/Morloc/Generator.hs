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
    Script(..)
  , Nexus
  , Pool
  , generate  
  , generatePools -- DO NOT EXPORT
) where

import qualified Morloc.Nexus as MN
import qualified Morloc.Language as ML
import Morloc.Types (SparqlEndPoint)
import Morloc.Operators

import qualified Morloc.Database.HSparql.Connection as DHC
import qualified Morloc.Query as Q

import qualified Data.Text as DT
import qualified Data.List.Extra as DLE
import qualified Data.Maybe as DM

-- | Stores everything needed to build one file
data Script = Script {
      scriptBase :: String  -- ^ script basename (no extension)
    , scriptLang :: String  -- ^ script language
    , scriptCode :: DT.Text -- ^ full script source code
  }
  deriving(Ord, Eq)

type Nexus = Script
type Pool = Script

-- | Given a SPARQL endpoint, generate an executable program
generate :: SparqlEndPoint -> IO (Nexus, [Pool])
generate e = (,) <$> generateNexus e <*> generatePools e

-- | Generate the nexus, which is a program that coordinates the execution of
-- the language-specific function pools.
generateNexus :: SparqlEndPoint -> IO Nexus
generateNexus e
  =   Script
  <$> pure "nexus"
  <*> pure "perl"
  <*> MN.perlNexus e

makeManifoldName :: DT.Text -> DT.Text
makeManifoldName x = case reverse (DT.splitOn "/" x) of
  (y:ys) -> "m" <> y
  _ -> error "Manifold uri does not match the pattern `.*/\\d+$`"

generatePools :: SparqlEndPoint -> IO [Pool]
generatePools e = fmap (map generatePool . makeDict) (Q.sourcesQ e)

makeDict :: Ord a => [[a]] -> [(a, [[a]])] 
makeDict = DLE.groupSort . map enkey where
  enkey :: [a] -> (a, [a])
  enkey (x:xs) = (x, xs)
  enkey _ = error "Bad RDF" 

-- SELECT ?fname ?alias ?path
generatePool :: (Maybe DT.Text, [[Maybe DT.Text]]) -> Pool
generatePool (Just "R", xss) = Script {
      scriptBase = "pool"
    , scriptLang = "R"
    , scriptCode = generatePoolCode g xss
  } where
    g = ML.rCodeGenerator

generatePoolCode :: ML.CodeGenerator -> [[Maybe DT.Text]] -> DT.Text 
generatePoolCode g xss =
  (ML.makePool g)
    --   any required global declarations
    [] 
    --   any input source code
    (DM.catMaybes $ map ((flip (!!)) 2) xss) 
    --   the node function declarations
    [] 
