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
) where

import qualified Morloc.Error as ME
import qualified Morloc.Nexus as MN
import qualified Morloc.Pool as MP
import qualified Morloc.Util as MU
import Morloc.Operators

import qualified Morloc.Database.HSparql.Connection as DHC
import qualified Morloc.Query as Q

import qualified Data.RDF as DR
import qualified Data.Text as DT

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
generate :: DHC.SparqlEndPoint -> IO (Nexus, [Pool])
generate e = (,) <$> generateNexus e <*> generatePools e

-- | Generate the nexus, which is a program that coordinates the execution of
-- the language-specific function pools.
generateNexus :: DHC.SparqlEndPoint -> IO Nexus
generateNexus e
  =   Script
  <$> pure "nexus"
  <*> pure lang'
  <*> nexusCode'
  where
    -- TODO allow user to choose a generator
    -- The generator be stored in the endpoint SPARQL db 
    g = MN.perlCliNexusGenerator
    lang' = "perl"
    nexusCode' = case Q.exports e of
      exports' -> fmap DT.unlines $ sequence
        [ return (MN.nexusPrologue g)
        , return ((MN.nexusPrint g) "")
        , fmap (MN.nexusDispatch g) exports' 
        , nexusHelp e g
        , fmap DT.unlines (generateNexusCalls e g)
        , return (MN.nexusEpilogue g)
        ]

-- | Generate a help message for each exported function
nexusHelp :: DHC.SparqlEndPoint -> MN.NexusGenerator -> IO DT.Text
nexusHelp e g
  =   (MN.nexusHelp g)
  <$> pure prologue'
  <*> exports'
  <*> pure epilogue'
  where
    prologue' = ["The following commands are exported:"]
    exports' = fmap (map (\s -> "    " <> s)) (Q.exports e)
    epilogue' = []

-- | Generate functions that call specific functions in specific pools.
-- Here is an example for a Perl nexus:
-- @
--   if(scalar(@_) != 2 ){
--     print STDERR "Expected 2 arguments to `foo`, given X";
--     exit 1;
--   }
--   return `Rscript foo.R m2 3`
-- @
generateNexusCalls :: DHC.SparqlEndPoint -> MN.NexusGenerator -> IO [DT.Text]
generateNexusCalls e g = (fmap . map) (generateNexusCall g) (Q.forNexusCall e)

generateNexusCall
  :: MN.NexusGenerator
  -> ( DT.Text -- ^ function's Morloc name (not necessarily it's native name)
     , DT.Text -- ^ function's native language
     , DT.Text -- ^ Morloc ID for the type declaration for this function
     , DT.Text -- ^ number of arguments (according to the type signature)
     )
  -> DT.Text
generateNexusCall g (fname, flang, fid, nargs) = (MN.nexusCall g)
  ("Rscript")            -- command -- FIXME: generalize
  ("pool." <> flang)     -- pool filename -- FIXME: generalize/customize
  (fname)                -- function name
  (makeManifoldName fid) -- manifold name made form type URI
  (read (show nargs) :: Int) -- number of arguments

makeManifoldName :: DT.Text -> DT.Text
makeManifoldName x = case reverse (DT.splitOn "/" x) of
  (y:ys) -> "m" <> y
  _ -> error "Manifold uri does not match the pattern `.*/\\d+$`"

generatePools :: DHC.SparqlEndPoint -> IO [Pool]
generatePools = undefined
-- generatePools e = forGeneratePools e >>= (sequence . map (generatePool e))
