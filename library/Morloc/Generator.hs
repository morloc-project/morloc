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

import qualified Database.HSparql.Connection as DHC
import qualified Morloc.Query as Q

import qualified Data.RDF as DR
import qualified Data.Text as DT

data Script = Script {
      scriptBase :: String  -- ^ script basename (no extension)
    , scriptLang :: String  -- ^ script language
    , scriptCode :: DT.Text -- ^ full script source code
  }
  deriving(Ord, Eq)

type Nexus = Script
type Pool = Script

generate :: DHC.EndPoint -> IO (Nexus, [Pool])
generate e = (,) <$> generateNexus e <*> generatePools e

generateNexus :: DHC.EndPoint -> IO Nexus
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
        , exports' >>= (\xs ->
               fmap DT.unlines
            .  sequence
            .  map (generateNexusCall e g)
            $  xs
          )
        , return (MN.nexusEpilogue g)
        ]

nexusHelp :: DHC.EndPoint -> MN.NexusGenerator -> IO DT.Text
nexusHelp e g
  =   (MN.nexusHelp g)
  <$> pure prologue'
  <*> exports'
  <*> pure epilogue'
  where
    prologue' = ["The following commands are exported:"]
    exports' = fmap (map (\s -> "    " <> s)) (Q.exports e)
    epilogue' = []

generateNexusCall :: DHC.EndPoint -> MN.NexusGenerator -> DT.Text -> IO DT.Text
generateNexusCall _ _ _ = return ""

generatePools :: DHC.EndPoint -> IO [Pool]
generatePools e = return []
