{-|
Module      : Morloc.Types
Description : Miscellaneous
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Types ( 
    Script(..)
  , SparqlEndPoint  
  , Name
  , Lang
  , Path
  , Code
  , ScriptGenerator
  , CodeGenerator
) where

import qualified Data.Text as DT

type Name = DT.Text
type Lang = DT.Text
type Path = DT.Text
type Code = DT.Text

-- | Stores a URL for a SPARQL endpoint (e.g. "http://localhost:3030/morloc")
type SparqlEndPoint = String

-- | A code generator
type ScriptGenerator = SparqlEndPoint -> IO Script
type CodeGenerator = SparqlEndPoint -> IO Code

-- | Stores everything needed to build one file
data Script = Script {
      scriptBase :: String  -- ^ script basename (no extension)
    , scriptLang :: String  -- ^ script language
    , scriptCode :: DT.Text -- ^ full script source code
  }
  deriving(Ord, Eq)
