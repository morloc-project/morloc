{-|
Module      : Morloc.Types
Description : Miscellaneous
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Types ( 
    SparqlEndPoint  
  , Script(..)
) where

import qualified Data.Text as DT

-- | Stores a URL for a SPARQL endpoint (e.g. "http://localhost:3030/morloc")
type SparqlEndPoint = String

-- | Stores everything needed to build one file
data Script = Script {
      scriptBase :: String  -- ^ script basename (no extension)
    , scriptLang :: String  -- ^ script language
    , scriptCode :: DT.Text -- ^ full script source code
  }
  deriving(Ord, Eq)
