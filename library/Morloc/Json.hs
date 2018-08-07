{-|
Module      : Morloc.Json
Description : Translate Morloc data to JSON
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Json (
    mdata2aeson
  , aeson2mdata
) where

import qualified Data.Aeson as DA
import qualified Data.RDF as DR

mdata2aeson :: DR.Rdf a => DR.RDF a -> DA.Value
mdata2aeson = undefined

aeson2mdata :: DR.Rdf a => DA.Value -> DR.RDF a
aeson2mdata = undefined
