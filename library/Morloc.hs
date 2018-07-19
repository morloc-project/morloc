module Morloc (rdf) where

import Morloc.Error
import Morloc.Parser (morlocScript)
import Morloc.Triple (showRDF)

rdf :: String -> String
rdf s = case morlocScript s of
  Left err -> show err ++ "\n"
  Right result -> showRDF result
