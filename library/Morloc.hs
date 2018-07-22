module Morloc (rdf) where

import Morloc.Error
import Morloc.Parser (morlocScript)
import Morloc.Tree (rdf2tree)
import Morloc.Evaluator (tree2program)
import Morloc.Processor (process)
import Morloc.Generator (generate)
import Morloc.Builder (build)
import Morloc.Triple (showRDF)

rdf :: String -> String
rdf s = case morlocScript s of
  Left err -> show err ++ "\n"
  Right result -> showRDF result

compile :: String -> ThrowsError (IO String)
compile s =
      return s
  >>= morlocScript  -- parse the script
  >>= rdf2tree      -- convert the RDF into a tree
  >>= tree2program  -- extract data structures
  >>= process       -- typecheck
  >>= generate      -- generate the nexus and pool code
  >>= build         -- write the files
