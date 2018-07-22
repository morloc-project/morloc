module Morloc (rdf, make) where

import Morloc.Error
import Morloc.Parser (morlocScript)
import Morloc.Tree (rdf2tree)
import Morloc.Evaluator (tree2program)
import Morloc.Processor (process)
import Morloc.Generator (generate, Nexus, Pool)
import Morloc.Builder (build)
import Morloc.Triple (showRDF)
import Morloc.Data

writeProgram :: ThrowsError (Script, [Script]) -> IO ()
writeProgram (Right x) = build x
writeProgram (Left err) = putStr (show err)

rdf :: String -> String
rdf s = case morlocScript s of
  Left err -> show err ++ "\n"
  Right result -> showRDF result

make :: String -> IO ()
make s = writeProgram $
      return s
  >>= morlocScript  -- parse the script
  >>= rdf2tree      -- convert the RDF into a tree
  >>= tree2program  -- extract data structures
  >>= process       -- typecheck
  >>= generate      -- generate the nexus and pool code
