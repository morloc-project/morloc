module Morloc (rdf) where

import Morloc.Evaluator (eval)
import Morloc.Check (typecheck)
import Morloc.Error
import Morloc.Generator (generate, Nexus, Pool)
import Morloc.Parser (morlocScript)
import Morloc.Triple (showRDF)

-- build :: String -> ThrowsError (Nexus, [Pool])
-- build s = eval s >>= typecheck >>= generate
--
-- -- get the abstract syntax tree (it isn't really an abstract syntax tree)
-- ast :: String -> String
-- ast s = case eval s of
--     Left  err    -> show err    ++ "\n"
--     Right result -> show result ++ "\n"

rdf :: String -> String
rdf s = case morlocScript s of
  Left err -> show err ++ "\n"
  Right result -> showRDF result
