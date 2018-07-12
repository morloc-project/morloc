module Morloc (ast, build, rdf) where

import Morloc.Evaluator (eval)
import Morloc.Check (typecheck)
import Morloc.Error
import Morloc.Generator (generate, Nexus, Pool)
import Morloc.Parser (morlocRDF)

build :: String -> ThrowsError (Nexus, [Pool]) 
build s = eval s >>= typecheck >>= generate

-- get the abstract syntax tree (it isn't really an abstract syntax tree)
ast :: String -> String
ast s = case eval s of
    Left  err    -> show err    ++ "\n"
    Right result -> show result ++ "\n"

rdf :: String -> String
rdf s = case morlocRDF s of
  Left err -> show err ++ "\n"
  Right result -> show result ++ "\n"
