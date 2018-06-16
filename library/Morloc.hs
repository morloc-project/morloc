module Morloc (ast, build) where

import Morloc.Data (Program)
import Morloc.Evaluator (eval)
import Morloc.EvalError
import Morloc.Generator (generate, Nexus, Pool)
import Text.Parsec (SourceName, parse)

build :: String -> ThrowsError (Nexus, [Pool]) 
build s = eval s >>= generate

-- get the abstract syntax tree (it isn't really an abstract syntax tree)
ast :: String -> String
ast s = case eval s of
    Left  err    -> show err    ++ "\n"
    Right result -> show result ++ "\n"
