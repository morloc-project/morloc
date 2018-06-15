module Morloc (interpret) where

import Morloc.Data (Program)
import Morloc.Evaluator (eval)
import Text.Parsec (SourceName, parse)

interpret :: String -> String
interpret s = case eval s of
    Left  err    -> show err    ++ "\n"
    Right result -> show result ++ "\n"
