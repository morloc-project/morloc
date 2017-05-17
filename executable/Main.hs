module Main (main) where

import Morloc (interpret)
import qualified Morloc.Mode as Mode
import MorlocExecutable.Repl (repl)

-- mode = Mode.asLIL
mode = Mode.asCode

process :: Mode.Mode -> String -> String
process mode s = case interpret mode s of
  Left  err -> err
  Right res -> res

main :: IO ()
main = repl (process mode)
