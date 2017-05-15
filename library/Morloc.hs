module Morloc (interpret) where

import Morloc.Parser as Parser
import Morloc.Interpreter as Interpreter

interpret :: String -> IO ()
interpret line = do
  let res = Parser.parseTopLevel line
  case res of
    Left err -> print err
    Right ex -> case mapM Interpreter.eval ex of
      Left  err  -> print err
      Right tree -> mapM_ (putStr . Interpreter.toLIL) tree
