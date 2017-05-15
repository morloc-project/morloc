module Morloc (interpret) where

import Morloc.Parser as Parser
import Morloc.Interpreter as Interpreter

pack :: Show a => a -> String
pack = unlines . lines . show

interpret :: String -> Either String String
interpret morloc_code = do
  -- parseTopLevel :: String -> ThrowsError [Expr]
  let res = Parser.parseTopLevel morloc_code
  case res of
    Left err -> Left $ pack err
    -- eval :: Expr -> Error.ThrowsError (Graph NodeAttr)
    Right ex -> case traverse Interpreter.eval ex of
      Left  err  -> Left $ pack err
      -- toLIL :: Graph NodeAttr -> String 
      Right gs -> Right $ concat $ map Interpreter.toLIL gs
