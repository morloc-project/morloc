module Morloc (interpret) where

import Morloc.Parser as Parser
import Morloc.Interpreter (eval)
import Morloc.Mode (Mode)

pack :: Show a => a -> String
pack = unlines . lines . show

interpret :: Mode -> String -> Either String String
interpret mode morloc_code = do
  -- parseTopLevel :: String -> ThrowsError [Expr]
  let res = Parser.parseTopLevel morloc_code
  case res of
    Left err -> Left $ pack err
    -- eval :: Expr -> Error.ThrowsError (Graph NodeAttr)
    Right ex -> case traverse eval ex of
      Left  err  -> Left $ pack err
      -- mode :: Graph NodeAttr -> String 
      Right gs -> Right $ concatMap mode gs
