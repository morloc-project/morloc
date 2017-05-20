-- Morloc.hs

-- | A module for pure interpretation of morloc

module Morloc (interpret) where

import Morloc.Parser as Parser
import Morloc.Graph (Graph)
import Morloc.NodeAttribute (NodeAttr)
import Morloc.Evaluator (eval)
import Data.Either (either)

interpret :: String -> Either String (Graph NodeAttr)
interpret code = either err res (eval code) where
  err = Left . unlines . lines . show
  res = Right
