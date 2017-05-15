-- Morloc.hs

-- | A module for pure interpretation of morloc

module Morloc (interpret) where

import Morloc.Parser as Parser
import Morloc.Evaluator (eval)
import Morloc.Mode (Mode)
import Data.Either (either)

interpret :: Mode -> String -> Either String String
interpret mode code = either err res (eval code) where
  err = Left . unlines . lines . show
  res = Right . mode
