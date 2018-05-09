-- Morloc.hs

-- | A module for pure interpretation of morloc

module Morloc (interpret) where

import Morloc.Graph (Graph)
import Morloc.Evaluator (eval)
import Morloc.Data
import Morloc.Core
import Data.Either (either)

interpret :: String -> Either String (Graph MData)
interpret code = either err res (eval code) where
  err = Left . unlines . lines . show
  res = Right
