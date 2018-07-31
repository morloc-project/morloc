{-|
Module      : Morloc.State
Description : Parsec state
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.State
(
    Parser
  , ParserState(..)
  , parserStateEmpty
  , getId
) where

import Text.Parsec hiding (State)
import qualified Data.RDF as DR
import qualified Morloc.Triple as M3

-- | A stateful Parser stores an integerthat is used to generate URIs
type Parser = Parsec String ParserState

data ParserState = ParserState {
    -- | Stores the current node number. This will be unique within a program.
    stateCount :: Int 
}

-- | The empty parser state, with the ID initialized to 0
parserStateEmpty :: ParserState
parserStateEmpty = ParserState {
    stateCount  = 0
}

-- | Get an RDF URI and increment the internal counter
getId :: Parser DR.Node
getId = do
  s <- getState
  modifyState (\s' -> s' {stateCount = (stateCount s') + 1})
  return $ M3.idUri (stateCount s)
