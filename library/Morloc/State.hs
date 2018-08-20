{-# LANGUAGE OverloadedStrings #-}

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
  , getSourceUri
) where

import Text.Megaparsec
import qualified Data.RDF as DR
import qualified Control.Monad.State as CMS
import Data.Void
import qualified Data.Text as DT

import qualified Morloc.Triple as M3



-- | A stateful Parser stores an integer that is used to generate URIs
type Parser a = CMS.StateT ParserState (Parsec Void DT.Text) a

data ParserState = ParserState {
    -- | Stores the current node number. This will be unique within a program.
    stateCount :: Int 
  , stateSourceUri :: Maybe DT.Text
}

-- | The empty parser state, with the ID initialized to 0
parserStateEmpty :: ParserState
parserStateEmpty = ParserState {
    stateCount  = 0
  , stateSourceUri = Nothing
}

-- | Get an RDF URI and increment the internal counter
getId :: Parser DR.Node
getId = do
  s <- CMS.get
  CMS.put (s {stateCount = (stateCount s) + 1})
  return $ M3.idUri (stateSourceUri s) (stateCount s)

getSourceUri :: Parser DT.Text
getSourceUri = do
  s <- CMS.get
  return $ maybe "<stdin>" id (stateSourceUri s) 
