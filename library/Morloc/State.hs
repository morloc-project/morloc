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
  , getModulePath
) where

import qualified Morloc.Data.RDF as R
import qualified Morloc.Data.Text as MT

import Text.Megaparsec
import qualified Control.Monad.State as CMS
import Data.Void



-- | A stateful Parser stores an integer that is used to generate URIs
type Parser a = CMS.StateT ParserState (Parsec Void MT.Text) a

data ParserState = ParserState {
    -- | Stores the current node number. This will be unique within a program.
    stateCount :: Int 
  , stateSourceUri :: Maybe MT.Text
  , stateModulePath :: Maybe MT.Text
}

-- | The empty parser state, with the ID initialized to 0
parserStateEmpty :: ParserState
parserStateEmpty = ParserState {
    stateCount  = 0
  , stateSourceUri = Nothing
  , stateModulePath = Nothing
}

-- | Get an RDF URI and increment the internal counter
getId :: Parser R.Node
getId = do
  s <- CMS.get
  CMS.put (s {stateCount = (stateCount s) + 1})
  return $ R.idUri (stateSourceUri s) (stateCount s)

getSourceUri :: Parser MT.Text
getSourceUri = do
  s <- CMS.get
  return $ maybe "<stdin>" id (stateSourceUri s)

getModulePath :: Parser MT.Text
getModulePath = do
  s <- CMS.get
  return $ maybe "<stdin>" id (stateModulePath s)
