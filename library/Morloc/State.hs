module Morloc.State
(
    ParserState(..)
  , parserStateEmpty
  , Parser
  , T
  , withCount
  , pushTriple
) where

import Text.Parsec hiding (Parser, State)

import Morloc.Triple

-- For now, the passed parser state is just an counter
data ParserState = ParserState {
    stateCount :: Int
  , stateScope :: Int
  , stateTriple :: [Triple]
}

parserStateEmpty :: ParserState
parserStateEmpty = ParserState {
    stateCount  = 0
  , stateScope  = 0
  , stateTriple = []
}

-- A numbered token
type T a = (Int, a)

-- data ParsecT s u m a
-- where
--   s := stream type
--   u := user state type
--   m := underlying monad
--   a := return type
--
-- type Parsec s u = ParsecT s u Identity
-- type Parser = Parsec String ()
-- 
-- Here, MorlocParser deviates from Parser (defined in Text.Parsec.String) by
-- passing Integer state.
type Parser = Parsec String ParserState

withCount :: Parser a -> Parser (T a)
withCount p = do 
  x <- p
  modifyState (\s -> s {stateCount = (stateCount s) + 1})
  s <- getState
  return (stateCount s, x)

pushTriple :: Subject -> RelObj -> Parser ()
pushTriple i r = do
  modifyState (\s -> s {stateTriple = (i, r):(stateTriple s)})
