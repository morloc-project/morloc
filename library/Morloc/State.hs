module Morloc.State
(
    Parser
  , ParserState(..)
  , parserStateEmpty
  , nest
  , getId
  , getScope
  , setScope
  , setScope'
) where

import Text.Parsec hiding (Parser, State)

-- data ParsecT s u m a
-- where
--   s := stream type
--   u := user state type
--   m := underlying monad
--   a := return type
--
-- type Parsec s u = ParsecT s u Identity
type Parser = Parsec String ParserState

-- For now, the passed parser state is just an counter
data ParserState = ParserState {
    -- A program unique ID
    stateCount :: Int 
    -- The parent ID
  , stateScope :: Int 
}

parserStateEmpty :: ParserState
parserStateEmpty = ParserState {
    stateCount  = 0
  , stateScope  = 0
}

setScope :: Int -> Parser ()
setScope i = do
  modifyState (\s -> s {stateScope = i})

getScope :: Parser Int
getScope = do
  s <- getState
  return $ stateScope s

setScope' :: Parser ()
setScope' = do
  modifyState (\s -> s {stateScope = stateCount s})

-- parsers need to be wrapped in `nest` when they contain internal linked lists
-- that need to be terminated. `nest` returns to the original scope.
nest :: Parser a -> Parser a
nest p = do
  i <- getScope
  x <- p
  setScope i
  return x

getId :: Parser Int
getId = do
  modifyState (\s -> s {stateCount = (stateCount s) + 1})
  s <- getState
  return $ stateCount s
